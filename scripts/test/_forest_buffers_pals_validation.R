library(tidyverse)
library(ggplot2)
library(MetBrewer)
library(data.table)
library(forcats)
library(gghighlight)
library(ggcharts)
library(exactextractr)

# load the data

# need to write the script that saves the nf_level_dominant_archetypes_uncertainty_nepa_2025-01-30.csv
df <- read_csv(here::here("outputs/nfbuffers_div_ent_nf_2025-08-28.csv")) 
# need to write the script that saves the nf_level_pals_year_arch_summs_2025-01-30.csv
# Load the previously made NEPA sums
nepa_summ_df <- read_csv(here::here("outputs/tables/nf_nepa_projs_arch_summ_2025-09-11.csv"))
df_year <- read_csv(here::here("outputs/tables/nf_level_pals_year_arch_summs_2025-04-24.csv"))
pals_df <- read_delim("~/Analysis/NEPA_Efficiency/data/original/pals_ongoing_projects_11-2022.csv", delim = ";")
sgfcm_all_result <- rast("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/outputs/nfbuffers_SGFCM_all_result_k3_2025-08-12.tif") 

df_year <- df_year %>%
  dplyr::select(1:12)

nepa_summ_df <- nepa_summ_df %>%
  dplyr::select(1:3, 17:33)

# Need to calculate the dominant archetype for each forest

# read in the 50km buffer shape
# something is wrong with the buffers - they aren't cropped to conus
# get the intersection with the regional boundaries to "crop" to conus
# I was getting a bunch of NA for archetypes from water 
nf_buffers <- read_sf(here::here("data/processed/nf_buffers_50k_2024-10-22.shp"))

# crop the buffers to the outline of conus
reg_test <- fs_reg.crop %>%
  filter(REGION == "06")

# select a forest to check
nf_buff_test <- nf_buffers %>%
  filter(FORESTORGC == "0612")

# get the intersection with the region
test_int <- st_intersection(nf_buff_test, reg_test)
plot(test_int$geometry)

# create a new nation forest buffers shape
nf_buffers_int <- st_intersection(nf_buffers, fs_reg.crop)

# calculate the archetype areas assuming crisp archetypes for each forest 
buff <- nf_buffers_int

v <- buff %>% st_cast("MULTIPOLYGON")
z <- crop(sgfcm_all_result, buff, mask = TRUE)

x <- exact_extract(z, v, coverage_area = TRUE)
names(x) <- v$FORESTORGC

areas_nf <- bind_rows(x, .id = "FORESTORGC") %>%
  group_by(FORESTORGC, value) %>%
  summarize(total_arch_area = sum(coverage_area)) %>%
  group_by(FORESTORGC) %>%
  mutate(proportion_pct = round((total_arch_area/sum(total_arch_area))*100, 2)) %>%
  mutate(proportion = (total_arch_area/sum(total_arch_area)))

areas_nf <- areas_nf %>% 
  replace_na(list(value = 0))

nf_arch_areas_for_table <- areas_nf %>%
  dplyr::select(FORESTORGC, value, proportion_pct) %>%
  pivot_wider(names_from = value, values_from = proportion_pct)

areas <- areas_nf %>%
  group_by(FORESTORGC) %>%
  mutate(max_pct = max(proportion_pct)) %>%
  ungroup()

dom_arch_test <- areas %>%
  filter(max_pct == proportion_pct)

pals_df_test <- pals_df %>%
  filter(as.Date(`INITIATION DATE`, format = "%m/%d/%Y") >= "2009-01-01") %>%
  filter(REGION_ID != "10" & REGION_ID != "13" & REGION_ID != "24" & REGION_ID != "00") %>%
  dplyr::select(`SIGNED FY`, FOREST_ID, REGION_ID, `ELAPSED DAYS`, `DECISION TYPE`, 
                `FC Facility management – purpose`, 
                `FR Research – purpose`, `HF Fuels management – purpose`, `HR Heritage resource management – purpose`,
                `LM Land ownership management – purpose`, `LW Land acquisition – purpose`,
                `MG Minerals and geology – purpose`, `PN Land management planning – purpose`,
                `RD Road management – purpose`, `RG Grazing management – purpose`, `RO Regulations, directives, orders – purpose`,
                `RU Special area management – purpose`, `RW Recreation management – purpose`,
                `SU Special use management – purpose`, `TM Forest products – purpose`, 
                `VM Vegetation management (non-forest products) – purpose`,
                `WF Wildlife, fish, rare plants – purpose`, `WM Water management – purpose`) %>%
  group_by(`SIGNED FY`, FOREST_ID, `DECISION TYPE`) %>%
  summarise(REGION = mean(as.numeric(REGION_ID)),
            mean_assess_time = mean(`ELAPSED DAYS`),
            med_assess_time = median(`ELAPSED DAYS`),
            p_facilities = sum(`FC Facility management – purpose`), 
            p_research = sum(`FR Research – purpose`),
            p_haz_fuels = sum(`HF Fuels management – purpose`),
            p_heritage = sum(`HR Heritage resource management – purpose`),
            p_land_own = sum(`LM Land ownership management – purpose`), 
            p_land_acqui = sum(`LW Land acquisition – purpose`), 
            p_min_geo = sum(`MG Minerals and geology – purpose`), 
            p_land_mngt_plan = sum(`PN Land management planning – purpose`),
            p_road = sum(`RD Road management – purpose`), 
            p_grazing = sum(`RG Grazing management – purpose`),
            p_regulations = sum(`RO Regulations, directives, orders – purpose`),
            p_spec_area = sum(`RU Special area management – purpose`), 
            p_recreation = sum(`RW Recreation management – purpose`), 
            p_spec_use = sum(`SU Special use management – purpose`),
            p_forest_prod = sum(`TM Forest products – purpose`),
            p_veg_mngt = sum(`VM Vegetation management (non-forest products) – purpose`), 
            p_wildlife = sum(`WF Wildlife, fish, rare plants – purpose`), 
            p_water = sum(`WM Water management – purpose`))

df <- left_join(df, dom_arch_test, by = "FORESTORGC")

nf_pals_df <- left_join(df, pals_df_test, by = c("FORESTORGC" = "FOREST_ID"))

nf_pals_df <- nf_pals_df %>%
  rename(dom_archetype = value)

a1 <-  nf_pals_df %>%
  filter(dom_archetype == 1) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(archetype = "A", 
         pct_purpose = values/sum(values) * 100)
# strip the p_ off of the names?

a2 <-  nf_pals_df %>%
  filter(dom_archetype == 2) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(archetype = "B", 
         pct_purpose = values/sum(values) * 100)

a3 <-  nf_pals_df %>%
  filter(dom_archetype == 3) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(archetype = "C", 
         pct_purpose = values/sum(values) * 100)

archs <- rbind(a1, a2, a3)

# need to filter out purposes where all archetypes are below 3%
## probably a much easier way to do this, but I got it to work.
dt_archs <- as.data.table(archs)

low_p_A <- dt_archs[pct_purpose < 3 & archetype == "A"]
low_p_B <- dt_archs[pct_purpose < 3 & archetype == "B"]
low_p_C <- dt_archs[pct_purpose < 3 & archetype == "C"]

test_join <- left_join(low_p_A, low_p_B, by = "purpose") %>%
  left_join(., low_p_C, by='purpose') %>%
  drop_na()

for (p in test_join$purpose){
  archs_filt <- archs %>% 
    filter(purpose != p)
}

archs_filt <- archs %>%
  filter(purpose != test_join$purpose[1]) %>%
  filter(., purpose != test_join$purpose[2]) %>%
  filter(., purpose != test_join$purpose[3])

# rename the purposes

archs_filt <- archs_filt %>%
  mutate(purpose_newname = case_when(purpose == "p_facilities" ~ "Facilities", 
                                     purpose == "p_forest_prod" ~ "Forest Products",
                                     purpose == "p_grazing" ~ "Grazing",
                                     purpose == "p_haz_fuels" ~ "Hazardous. Fuels",
                                     purpose == "p_min_geo" ~ "Mining & Geology", 
                                     purpose == "p_recreation" ~ "Recreation",
                                     purpose == "p_road" ~ "Roads",
                                     purpose == "p_spec_use" ~ "Special Use Permits",
                                     purpose == "p_veg_mngt" ~ "Vegetation Management",
                                     purpose == "p_water" ~ "Water",
                                     purpose == "p_wildlife" ~ "Wildlife"))
# plot
archs_purpose_plot <- ggplot(archs_filt, aes(x=purpose_newname, y = pct_purpose, fill = archetype)) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge(), color = "black") +
  scale_fill_discrete(limits=c("A", "B", "C")) + 
  scale_fill_met_d("Hokusai3") +
  ylim(0, 40) +
  xlab("Purpose") + 
  ylab("% Projects with Purpose") +
  guides(fill=guide_legend(title="Archetype")) +
  theme_minimal() + 
  theme_bw() +
  theme(text = element_text(size = 8),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.text = element_text(size = 8)
  )
archs_purpose_plot

# Create some box plots and look at project purposes through time

test_pct_year_nf <- pals_df_test %>%
  group_by(FOREST_ID, `SIGNED FY`) %>%
  summarise(tot_spec_use = sum(p_spec_use),
            tot_for_prod = sum(p_forest_prod),
            tot_rec = sum(p_recreation),
            tot_haz_fuel = sum(p_haz_fuels),
            tot_wlife = sum(p_wildlife),
            tot_geo = sum(p_min_geo),
            tot_veg_mngt = sum(p_veg_mngt),
            tot_water = sum(p_water),
            tot_all_proj = sum(c_across(starts_with("p_")))) %>%
  mutate(pct_spec_use = (tot_spec_use / tot_all_proj)*100,
         pct_for_prod = (tot_for_prod / tot_all_proj)*100,
         pct_rec = (tot_rec / tot_all_proj)*100, 
         pct_haz_fuel = (tot_haz_fuel / tot_all_proj)*100,
         pct_wlife = (tot_wlife / tot_all_proj)*100,
         pct_geo = (tot_geo / tot_all_proj)*100,
         pct_water = (tot_water / tot_all_proj)*100,
         pct_veg_mngt = (tot_veg_mngt / tot_all_proj)*100) %>%
  drop_na()


test_join <- right_join(test_pct_year_nf, nepa_summ_df, by = c("FOREST_ID" = "forest_num"))
test_join <- right_join(test_join, df, by = c("FOREST_ID" = "FORESTORGC"))
test_join <- test_join %>%
  rename(dom_archetype = value)

test_join_long <- test_join %>%
  dplyr::select(`SIGNED FY`,FOREST_ID, pct_spec_use, pct_for_prod, pct_rec, 
                pct_haz_fuel, pct_wlife, pct_geo, pct_veg_mngt, pct_water, dom_archetype) %>%
  pivot_longer(!c(`SIGNED FY`, FOREST_ID, dom_archetype), names_to = "projects", values_to = "values")

test_join_long %>% 
  group_by(`SIGNED FY`, dom_archetype, projects) %>%
  summarize(values = mean(values)) %>%
  ggplot(aes(x = `SIGNED FY`, y = values, color = projects)) + 
  geom_line() +
  facet_wrap(~dom_archetype)

test_join %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(dom_archetype), pct_spec_use), notch = TRUE) + 
  ylim(0, 100)

test_join %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(dom_archetype), pct_for_prod), notch = TRUE) + 
  ylim(0, 100)

test_join %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(dom_archetype), pct_rec), notch = TRUE) + 
  ylim(0, 100)

test_join %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(dom_archetype), pct_haz_fuel), notch = TRUE) + 
  ylim(0, 100)

test_join %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(dom_archetype), pct_wlife), notch = TRUE) + 
  ylim(0, 100)

test_join %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(dom_archetype), pct_geo), notch = TRUE) + 
  ylim(0, 100)

test_join %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(dom_archetype), pct_water), notch = TRUE) + 
  ylim(0, 100)

# Look at special uses by region
#----------------------------------------

sup_df <- pals_df %>%
  filter(as.Date(`INITIATION DATE`, format = "%m/%d/%Y") >= "2009-01-01") %>%
  filter(`SU Special use management – purpose` == 1)

sup_df_join <- right_join(sup_df, df, by = c("FOREST_ID" = "FORESTORGC"))

arch_list <- c(1:3)

arch_sup_df <- data.frame(REGION_ID = character(), 
                          `PROJECT NAME` = character(),
                          FOREST_ID = numeric(),
                          forest_name = character(),
                          `DECISION SIGNED` = character(),
                          dom_archetype = numeric())
for (i in arch_list) {
  # Generate some data in each iteration
  new_row <- sup_df_join %>%
    dplyr::select(REGION_ID, `PROJECT NAME`, FOREST_ID, `LMU (ACTUAL)`, `DECISION SIGNED`, value) %>%
    filter(value == i) %>%
    arrange(desc(as.Date(`DECISION SIGNED`, tryFormats = c("%m/%d/%Y")))) %>%
    head(., 10)
  new_row2 <- sup_df_join %>%
    dplyr::select(REGION_ID, `PROJECT NAME`, FOREST_ID, `LMU (ACTUAL)`, `DECISION SIGNED`, value) %>%
    filter(value == i) %>%
    arrange(as.Date(`DECISION SIGNED`, tryFormats = c("%m/%d/%Y"))) %>%
    head(., 10)
  # Append the new row using rbind()
  arch_sup_df <- rbind(arch_sup_df, new_row, new_row2)
}


# NEPA Correlations with Hetero Metrics
# correlations
cor(df$shan_dv_all, nepa_summ_df$yearly_pct_med_EA_EIS, use = "complete.obs")
cor(df$ent_all, nepa_summ_df$yearly_pct_med_EA_EIS, use = "complete.obs")
cor(df$shan_dv_all, nepa_summ_df$total_EIS, use = "complete.obs")
cor(df$ent_all, nepa_summ_df$total_EIS, use = "complete.obs")
cor(df$shan_dv_all, nepa_summ_df$tot_appeal, use = "complete.obs")
cor(df$ent_all, nepa_summ_df$tot_appeal, use = "complete.obs")


pals_test <- pals_df_test %>%
  group_by(FOREST_ID) %>%
  summarise(ave_med_assess = mean(med_assess_time, na.rm = TRUE),
            ave_mean_assess = mean(mean_assess_time, na.rm = TRUE))

cor(df$shan_dv_all, pals_test$ave_med_assess, use = "complete.obs")
cor(df$ent_all, pals_test$ave_med_assess, use = "complete.obs")
