library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(exactextractr)
library(tigris)
library(MetBrewer)

# Load the data
sgfcm_all_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")
sgfcm_all_attri_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")
sgfcm_all_k6_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_all_result_k6_2024-10-15.tif")
nf_summ_df <- read_csv(here::here("outputs/tables/nf_level_dominant_archetypes_uncertainty_2024-12-11.csv")) 

# Load the USFS boundaries
fs_nf <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeForest.shp")
fs_reg <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeRegion.shp")

projection <- "epsg: 5070"

fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_nf.crop <- st_crop(fs_nf.proj, ext(sgfcm_all_attri_sc))
fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.crop <- st_crop(fs_reg.proj, ext(sgfcm_all_attri_sc))

# read in the 50km buffer shape

nf_buffers <- read_sf(here::here("data/processed/nf_buffers_50k_2024-10-22.shp"))

# crop the buffers to the outline of conus
reg_test <- fs_reg.crop %>%
  filter(REGION == "06")

nf_buff_test <- nf_buffers %>%
  filter(FORESTORGC == "0612")

test_int <- st_intersection(nf_buff_test, reg_test)
plot(test_int$geometry)

nf_buffers_int <- st_intersection(nf_buffers, fs_reg.crop)


# read in the national forest level shannon diversity shapefile and entropy shapefile 
shan_nf <- read_sf(here::here("data/processed/shan_h_nf_2024-11-05.shp"))
ent_nf <- read_sf(here::here("data/processed/ent_nf_2024-11-01.shp"))

# calculate the archetype areas assuming crisp archetypes for each forest 
buff <- nf_buffers_int

v <- buff %>% st_cast("MULTIPOLYGON")
z <- crop(sgfcm_all_k6_result, buff, mask = TRUE)

x <- exact_extract(z, v, coverage_area = TRUE)
names(x) <- v$FORESTORGC

areas <- bind_rows(x, .id = "FORESTORGC") %>%
  group_by(FORESTORGC, value) %>%
  summarize(total_arch_area = sum(coverage_area)) %>%
  group_by(FORESTORGC) %>%
  mutate(proportion_pct = round((total_arch_area/sum(total_arch_area))*100, 2)) %>%
  mutate(proportion = (total_arch_area/sum(total_arch_area)))

areas <- areas %>% 
  replace_na(list(value = 0))

areas <- areas %>%
  group_by(FORESTORGC) %>%
  mutate(max_pct = max(proportion_pct)) %>%
  ungroup()


#----Archetype Validation with Common Project Types----

# load pals data

pals_df <- read_delim("~/Analysis/NEPA_Efficiency/data/original/pals_ongoing_projects_11-2022.csv", delim = ";")

# Filter for date and select Forest Number and Purposes
pals_df_2009 <- pals_df %>%
  filter(as.Date(`INITIATION DATE`, format = "%m/%d/%Y") >= "2009-01-01") %>%
  filter(REGION_ID != "10" & REGION_ID != "13" & REGION_ID != "24" & REGION_ID != "00") %>%
  dplyr::select(REGION_ID, FOREST_ID, `FC Facility management – purpose`, 
                `FR Research – purpose`, `HF Fuels management – purpose`, `HR Heritage resource management – purpose`,
                `LM Land ownership management – purpose`, `LW Land acquisition – purpose`,
                `MG Minerals and geology – purpose`, `PN Land management planning – purpose`,
                `RD Road management – purpose`, `RG Grazing management – purpose`, `RO Regulations, directives, orders – purpose`,
                `RU Special area management – purpose`, `RW Recreation management – purpose`,
                `SU Special use management – purpose`, `TM Forest products – purpose`, 
                `VM Vegetation management (non-forest products) – purpose`,
                `WF Wildlife, fish, rare plants – purpose`, `WM Water management – purpose`) %>%
  group_by(FOREST_ID) %>%
  summarise(REGION = mean(as.numeric(REGION_ID)),
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

pals_df_test <- pals_df %>%
  filter(as.Date(`INITIATION DATE`, format = "%m/%d/%Y") >= "2009-01-01") %>%
  filter(REGION_ID != "10" & REGION_ID != "13" & REGION_ID != "24" & REGION_ID != "00") %>%
  dplyr::select(`SIGNED FY`, FOREST_ID, REGION_ID, `ELAPSED DAYS`,
                `FC Facility management – purpose`, 
                `FR Research – purpose`, `HF Fuels management – purpose`, `HR Heritage resource management – purpose`,
                `LM Land ownership management – purpose`, `LW Land acquisition – purpose`,
                `MG Minerals and geology – purpose`, `PN Land management planning – purpose`,
                `RD Road management – purpose`, `RG Grazing management – purpose`, `RO Regulations, directives, orders – purpose`,
                `RU Special area management – purpose`, `RW Recreation management – purpose`,
                `SU Special use management – purpose`, `TM Forest products – purpose`, 
                `VM Vegetation management (non-forest products) – purpose`,
                `WF Wildlife, fish, rare plants – purpose`, `WM Water management – purpose`) %>%
  group_by(`SIGNED FY`, FOREST_ID) %>%
  summarise(REGION = mean(as.numeric(REGION_ID)),
            mean_assess_time = mean(`ELAPSED DAYS`), 
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

areas_wide <- areas %>%
  dplyr::select(-total_arch_area) %>%
  pivot_wider(names_from = value, values_from = proportion_pct)

pals_purpose_arch_pct_area <- left_join(pals_df_2009, areas_wide, by = join_by(FOREST_ID == FORESTORGC)) %>%
  mutate_if(is.numeric, coalesce, 0)

# Filter for date and summarise the elapsed days and NEPA type counts
pals_df_2009_nepa_time <- pals_df %>%
  filter(as.Date(`INITIATION DATE`, format = "%m/%d/%Y") >= "2009-01-01") %>%
  filter(REGION_ID != "10" & REGION_ID != "13" & REGION_ID != "24" & REGION_ID != "00") %>%
  group_by(FOREST_ID) %>%
  summarise(REGION = mean(as.numeric(REGION_ID)),
            mean_nepa_time = mean(`ELAPSED DAYS`, na.rm = TRUE),
            med_nepa_time = median(`ELAPSED DAYS`, na.rm = TRUE))

pals_df_2009_nepa_time_year <- pals_df %>%
  filter(as.Date(`INITIATION DATE`, format = "%m/%d/%Y") >= "2009-01-01") %>%
  filter(REGION_ID != "10" & REGION_ID != "13" & REGION_ID != "24" & REGION_ID != "00") %>%
  group_by(`SIGNED FY`, FOREST_ID) %>%
  summarise(REGION = mean(as.numeric(REGION_ID)),
            mean_nepa_time = mean(`ELAPSED DAYS`, na.rm = TRUE),
            med_nepa_time = median(`ELAPSED DAYS`, na.rm = TRUE))

pals_df_2009_nepa_type <- pals_df %>%
  filter(as.Date(`INITIATION DATE`, format = "%m/%d/%Y") >= "2009-01-01") %>%
  filter(REGION_ID != "10" & REGION_ID != "13" & REGION_ID != "24" & REGION_ID != "00") %>%
  group_by(FOREST_ID) %>%
  count(`DECISION TYPE`) %>%
  pivot_wider(names_from = `DECISION TYPE`, values_from = n, values_fill = 0) 
  #mutate(pct_EA_EIS = ((ROD + DN)/(ROD + DN + DM)) * 100)

pals_df_2009_nepa_type_year <- pals_df %>%
  filter(as.Date(`INITIATION DATE`, format = "%m/%d/%Y") >= "2009-01-01") %>%
  filter(REGION_ID != "10" & REGION_ID != "13" & REGION_ID != "24" & REGION_ID != "00") %>%
  group_by(`SIGNED FY`, FOREST_ID) %>%
  count(`DECISION TYPE`) %>%
  pivot_wider(names_from = `DECISION TYPE`, values_from = n, values_fill = 0) 
  #mutate(pct_EA_EIS = ((ROD + DN)/(ROD + DN + DM)) * 100,
  #       total_projs = ROD + DN + DM)

# combine with the nf archetype summary df
nf_arch_summ_df <- left_join(pals_df_2009_nepa_time, pals_df_2009_nepa_type)
nf_arch_summ_df <- right_join(nf_arch_summ_df, nf_summ_df, by = c("FOREST_ID" = "forest_num"))

# combine the data by year
nf_arch_year_df <- left_join(pals_df_2009_nepa_time_year, pals_df_2009_nepa_type_year)
nf_arch_year_df_test <- right_join(nf_arch_year_df, nf_summ_df, by = c("FOREST_ID" = "forest_num"))

# save the csv file
#write_csv(nf_arch_summ_df, here::here(paste0("outputs/tables/nf_level_dominant_archetypes_uncertainty_nepa_", Sys.Date(), ".csv")))
#write_csv(nf_arch_year_df_test, here::here(paste0("outputs/tables/nf_level_pals_year_arch_summs_", Sys.Date(), ".csv")))

# look at region 4 
pals_arch_reg4 <- pals_purpose_arch_pct_area %>%
  filter(FOREST_ID %in% c("0401", "0402", "0403", "0407", "0408", "0410", "0412", "0413", "0414", "0415", "0417", "0419"))

pals_arch_reg4$FOREST_ID <- as.character(pals_arch_reg4$FOREST_ID)
reg4 <- pals_arch_reg4 %>% 
  group_by(FOREST_ID) %>%
  summarise(across(is.numeric, mean, na.rm = TRUE))

reg_4 <- pals_arch_reg4 %>% 
  group_by(FOREST_ID) %>%
  summarise(across(is.numeric, mean, na.rm = TRUE)) %>%
  dplyr::select(1:19) %>%
  pivot_longer(!FOREST_ID, names_to = "purpose", values_to = "count")

reg4_projects <- ggplot(reg_4, aes(x=purpose, y = count)) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge(), color = "black") +
  facet_wrap(~FOREST_ID) +
  theme_minimal() + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
reg4_projects
ggsave(here::here(paste0("outputs/plots/reg4_proj_purpose_by_forest_",
                                        Sys.Date(), ".png")),
       reg4_projects, width = 12, height = 10, dpi = 300)

# look at region 5 
pals_arch_reg5 <- pals_purpose_arch_pct_area %>%
  filter(FOREST_ID %in% c("0501", "0502", "0503", "0504", "0505", "0506", "0507", "0508", "0509", "0510", "0511", "0512", "0513", "0514", "0515", "0516", "0517", "0519"))

pals_arch_reg5$FOREST_ID <- as.character(pals_arch_reg5$FOREST_ID)
reg5 <- pals_arch_reg5 %>% 
  group_by(FOREST_ID) %>%
  summarise(across(is.numeric, mean, na.rm = TRUE))

reg_5 <- pals_arch_reg5 %>% 
  group_by(FOREST_ID) %>%
  summarise(across(is.numeric, mean, na.rm = TRUE)) %>%
  dplyr::select(1:19) %>%
  pivot_longer(!FOREST_ID, names_to = "purpose", values_to = "count")

reg5_projects <- ggplot(reg_5, aes(x=purpose, y = count)) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge(), color = "black") +
  facet_wrap(~FOREST_ID) +
  theme_minimal() + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
reg5_projects
ggsave(here::here(paste0("outputs/plots/reg5_proj_purpose_by_forest_",
                         Sys.Date(), ".png")),
       reg5_projects, width = 12, height = 10, dpi = 300)

# look at region 8 
pals_arch_reg8 <- pals_purpose_arch_pct_area %>%
  filter(FOREST_ID %in% c("0801", "0802", "0803", "0804", "0805", "0806", "0807", "0808", "0809", "0810", "0811", "0812", "0813", "0860"))

pals_arch_reg8$FOREST_ID <- as.character(pals_arch_reg8$FOREST_ID)
reg8 <- pals_arch_reg8 %>% 
  group_by(FOREST_ID) %>%
  summarise(across(is.numeric, mean, na.rm = TRUE))

reg_8 <- pals_arch_reg8 %>% 
  group_by(FOREST_ID) %>%
  summarise(across(is.numeric, mean, na.rm = TRUE)) %>%
  dplyr::select(1:19) %>%
  pivot_longer(!FOREST_ID, names_to = "purpose", values_to = "count")

reg8_projects <- ggplot(reg_8, aes(x=purpose, y = count)) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge(), color = "black") +
  facet_wrap(~FOREST_ID, ncol = 3) +
  theme_minimal() + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
reg8_projects
ggsave(here::here(paste0("outputs/plots/reg8_proj_purpose_by_forest_",
                         Sys.Date(), ".png")),
       reg8_projects, width = 12, height = 10, dpi = 300)




# look at projects by region
reg_test <- pals_df_test %>% 
  group_by(REGION) %>%
  summarise(across(is.numeric, sum, na.rm = TRUE)) %>% 
  dplyr::select(-`SIGNED FY`) %>%
  dplyr::select(-mean_assess_time) %>%
  pivot_longer(!REGION, names_to = "purpose", values_to = "count")

region_names <- list(
  '1' = "R1 - Northern",
  '2' = "R2 - Rocky Mnt",
  '3' = "R3 - Southwestern",
  '4' = "R4 - Intermountain", 
  '5' = "R5 - Pacific SW",
  '6' = "R6 - Pacific NW", 
  '8' = "Southern",
  '9' = "Eastern"
)

region_labeller <- function(variable,value){
  return(region_names[value])
}

reg_projects <- ggplot(reg_test, aes(x=purpose, y = count)) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge(), color = "black") +
  facet_wrap(~as.factor(REGION), ncol = 4, labeller = region_labeller) +
  theme_minimal() + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
reg_projects
ggsave(here::here(paste0("outputs/plots/regs_proj_purpose_by_forest_",
                         Sys.Date(), ".png")),
       reg_projects, width = 12, height = 10, dpi = 300)

# could filter by forests with >70% in any specific archetype

# try with archetypes 1 and 4 first
dom_pct <- 50

arche1 <-  pals_purpose_arch_pct_area %>%
  filter(`1` >= dom_pct) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "one", 
         pct_purpose = values/sum(values) * 100)

arche2 <- pals_purpose_arch_pct_area %>%
  filter(`2` >= dom_pct) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "two", 
         pct_purpose = values/sum(values) * 100)

arche3 <- pals_purpose_arch_pct_area %>%
  filter(`3` >= dom_pct) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "three", 
         pct_purpose = values/sum(values) * 100)

arche4 <- pals_purpose_arch_pct_area %>%
  filter(`4` >= dom_pct) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "four", 
         pct_purpose = values/sum(values) * 100)

arche5 <- pals_purpose_arch_pct_area %>%
  filter(`5` >= dom_pct) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "five", 
         pct_purpose = values/sum(values) * 100)

arche6 <- pals_purpose_arch_pct_area %>%
  filter(`6` >= dom_pct) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "six", 
         pct_purpose = values/sum(values) * 100)

arche_no_dom <- pals_purpose_arch_pct_area %>%
  filter(`1` < dom_pct & `2` < dom_pct & `3` < dom_pct & `4` < dom_pct & `5` < dom_pct & `6` < dom_pct) %>%
  filter(FOREST_ID != "0000" | FOREST_ID != "1004" | FOREST_ID != "1005" | FOREST_ID != "2400" | FOREST_ID != "2403" | FOREST_ID != "2408") %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "no dominant archetype (>70%)",
         pct_purpose = values/sum(values) * 100)

#arche1.4 <- left_join(arche1, arche4)
arche_purposes <- rbind(arche1, arche2, arche3, arche4, arche5, arche6, arche_no_dom)

arche_purposes$archetype <- factor(arche_purposes$archetype,
                                   levels = c("one", "two", "three",
                                              "four", "five", "six",
                                              "no dominant archetype (>70%)"))

ggplot(arche_no_dom, aes(x=purpose, y = pct_purpose)) +
  geom_bar(stat="identity", width = 0.7, fill = "steelblue") +
  theme_minimal()

arch_all_purpose <- ggplot(arche_purposes, aes(x=purpose, y = pct_purpose, fill = archetype)) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge(), color = "black") +
  scale_fill_discrete(limits=c("one", "two", "three", 
                               "four", "five", "six",
                               "no dominant archetype (>70%)")) + 
  scale_fill_met_d("Hokusai3") +
  theme_minimal() + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
arch_all_purpose
ggsave(filename = here::here(paste0("outputs/plots/archetype_validation_test_70_", Sys.Date(), ".png")), 
       plot = arch_all_purpose, 
       width = 12, 
       height = 4, dpi = 300)

#---Without looking at dominant archetype or by filtering out high ent - high div forests

nf_pals_df <- left_join(nf_arch_summ_df, pals_df_2009, by = "FOREST_ID")

nf_pal_lowlow <- nf_pals_df %>%
  filter(div_to_ent == "low_ent_low_div") %>%
  select(FOREST_ID, REGION.x, starts_with("p_"))

nf_pal_lowhigh <- nf_pals_df %>%
  filter(div_to_ent == "low_ent_high_div") %>%
  select(FOREST_ID, REGION.x, starts_with("p_"))

nf_pal_highlow <- nf_pals_df %>%
  filter(div_to_ent == "high_ent_low_div") %>%
  select(FOREST_ID, REGION.x, starts_with("p_"))

nf_pal_highhigh <- nf_pals_df %>%
  filter(div_to_ent == "high_ent_high_div") %>%
  select(FOREST_ID, REGION.x, starts_with("p_"))

lowlow <- nf_pal_lowlow %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(pct_purpose = values/sum(values) * 100,
         ent_div = "low-low")

lowhigh <- nf_pal_lowhigh %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(pct_purpose = values/sum(values) * 100,
         ent_div = "low-high")

highlow <- nf_pal_highlow %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(pct_purpose = values/sum(values) * 100,
         ent_div = "high-low")

highhigh <- nf_pal_highhigh %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(pct_purpose = values/sum(values) * 100,
         ent_div = "high-high")

ent_div_purpose <- rbind(lowlow, lowhigh, highlow, highhigh)

ent_div_purpose_plot <- ggplot(ent_div_purpose, aes(x=purpose, y = pct_purpose, fill = ent_div)) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge(), color = "black") +
  scale_fill_discrete(limits=c("low-low", "low-high", "high-low", "high-high")) + 
  scale_fill_met_d("Hokusai3") +
  theme_minimal() + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
ent_div_purpose_plot
ggsave(filename = here::here(paste0("outputs/plots/archetype_validation_test_ent_div_purpose_", Sys.Date(), ".png")), 
       plot = ent_div_purpose_plot, 
       width = 12, 
       height = 4, dpi = 300)

nf_pal_nohighhigh_domarch <- nf_pals_df %>%
  filter(div_to_ent != "high_ent_high_div") %>%
  select(FOREST_ID, REGION.x, starts_with("p_"), dom_archetype)

nf_pal_highhigh_domarch <- nf_pals_df %>%
  filter(div_to_ent == "high_ent_high_div") %>%
  select(FOREST_ID, REGION.x, starts_with("p_"), dom_archetype)


a1_nhh <-  nf_pal_nohighhigh_domarch %>%
  filter(dom_archetype == 1) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "A", 
         pct_purpose = values/sum(values) * 100)

a2_nhh <-  nf_pal_nohighhigh_domarch %>%
  filter(dom_archetype == 2) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(archetype = "B", 
         pct_purpose = values/sum(values) * 100)

a3_nhh <-  nf_pal_nohighhigh_domarch %>%
  filter(dom_archetype == 3) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "C", 
         pct_purpose = values/sum(values) * 100)

a4_nhh <-  nf_pal_nohighhigh_domarch %>%
  filter(dom_archetype == 4) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "D", 
         pct_purpose = values/sum(values) * 100)

a5_nhh <-  nf_pal_nohighhigh_domarch %>%
  filter(dom_archetype == 5) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "E", 
         pct_purpose = values/sum(values) * 100)

a6_nhh <-  nf_pal_nohighhigh_domarch %>%
  filter(dom_archetype == 6) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "F", 
         pct_purpose = values/sum(values) * 100)

hh <- nf_pal_highhigh_domarch %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "high_high", 
         pct_purpose = values/sum(values) * 100)

test_dom_hh <- rbind(a1_nhh, a2_nhh, a3_nhh, a4_nhh, a5_nhh, a6_nhh, hh)

test_dom_hh_plot <- ggplot(test_dom_hh, aes(x=purpose, y = pct_purpose, fill = archetype)) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge(), color = "black") +
  scale_fill_discrete(limits=c("A", "B", "C", "D", "E", "F", "high ent - high div")) + 
  scale_fill_met_d("Hokusai3") +
  theme_minimal() + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
test_dom_hh_plot
ggsave(filename = here::here(paste0("outputs/plots/archetype_validation_test_nothres_highhigh_purpose_", Sys.Date(), ".png")), 
       plot = test_dom_hh_plot, 
       width = 12, 
       height = 4, dpi = 300)


# I still want to figure something out with the regions and archetypes? 
nf_pals_year_df <- left_join(nf_arch_year_df_test, pals_df_2009, by = "FOREST_ID")

r1 <- nf_pals_year_df %>%
  filter(REGION.x == 1)
r2 <- nf_pals_year_df %>%
  filter(REGION.x == 2)
r3 <- nf_pals_year_df %>%
  filter(REGION.x == 3)
r4 <- nf_pals_year_df %>%
  filter(REGION.x == 4)
r5 <- nf_pals_year_df %>%
  filter(REGION.x == 5)
r6 <- nf_pals_year_df %>%
  filter(REGION.x == 6)
r8 <- nf_pals_year_df %>%
  filter(REGION.x == 8)
r9 <- nf_pals_year_df %>%
  filter(REGION.x == 9)

r4_filt <- pals_df %>%
  filter(as.Date(`INITIATION DATE`, format = "%m/%d/%Y") >= "2009-01-01") %>%
  filter(FOREST_ID %in% c("0402", "0412", "0407", "0408", "0410")) %>%
  filter(`ELAPSED DAYS` >= 0) %>%
  mutate(forname_fct = factor(`LMU – FOREST`, levels = c("BoNF",
                                                         "PaNF",
                                                         "DiNF",
                                                         "FiNF",
                                                         "MLNF")))

r4_filt_test <- nf_pals_year_df %>%
  filter(FOREST_ID %in% c("0402", "0412", "0407", "0408", "0410")) %>%
  mutate(forname_fct = factor(forest_name, levels = c("Boise National Forest", 
                                                      "Payette National Forest",
                                                      "Dixie National Forest", 
                                                      "Fishlake National Forest",
                                                      "Manti-La Sal National Forest")))
  #filter(`ELAPSED DAYS` >= 0) %>%
  #mutate(forname_fct = factor(`LMU – FOREST`, levels = c("BoNF",
                                                         

r4_filt_boxplots <- ggplot(r4_filt_test, aes(x = forname_fct, y = ((DN + ROD) / med_nepa_time) , fill = forname_fct)) + 
  geom_boxplot() 
  #facet_wrap(~`SIGNED FY`)
r4_filt_boxplots
ggsave(filename = here::here(paste0("outputs/plots/archetype_validation_r4_forests_boxplots_med_time_", Sys.Date(), ".png")), 
       plot = r4_filt_boxplots, 
       width = 12, 
       height = 6, dpi = 300)

#----What does this mean for NEPA assessment times?----
# selected forests from PMRC draft
pals_df_sel <- pals_df %>%
  filter(FOREST_ID %in% c("0511", "0909", "0402", "0801")) %>%
  filter(as.Date(`INITIATION DATE`, format = "%m/%d/%Y") >= "2009-01-01") %>%
  group_by(FOREST_ID, `DECISION TYPE`) %>%
  summarise(ave_days = mean(`ELAPSED DAYS`, na.rm = TRUE),
            count = n())

pals_count <- pals_df %>% 
  select(FOREST_ID, `DECISION TYPE`) %>%
  filter(FOREST_ID %in% c("0511", "0909", "0402", "0801")) %>%
  group_by(FOREST_ID, `DECISION TYPE`) %>% 
  summarise(count = n())

pals_df_edays <- pals_df %>%
  filter(as.Date(`INITIATION DATE`, format = "%m/%d/%Y") >= "2009-01-01") %>%
  group_by(FOREST_ID, `DECISION TYPE`) %>%
  summarise(ave_days = mean(`ELAPSED DAYS`, na.rm = TRUE),
            count = n())
shan_df <- shan_h %>%
  select(FORESTORGC, shan_div)

pals_edays_shanh <- left_join(pals_df_edays, shan_df, by = c("FOREST_ID" = "FORESTORGC"))

edays_shanh_plot <- ggplot(pals_edays_shanh, aes(shan_div, ave_days)) +
  geom_point(aes(color = factor(`DECISION TYPE`)))
edays_shanh_plot

hist(pals_edays_shanh$ave_days)
hist(pals_edays_shanh$shan_div)

test_hist <- ggplot(pals_edays_shanh) +
  geom_histogram(aes(mapping = shan_div, color = factor(`DECISION TYPE`)))
test_hist

test_shandiv <- pals_edays_shanh %>%
  ggplot( aes(x=shan_div, fill=factor(`DECISION TYPE`))) +
  geom_histogram( color="#e9ecef", alpha=0.5, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080", "#404")) +
  facet_wrap(~factor(`DECISION TYPE`)) +
  theme_bw() +
  labs(fill="")
test_shandiv

test_days <- pals_edays_shanh %>%
  ggplot( aes(x=ave_days, fill=factor(`DECISION TYPE`))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080", "#404")) +
  theme_bw() +
  labs(fill="")
test_days


## using pals_df_test create a time series plot facet wrap by region and each line a forest

ggplot(pals_df_test, aes(x = `SIGNED FY`, y = mean_assess_time, color = FOREST_ID)) +
  geom_line() +
  theme(legend.position = "blank") +
  facet_wrap(~REGION)

# join pals_df_test to pals_df_2009_nepa_type_year and pals_df_2009_nepa_time_year?

ggplot(pals_df_2009_nepa_type_year, aes(x = `SIGNED FY`, y = total_projs, color = FOREST_ID)) +
  geom_line() +
  theme(legend.position = "blank")
  #facet_wrap(~REGION)



