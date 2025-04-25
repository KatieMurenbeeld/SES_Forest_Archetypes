library(tidyverse)
library(ggplot2)
library(MetBrewer)
library(data.table)

# load the data

# need to write the script that saves the nf_level_dominant_archetypes_uncertainty_nepa_2025-01-30.csv
df <- read_csv(here::here("outputs/tables/nf_level_dominant_archetypes_uncertainty_nepa_2025-04-24.csv")) 
# need to write the script that saves the nf_level_pals_year_arch_summs_2025-01-30.csv
df_year <- read_csv(here::here("outputs/tables/nf_level_pals_year_arch_summs_2025-04-24.csv"))
pals_df <- read_delim("~/Analysis/NEPA_Efficiency/data/original/pals_ongoing_projects_11-2022.csv", delim = ";")


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


nf_pals_df <- left_join(df, pals_df_test, by = "FOREST_ID")

a1 <-  nf_pals_df %>%
  filter(dom_archetype == 1) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
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
  summarise(values = sum(value)) %>%
  mutate(archetype = "C", 
         pct_purpose = values/sum(values) * 100)

a4 <-  nf_pals_df %>%
  filter(dom_archetype == 4) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "D", 
         pct_purpose = values/sum(values) * 100)

a5 <-  nf_pals_df %>%
  filter(dom_archetype == 5) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "E", 
         pct_purpose = values/sum(values) * 100)

a6 <-  nf_pals_df %>%
  filter(dom_archetype == 6) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "F", 
         pct_purpose = values/sum(values) * 100)

archs <- rbind(a1, a2, a3, a4, a5, a6)

# need to filter out purposes where all archetypes are below 3%
## probably a much easier way to do this, but I got it to work.
dt_archs <- as.data.table(archs)

low_p_A <- dt_archs[pct_purpose < 3 & archetype == "A"]
low_p_B <- dt_archs[pct_purpose < 3 & archetype == "B"]
low_p_C <- dt_archs[pct_purpose < 3 & archetype == "C"]
low_p_D <- dt_archs[pct_purpose < 3 & archetype == "D"]
low_p_E <- dt_archs[pct_purpose < 3 & archetype == "E"]
low_p_F <- dt_archs[pct_purpose < 3 & archetype == "F"]

test_join <- left_join(low_p_A, low_p_B, by = "purpose") %>%
  left_join(., low_p_C, by='purpose') %>%
  left_join(., low_p_D, by='purpose') %>%
  left_join(., low_p_E, by='purpose') %>%
  left_join(., low_p_F, by='purpose') %>%
  drop_na()

for (p in test_join$purpose){
  archs_filt <- archs %>% 
    filter(purpose != p)
}

archs_filt <- archs %>%
  filter(purpose != test_join$purpose[1]) %>%
  filter(., purpose != test_join$purpose[2]) %>%
  filter(., purpose != test_join$purpose[3]) %>%
  filter(., purpose != test_join$purpose[4]) %>%
  filter(., purpose != test_join$purpose[5]) %>%
  filter(., purpose != test_join$purpose[6]) %>%
  filter(., purpose != test_join$purpose[7])

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
  scale_fill_discrete(limits=c("A", "B", "C", "D", "E", "F")) + 
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

ggsave(here::here(paste0("outputs/plots/archetype_analysis_fig5_testing_", Sys.Date(), ".jpeg")),
       archs_purpose_plot,  height = 100, width = 140, dpi = 500, units = "mm", device = "jpeg", bg = "white")
