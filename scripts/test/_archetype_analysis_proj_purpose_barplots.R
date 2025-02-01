library(tidyverse)
library(ggplot2)
library(MetBrewer)

# load the data

df <- read_csv(here::here("outputs/tables/nf_level_dominant_archetypes_uncertainty_nepa_2025-01-30.csv")) 
df_year <- read_csv(here::here("outputs/tables/nf_level_pals_year_arch_summs_2025-01-30.csv"))
pals_df <- read_delim("~/Analysis/NEPA_Efficiency/data/original/pals_ongoing_projects_11-2022.csv", delim = ";")

# Filter for date and select Forest Number and Purposes
pals_df_2009 <- pals_df %>%
  filter(as.Date(`INITIATION DATE`, format = "%m/%d/%Y") >= "2009-01-01") %>%
  filter(REGION_ID != "10" & REGION_ID != "13" & REGION_ID != "24" & REGION_ID != "00")

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

pals_df_test_filt <- pals_df_test %>%
  filter(FOREST_ID %in% c("0402", "0412", "0407", "0408", "0410"))


nf_pals_df <- left_join(df, pals_df_test, by = "FOREST_ID")



r1 <- nf_pals_df %>%
  filter(REGION == 1) %>%
  select(FOREST_ID, REGION, starts_with("p_"), dom_archetype)
r2 <- nf_pals_df %>%
  filter(REGION == 2)%>%
  select(FOREST_ID, REGION, starts_with("p_"), dom_archetype)
r3 <- nf_pals_df %>%
  filter(REGION == 3)%>%
  select(FOREST_ID, REGION, starts_with("p_"), dom_archetype)
r4 <- nf_pals_df %>%
  filter(REGION == 4)%>%
  select(FOREST_ID, REGION, starts_with("p_"), dom_archetype)
r5 <- nf_pals_df %>%
  filter(REGION == 5)%>%
  select(FOREST_ID, REGION, starts_with("p_"), dom_archetype)
r6 <- nf_pals_df %>%
  filter(REGION == 6)%>%
  select(FOREST_ID, REGION, starts_with("p_"), dom_archetype)
r8 <- nf_pals_df %>%
  filter(REGION == 8)%>%
  select(FOREST_ID, REGION, starts_with("p_"), dom_archetype)
r9 <- nf_pals_df %>%
  filter(REGION == 9)%>%
  select(FOREST_ID, REGION, starts_with("p_"), dom_archetype)

# for R1
r1_a1 <- r1 %>% 
  filter(dom_archetype == 1) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "A", 
         pct_purpose = values/sum(values) * 100)
r1_a2 <- r1 %>% 
  filter(dom_archetype == 2) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "B", 
         pct_purpose = values/sum(values) * 100)
r1_a3 <- r1 %>% 
  filter(dom_archetype == 3) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "C", 
         pct_purpose = values/sum(values) * 100)
r1_a4 <- r1 %>% 
  filter(dom_archetype == 4) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "D", 
         pct_purpose = values/sum(values) * 100)
r1_a5 <- r1 %>% 
  filter(dom_archetype == 5) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "E", 
         pct_purpose = values/sum(values) * 100)
r1_a6 <- r1 %>% 
  filter(dom_archetype == 6) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "F", 
         pct_purpose = values/sum(values) * 100)

r1_arch <- rbind(r1_a1, r1_a2, r1_a3, r1_a4, r1_a5, r1_a6)
r1_arch <- r1_arch %>%
  mutate(region = "R1")

# for R2
r2_a1 <- r2 %>% 
  filter(dom_archetype == 1) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "A", 
         pct_purpose = values/sum(values) * 100)
r2_a2 <- r2 %>% 
  filter(dom_archetype == 2) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "B", 
         pct_purpose = values/sum(values) * 100)
r2_a3 <- r2 %>% 
  filter(dom_archetype == 3) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "C", 
         pct_purpose = values/sum(values) * 100)
r2_a4 <- r2 %>% 
  filter(dom_archetype == 4) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "D", 
         pct_purpose = values/sum(values) * 100)
r2_a5 <- r2 %>% 
  filter(dom_archetype == 5) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "E", 
         pct_purpose = values/sum(values) * 100)
r2_a6 <- r2 %>% 
  filter(dom_archetype == 6) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "F", 
         pct_purpose = values/sum(values) * 100)

r2_arch <- rbind(r2_a1, r2_a2, r2_a3, r2_a4, r2_a5, r2_a6)
r2_arch <- r2_arch %>%
  mutate(region = "R2")

# for R3
r3_a1 <- r3 %>% 
  filter(dom_archetype == 1) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "A", 
         pct_purpose = values/sum(values) * 100)
r3_a2 <- r3 %>% 
  filter(dom_archetype == 2) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "B", 
         pct_purpose = values/sum(values) * 100)
r3_a3 <- r3 %>% 
  filter(dom_archetype == 3) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "C", 
         pct_purpose = values/sum(values) * 100)
r3_a4 <- r3 %>% 
  filter(dom_archetype == 4) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "D", 
         pct_purpose = values/sum(values) * 100)
r3_a5 <- r3 %>% 
  filter(dom_archetype == 5) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "E", 
         pct_purpose = values/sum(values) * 100)
r3_a6 <- r3 %>% 
  filter(dom_archetype == 6) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "F", 
         pct_purpose = values/sum(values) * 100)

r3_arch <- rbind(r3_a1, r3_a2, r3_a3, r3_a4, r3_a5, r3_a6)
r3_arch <- r3_arch %>%
  mutate(region = "R3")

# for R4
r4_a1 <- r4 %>% 
  filter(dom_archetype == 1) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "A", 
         pct_purpose = values/sum(values) * 100)
r4_a2 <- r4 %>% 
  filter(dom_archetype == 2) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "B", 
         pct_purpose = values/sum(values) * 100)
r4_a3 <- r4 %>% 
  filter(dom_archetype == 3) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "C", 
         pct_purpose = values/sum(values) * 100)
r4_a4 <- r4 %>% 
  filter(dom_archetype == 4) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "D", 
         pct_purpose = values/sum(values) * 100)
r4_a5 <- r4 %>% 
  filter(dom_archetype == 5) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "E", 
         pct_purpose = values/sum(values) * 100)
r4_a6 <- r4 %>% 
  filter(dom_archetype == 6) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "F", 
         pct_purpose = values/sum(values) * 100)

r4_arch <- rbind(r4_a1, r4_a2, r4_a3, r4_a4, r4_a5, r4_a6)
r4_arch <- r4_arch %>%
  mutate(region = "R4")

# for R5
r5_a1 <- r5 %>% 
  filter(dom_archetype == 1) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "A", 
         pct_purpose = values/sum(values) * 100)
r5_a2 <- r5 %>% 
  filter(dom_archetype == 2) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "B", 
         pct_purpose = values/sum(values) * 100)
r5_a3 <- r5 %>% 
  filter(dom_archetype == 3) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "C", 
         pct_purpose = values/sum(values) * 100)
r5_a4 <- r5 %>% 
  filter(dom_archetype == 4) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "D", 
         pct_purpose = values/sum(values) * 100)
r5_a5 <- r5 %>% 
  filter(dom_archetype == 5) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "E", 
         pct_purpose = values/sum(values) * 100)
r5_a6 <- r5 %>% 
  filter(dom_archetype == 6) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "F", 
         pct_purpose = values/sum(values) * 100)

r5_arch <- rbind(r5_a1, r5_a2, r5_a3, r5_a4, r5_a5, r5_a6)
r5_arch <- r5_arch %>%
  mutate(region = "R5")

# for R6
r6_a1 <- r6 %>% 
  filter(dom_archetype == 1) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "A", 
         pct_purpose = values/sum(values) * 100)
r6_a2 <- r6 %>% 
  filter(dom_archetype == 2) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "B", 
         pct_purpose = values/sum(values) * 100)
r6_a3 <- r6 %>% 
  filter(dom_archetype == 3) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "C", 
         pct_purpose = values/sum(values) * 100)
r6_a4 <- r6 %>% 
  filter(dom_archetype == 4) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "D", 
         pct_purpose = values/sum(values) * 100)
r6_a5 <- r6 %>% 
  filter(dom_archetype == 5) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "E", 
         pct_purpose = values/sum(values) * 100)
r6_a6 <- r6 %>% 
  filter(dom_archetype == 6) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "F", 
         pct_purpose = values/sum(values) * 100)

r6_arch <- rbind(r6_a1, r6_a2, r6_a3, r6_a4, r6_a5, r6_a6)
r6_arch <- r6_arch %>%
  mutate(region = "R6")

# for R8
r8_a1 <- r8 %>% 
  filter(dom_archetype == 1) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "A", 
         pct_purpose = values/sum(values) * 100)
r8_a2 <- r8 %>% 
  filter(dom_archetype == 2) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "B", 
         pct_purpose = values/sum(values) * 100)
r8_a3 <- r8 %>% 
  filter(dom_archetype == 3) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "C", 
         pct_purpose = values/sum(values) * 100)
r8_a4 <- r8 %>% 
  filter(dom_archetype == 4) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "D", 
         pct_purpose = values/sum(values) * 100)
r8_a5 <- r8 %>% 
  filter(dom_archetype == 5) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "E", 
         pct_purpose = values/sum(values) * 100)
r8_a6 <- r8 %>% 
  filter(dom_archetype == 6) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "F", 
         pct_purpose = values/sum(values) * 100)

r8_arch <- rbind(r8_a1, r8_a2, r8_a3, r8_a4, r8_a5, r8_a6)
r8_arch <- r8_arch %>%
  mutate(region = "R8")

# for R9
r9_a1 <- r9 %>% 
  filter(dom_archetype == 1) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "A", 
         pct_purpose = values/sum(values) * 100)
r9_a2 <- r9 %>% 
  filter(dom_archetype == 2) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "B", 
         pct_purpose = values/sum(values) * 100)
r9_a3 <- r9 %>% 
  filter(dom_archetype == 3) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "C", 
         pct_purpose = values/sum(values) * 100)
r9_a4 <- r9 %>% 
  filter(dom_archetype == 4) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "D", 
         pct_purpose = values/sum(values) * 100)
r9_a5 <- r9 %>% 
  filter(dom_archetype == 5) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "E", 
         pct_purpose = values/sum(values) * 100)
r9_a6 <- r9 %>% 
  filter(dom_archetype == 6) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(arch = "F", 
         pct_purpose = values/sum(values) * 100)

r9_arch <- rbind(r9_a1, r9_a2, r9_a3, r9_a4, r9_a5, r9_a6)
r9_arch <- r9_arch %>%
  mutate(region = "R9")

purp_reg <- rbind(r1_arch, r2_arch, r3_arch, r4_arch, r5_arch, r6_arch, r8_arch, r9_arch)



test_reg_arch_plot <- ggplot(purp_reg, aes(x=purpose, y = pct_purpose, fill = arch)) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge(), color = "black") +
  facet_wrap(~region, ncol = 4) +
  scale_fill_discrete(limits=c("A", "B", "C", "D", "E", "F")) + 
  scale_fill_met_d("Hokusai3") +
  theme_minimal() + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
test_reg_arch_plot
ggsave(here::here(paste0("outputs/plots/draft_reg_arch_plot_",
                          Sys.Date(), ".png")),
       test_reg_arch_plot, width = 12, height = 5, dpi = 300)
