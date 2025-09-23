library(tidyverse)
library(stringr)
library(terra)
library(sf)
library(ggplot2)
library(exactextractr)
library(tigris)
library(MetBrewer)
library(ggtext)

# Load the previously made NEPA sums
nepa_summ_df <- read_csv(here::here("outputs/tables/nf_nepa_projs_arch_summ_2025-09-11.csv"))
df_year <- read_csv(here::here("outputs/tables/nf_level_pals_year_arch_summs_2025-04-24.csv"))
pals_df <- read_delim("~/Analysis/NEPA_Efficiency/data/original/pals_ongoing_projects_11-2022.csv", delim = ";")

# simple histograms of diversity metrics
plot(hist(nepa_summ_df$pct_area_dom_arch))
plot(hist(nepa_summ_df$shan_diverse))
plot(hist(nepa_summ_df$shan_diverse_norm))
plot(hist(nepa_summ_df$entropy_all))


# look into changes in project portfolios through time
#------------------------------------------------------
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
                `WF Wildlife, fish, rare plants – purpose`, `WM Water management – purpose`,
                `APPEALED OR OBJECTED?`, `LITIGATED?`) %>%
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
            p_water = sum(`WM Water management – purpose`),
            total_appealed = sum(`APPEALED OR OBJECTED?`),
            total_litigated = sum(`LITIGATED?`))


test_pct_year <- pals_df_test %>%
  group_by(`SIGNED FY`) %>%
  summarise(tot_spec_use = sum(p_spec_use),
            tot_for_prod = sum(p_forest_prod),
            tot_rec = sum(p_recreation),
            tot_haz_fuel = sum(p_haz_fuels),
            tot_wlife = sum(p_wildlife),
            tot_geo = sum(p_min_geo),
            tot_veg_mngt = sum(p_veg_mngt),
            tot_all_proj = sum(c_across(starts_with("p_")))) %>%
  mutate(pct_spec_use = (tot_spec_use / tot_all_proj)*100,
         pct_for_prod = (tot_for_prod / tot_all_proj)*100,
         pct_rec = (tot_rec / tot_all_proj)*100, 
         pct_haz_fuel = (tot_haz_fuel / tot_all_proj)*100,
         pct_wlife = (tot_wlife / tot_all_proj)*100,
         pct_geo = (tot_geo / tot_all_proj)*100,
         pct_veg_mngt = (tot_veg_mngt / tot_all_proj)*100) %>%
  drop_na()

test_pct_year_reg <- pals_df_test %>%
  group_by(REGION, `SIGNED FY`) %>%
  summarise(tot_spec_use = sum(p_spec_use),
            tot_for_prod = sum(p_forest_prod),
            tot_rec = sum(p_recreation),
            tot_haz_fuel = sum(p_haz_fuels),
            tot_wlife = sum(p_wildlife),
            tot_geo = sum(p_min_geo),
            tot_veg_mngt = sum(p_veg_mngt),
            tot_all_proj = sum(c_across(starts_with("p_")))) %>%
  mutate(pct_spec_use = (tot_spec_use / tot_all_proj)*100,
         pct_for_prod = (tot_for_prod / tot_all_proj)*100,
         pct_rec = (tot_rec / tot_all_proj)*100, 
         pct_haz_fuel = (tot_haz_fuel / tot_all_proj)*100,
         pct_wlife = (tot_wlife / tot_all_proj)*100,
         pct_geo = (tot_geo / tot_all_proj)*100,
         pct_veg_mngt = (tot_veg_mngt / tot_all_proj)*100) %>%
  drop_na()

test_pct_year_nf <- pals_df_test %>%
  group_by(FOREST_ID, `SIGNED FY`) %>%
  summarise(tot_spec_use = sum(p_spec_use),
            tot_for_prod = sum(p_forest_prod),
            tot_rec = sum(p_recreation),
            tot_haz_fuel = sum(p_haz_fuels),
            tot_wlife = sum(p_wildlife),
            tot_geo = sum(p_min_geo),
            tot_veg_mngt = sum(p_veg_mngt),
            tot_all_proj = sum(c_across(starts_with("p_")))) %>%
  mutate(pct_spec_use = (tot_spec_use / tot_all_proj)*100,
         pct_for_prod = (tot_for_prod / tot_all_proj)*100,
         pct_rec = (tot_rec / tot_all_proj)*100, 
         pct_haz_fuel = (tot_haz_fuel / tot_all_proj)*100,
         pct_wlife = (tot_wlife / tot_all_proj)*100,
         pct_geo = (tot_geo / tot_all_proj)*100,
         pct_veg_mngt = (tot_veg_mngt / tot_all_proj)*100) %>%
  drop_na()

# plot some line plots

test_pct_year_long <- test_pct_year %>%
  dplyr::select(`SIGNED FY`, pct_spec_use, pct_for_prod, pct_rec, 
                pct_haz_fuel, pct_wlife, pct_geo, pct_veg_mngt) %>%
  pivot_longer(!`SIGNED FY`, names_to = "projects", values_to = "values")

ggplot(data = test_pct_year_long, aes(x = `SIGNED FY`, y = values, color = projects)) + 
  geom_line()
  
test_pct_year_reg_long <- test_pct_year_reg %>%
  dplyr::select(`SIGNED FY`, REGION, pct_spec_use, pct_for_prod, pct_rec, 
                pct_haz_fuel, pct_wlife, pct_geo, pct_veg_mngt) %>%
  pivot_longer(!c(`SIGNED FY`,REGION), names_to = "projects", values_to = "values")

ggplot(data = test_pct_year_reg_long, aes(x = `SIGNED FY`, y = values, color = projects)) + 
  geom_line() +
  facet_wrap(~REGION)

test_pct_year_nf_long <- test_pct_year_nf %>%
  dplyr::select(`SIGNED FY`,FOREST_ID, pct_spec_use, pct_for_prod, pct_rec, 
                pct_haz_fuel, pct_wlife, pct_geo, pct_veg_mngt) %>%
  pivot_longer(!c(`SIGNED FY`, FOREST_ID), names_to = "projects", values_to = "values")

test_pct_year_nf_long %>% 
  filter(str_detect(FOREST_ID, "^01")) %>%
  ggplot(aes(x = `SIGNED FY`, y = values, color = projects)) + 
  geom_line() +
  facet_wrap(~FOREST_ID) + 
  theme(legend.position = "None")

test_pct_year_nf_long %>% 
  filter(str_detect(FOREST_ID, "^02")) %>%
  ggplot(aes(x = `SIGNED FY`, y = values, color = projects)) + 
  geom_line() +
  facet_wrap(~FOREST_ID) + 
  theme(legend.position = "None")

test_pct_year_nf_long %>% 
  filter(str_detect(FOREST_ID, "^03")) %>%
  ggplot(aes(x = `SIGNED FY`, y = values, color = projects)) + 
  geom_line() +
  facet_wrap(~FOREST_ID) + 
  theme(legend.position = "None")

test_pct_year_nf_long %>% 
  filter(str_detect(FOREST_ID, "^04")) %>%
  ggplot(aes(x = `SIGNED FY`, y = values, color = projects)) + 
  geom_line() +
  facet_wrap(~FOREST_ID) + 
  theme(legend.position = "None")

test_pct_year_nf_long %>% 
  filter(str_detect(FOREST_ID, "^05")) %>%
  ggplot(aes(x = `SIGNED FY`, y = values, color = projects)) + 
  geom_line() +
  facet_wrap(~FOREST_ID) + 
  theme(legend.position = "None")

test_pct_year_nf_long %>% 
  filter(str_detect(FOREST_ID, "^06")) %>%
  ggplot(aes(x = `SIGNED FY`, y = values, color = projects)) + 
  geom_line() +
  facet_wrap(~FOREST_ID) + 
  theme(legend.position = "None")

test_pct_year_nf_long %>% 
  filter(str_detect(FOREST_ID, "^08")) %>%
  ggplot(aes(x = `SIGNED FY`, y = values, color = projects)) + 
  geom_line() +
  facet_wrap(~FOREST_ID) + 
  theme(legend.position = "None")

test_pct_year_nf_long %>% 
  filter(str_detect(FOREST_ID, "^09")) %>%
  ggplot(aes(x = `SIGNED FY`, y = values, color = projects)) + 
  geom_line() +
  facet_wrap(~FOREST_ID) + 
  theme(legend.position = "None")


# I want to see how this may correlate to heterogeneity metrics
#-------------------------------------------------------------------------------

test_join <- right_join(test_pct_year_nf, nepa_summ_df, by = c("FOREST_ID" = "forest_num"))

dom_arch_labels <- nepa_summ_df %>%
  dplyr::select(forest_num, dom_archetype)
pct_dom_arch_labels <- nepa_summ_df %>%
  dplyr::select(forest_num, pct_area_dom_arch)
shan_norm_labels <- nepa_summ_df %>%
  dplyr::select(forest_num, shan_diverse_norm)
ent_labels <- nepa_summ_df %>%
  dplyr::select(forest_num, entropy_all)

#create a label column
test_join$label_content <- paste0(
  "<span style = 'font-size:8pt'><b>Dom. Arch.:</b> ", test_join$dom_archetype, "<br>",
  "<b>% Arch:</b> ", test_join$pct_area_dom_arch, "<br></span>"
  #"<b>Shan diverse:</b> ", test_join$shan_diverse_norm, "<br>",
  #"<b>Entropy:</b>", test_join$entropy_all, "<br>"
)

test_join %>%
  filter(str_detect(FOREST_ID, "^09")) %>%
  ggplot(aes(label = label_content)) + 
  geom_line(aes(x = `SIGNED FY`, y = pct_spec_use)) + 
  geom_line(aes(x = `SIGNED FY`, y = pct_for_prod), color = "green4") + 
  geom_line(aes(x = `SIGNED FY`, y = pct_rec), color = "blue3") + 
  geom_line(aes(x = `SIGNED FY`, y = pct_haz_fuel), color = "orange3") +
  geom_richtext(aes(x = 2012, y = 75), hjust = 1) + 
  theme_bw() + 
  facet_wrap(~FOREST_ID)

test_join %>%
  filter(str_detect(FOREST_ID, "^05")) %>%
  ggplot(aes(label = label_content)) + 
  geom_line(aes(x = `SIGNED FY`, y = pct_spec_use)) + 
  geom_line(aes(x = `SIGNED FY`, y = pct_for_prod), color = "green4") + 
  geom_line(aes(x = `SIGNED FY`, y = pct_rec), color = "blue3") + 
  geom_line(aes(x = `SIGNED FY`, y = pct_haz_fuel), color = "orange3") +
  geom_richtext(aes(x = 2012, y = 75), hjust = 1) + 
  theme_bw() + 
  facet_wrap(~FOREST_ID)

test_join %>%
  group_by(FOREST_ID) %>%
  summarise(mean_ann_sup = mean(pct_spec_use), 
            dom_archetype = mean(dom_archetype)) %>%
  ggplot() + 
#  geom_point(aes(x = dom_archetype, y = mean_ann_sup)) + 
  geom_boxplot(aes(as.factor(dom_archetype), mean_ann_sup))

test_join %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(dom_archetype), pct_spec_use), notch = TRUE)

test_join %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(dom_archetype), pct_for_prod), notch = TRUE)

test_join %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(dom_archetype), pct_rec), notch = TRUE)

test_join %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(dom_archetype), pct_haz_fuel), notch = TRUE)

test_join %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(region), pct_haz_fuel), notch = TRUE)

test_join %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(region), pct_spec_use), notch = TRUE)

