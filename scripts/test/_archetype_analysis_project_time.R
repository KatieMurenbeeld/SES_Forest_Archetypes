library(tidyverse)
library(stringr)
library(terra)
library(sf)
library(ggplot2)
library(exactextractr)
library(tigris)
library(MetBrewer)
library(ggtext)
library(lubridate)
library(car)
library(rstatix)
library(PMCMRplus)

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

test_pct_year_reg <- pals_df_test %>%
  group_by(REGION, `SIGNED FY`) %>%
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

# plot some line plots

test_pct_year_long <- test_pct_year %>%
  dplyr::select(`SIGNED FY`, pct_for_prod, pct_rec, 
                pct_haz_fuel, pct_wlife, pct_geo, pct_veg_mngt, pct_water) %>%
  pivot_longer(!`SIGNED FY`, names_to = "projects", values_to = "values")

ggplot(data = test_pct_year_long, aes(x = `SIGNED FY`, y = values, color = projects)) + 
  geom_line() + 
  geom_smooth(method = "lm") + 
  theme_minimal()
  
test_pct_year_reg_long <- test_pct_year_reg %>%
  dplyr::select(`SIGNED FY`, REGION, pct_for_prod, pct_rec, 
                pct_haz_fuel, pct_wlife, pct_geo, pct_veg_mngt, pct_water) %>%
  pivot_longer(!c(`SIGNED FY`,REGION), names_to = "projects", values_to = "values")

ggplot(data = test_pct_year_reg_long, aes(x = `SIGNED FY`, y = values, color = projects)) + 
  geom_line() +
  facet_wrap(~REGION)

test_pct_year_nf_long <- test_pct_year_nf %>%
  dplyr::select(`SIGNED FY`,FOREST_ID, pct_spec_use, pct_for_prod, pct_rec, 
                pct_haz_fuel, pct_wlife, pct_geo, pct_veg_mngt, pct_water) %>%
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

# NF of North Carolina (decrease in hazardous fuels and rec, increase in 
# veg management and water)
test_pct_year_nf_long %>% 
  filter(str_detect(FOREST_ID, "0811")) %>%
  ggplot(aes(x = `SIGNED FY`, y = values, color = projects)) + 
  geom_line() +
  geom_smooth(method = "lm") +
  theme_minimal()

# Tonto NF (increase in geo -Resolution Copper Mine- and small increase in rec)
test_pct_year_nf_long %>% 
  filter(str_detect(FOREST_ID, "0312")) %>%
  ggplot(aes(x = `SIGNED FY`, y = values, color = projects)) + 
  geom_line() +
  geom_smooth(method = "lm") +
  theme_minimal()


# I want to see how this may correlate to heterogeneity metrics
#-------------------------------------------------------------------------------

test_join <- right_join(test_pct_year_nf, nepa_summ_df, by = c("FOREST_ID" = "forest_num"))

test_join_long <- test_join %>%
  dplyr::select(`SIGNED FY`,FOREST_ID, pct_for_prod, pct_rec, 
                pct_haz_fuel, pct_wlife, pct_veg_mngt, pct_water, dom_archetype) %>%
  pivot_longer(!c(`SIGNED FY`, FOREST_ID, dom_archetype), names_to = "projects", values_to = "values")

test_join_long %>% 
  group_by(`SIGNED FY`, dom_archetype, projects) %>%
  summarize(values = mean(values)) %>%
  ggplot(aes(x = `SIGNED FY`, y = values, color = projects)) + 
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~dom_archetype) + 
  theme_minimal()

test_join_long %>% 
  group_by(`SIGNED FY`, dom_archetype, projects) %>%
  summarize(stdev = sd(values)) %>%
  ggplot(aes(x = `SIGNED FY`, y = stdev, color = projects)) + 
  geom_line() +
  facet_wrap(~dom_archetype)

test_join_long %>% 
  group_by(`SIGNED FY`, dom_archetype, projects) %>%
  summarize(med_values = median(values)) %>%
  ggplot(aes(x = `SIGNED FY`, y = med_values, color = projects)) + 
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~dom_archetype) + 
  theme_minimal()

test_join_long %>% 
  group_by(`SIGNED FY`, dom_archetype, projects) %>%
  summarize(range_values = max(values) - min(values)) %>%
  ggplot(aes(x = `SIGNED FY`, y = range_values, color = projects)) + 
  geom_line() +
  facet_wrap(~dom_archetype)

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

test_join %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(region), pct_haz_fuel), notch = TRUE)

test_join %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(region), pct_spec_use), notch = TRUE)


# ANOVA for Cory
#--------------------------------------------------------------
test_join <- test_join %>%
  filter(!FOREST_ID == "0836") %>%
  mutate(dom_archetype = as.factor(dom_archetype))

test_join_for_prod <- test_join %>%
  filter(!dom_archetype == "1")

## ANOVA for pct_for_prod
### Have to remove Archetype A because it has 0 variance for pct_for_prod
aov1 <-  aov(test_join_for_prod$pct_for_prod ~ factor(test_join_for_prod$dom_archetype))
summary(aov1)

ggplot(data=test_join_for_prod, aes(x=pct_for_prod, group=dom_archetype, fill= as.factor(dom_archetype))) +
  geom_density(adjust=1.5, alpha=.7) +
  scale_fill_manual(values = custom_palette) +
  theme_minimal()

par(mfrow = c(1, 2)) # combine plots

# histogram
hist(aov1$residuals)

# QQ-plot
qqPlot(aov1$residuals,
       id = FALSE # id = FALSE to remove point identification
)

# Test for equal variance
leveneTest(pct_for_prod ~ as.factor(dom_archetype),
           data = test_join_for_prod
)
#tapply(test_join_for_prod$pct_, test_join_for_prod$dom_archetype, var)

# unequal variance - use Welch ANOVA
oneway.test(pct_for_prod ~ as.factor(dom_archetype), data = test_join_for_prod, var.equal = FALSE)

# use Games-Howell Post-Hoc test to see about between group
gamesHowellTest(pct_for_prod ~ dom_archetype, test_join_for_prod)

## ANOVA for pct_geo
aov2 <-  aov(test_join$pct_geo ~ factor(test_join$dom_archetype))
summary(aov2)

ggplot(data=test_join, aes(x=pct_geo, group=dom_archetype, fill= as.factor(dom_archetype))) +
  geom_density(adjust=1.5, alpha=.7) +
  scale_fill_manual(values = custom_palette) +
  theme_minimal()

par(mfrow = c(1, 2)) # combine plots

# histogram
hist(aov2$residuals)

# QQ-plot
qqPlot(aov2$residuals,
       id = FALSE # id = FALSE to remove point identification
)

leveneTest(pct_geo ~ as.factor(dom_archetype),
           data = test_join)
oneway.test(pct_geo ~ as.factor(dom_archetype), data = test_join, var.equal = FALSE)

gamesHowellTest(pct_geo ~ dom_archetype, test_join)

## ANOVA for pct_water
aov3 <-  aov(test_join$pct_water ~ factor(test_join$dom_archetype))
summary(aov3)

ggplot(data=test_join, aes(x=pct_water, group=dom_archetype, fill= as.factor(dom_archetype))) +
  geom_density(adjust=1.5, alpha=.7) +
  scale_fill_manual(values = custom_palette) +
  theme_minimal()

par(mfrow = c(1, 2)) # combine plots

# histogram
hist(aov3$residuals)

# QQ-plot
qqPlot(aov3$residuals,
       id = FALSE # id = FALSE to remove point identification
)

leveneTest(pct_water ~ as.factor(dom_archetype),
           data = test_join)
tapply(test_join$pct_water, test_join$dom_archetype, var)
# Fairly equal varianve

oneway.test(pct_water ~ as.factor(dom_archetype), data = test_join, var.equal = TRUE)

tukeyTest(pct_water ~ dom_archetype, test_join)

## ANOVA for pct_haz_fuel
aov4 <-  aov(test_join$pct_haz_fuel ~ factor(test_join$dom_archetype))
summary(aov4)

ggplot(data=test_join, aes(x=pct_haz_fuel, group=dom_archetype, fill= as.factor(dom_archetype))) +
  geom_density(adjust=1.5, alpha=.7) +
  scale_fill_manual(values = custom_palette) +
  theme_minimal()

par(mfrow = c(1, 2)) # combine plots

# histogram
hist(aov4$residuals)

# QQ-plot
qqPlot(aov4$residuals,
       id = FALSE # id = FALSE to remove point identification
)

leveneTest(pct_haz_fuel ~ as.factor(dom_archetype),
           data = test_join)
tapply(test_join$pct_haz_fuel, test_join$dom_archetype, var)

oneway.test(pct_geo ~ as.factor(dom_archetype), data = test_join, var.equal = FALSE)

gamesHowellTest(pct_geo ~ dom_archetype, test_join)

## ANOVA for pct_wlife
aov5 <-  aov(test_join$pct_wlife ~ factor(test_join$dom_archetype))
summary(aov5)

ggplot(data=test_join, aes(x=pct_wlife, group=dom_archetype, fill= as.factor(dom_archetype))) +
  geom_density(adjust=1.5, alpha=.7) +
  scale_fill_manual(values = custom_palette) +
  theme_minimal()

par(mfrow = c(1, 2)) # combine plots

# histogram
hist(aov5$residuals)

# QQ-plot
qqPlot(aov5$residuals,
       id = FALSE # id = FALSE to remove point identification
)

leveneTest(pct_wlife ~ as.factor(dom_archetype),
           data = test_join)
tapply(test_join$pct_wlife, test_join$dom_archetype, var)

oneway.test(pct_wlife ~ as.factor(dom_archetype), data = test_join, var.equal = FALSE)

gamesHowellTest(pct_wlife ~ dom_archetype, test_join)

## ANOVA for pct_rec
aov6 <-  aov(test_join$pct_rec ~ factor(test_join$dom_archetype))
summary(aov6)

ggplot(data=test_join, aes(x=pct_rec, group=dom_archetype, fill= as.factor(dom_archetype))) +
  geom_density(adjust=1.5, alpha=.7) +
  scale_fill_manual(values = custom_palette) +
  theme_minimal()

par(mfrow = c(1, 2)) # combine plots

# histogram
hist(aov6$residuals)

# QQ-plot
qqPlot(aov6$residuals,
       id = FALSE # id = FALSE to remove point identification
)

leveneTest(pct_rec ~ as.factor(dom_archetype),
           data = test_join)
tapply(test_join$pct_rec, test_join$dom_archetype, var)

oneway.test(pct_rec ~ as.factor(dom_archetype), data = test_join, var.equal = FALSE)

gamesHowellTest(pct_rec ~ dom_archetype, test_join)

## ANOVA for pct_veg_mngt
aov7 <-  aov(test_join$pct_veg_mngt ~ factor(test_join$dom_archetype))
summary(aov7)

ggplot(data=test_join, aes(x=pct_veg_mngt, group=dom_archetype, fill= as.factor(dom_archetype))) +
  geom_density(adjust=1.5, alpha=.7) +
  scale_fill_manual(values = custom_palette) +
  theme_minimal()

par(mfrow = c(1, 2)) # combine plots

# histogram
hist(aov7$residuals)

# QQ-plot
qqPlot(aov7$residuals,
       id = FALSE # id = FALSE to remove point identification
)

leveneTest(pct_veg_mngt ~ as.factor(dom_archetype),
           data = test_join)
tapply(test_join$pct_veg_mngt, test_join$dom_archetype, var)

oneway.test(pct_veg_mngt ~ as.factor(dom_archetype), data = test_join, var.equal = FALSE)

gh_veg_mngt <- gamesHowellTest(pct_veg_mngt ~ dom_archetype, test_join)
summary(gh_veg_mngt)
summaryGroup(gh_veg_mngt)

## ANOVA for pct_spec_use
aov8 <-  aov(test_join$pct_spec_use ~ factor(test_join$dom_archetype))
summary(aov8)

ggplot(data=test_join, aes(x=pct_spec_use, group=dom_archetype, fill= as.factor(dom_archetype))) +
  geom_density(adjust=1.5, alpha=.7) +
  scale_fill_manual(values = custom_palette) +
  theme_minimal()

par(mfrow = c(1, 2)) # combine plots

# histogram
hist(aov8$residuals)

# QQ-plot
qqPlot(aov8$residuals,
       id = FALSE # id = FALSE to remove point identification
)

leveneTest(pct_spec_use ~ as.factor(dom_archetype),
           data = test_join)
tapply(test_join$pct_spec_use, test_join$dom_archetype, var)

oneway.test(pct_spec_use ~ as.factor(dom_archetype), data = test_join, var.equal = TRUE)

tukeyTest(pct_spec_use ~ dom_archetype, test_join)

# Just realized unbalanced data and mostly non normal distribution of residuals
# Will do Kruskal-Wallis test
#---------------------------------------------------------------------
test_join %>%
  group_by(dom_archetype) %>%
  summarise(
    count = n(),
    mean = mean(pct_spec_use, na.rm = TRUE),
    sd = sd(pct_spec_use, na.rm = TRUE),
    median = median(pct_spec_use, na.rm = TRUE),
    IQR = IQR(pct_spec_use, na.rm = TRUE)
  )

test_join %>%
  group_by(dom_archetype) %>%
  summarise(
    count = n(),
    mean = mean(pct_for_prod, na.rm = TRUE),
    sd = sd(pct_for_prod, na.rm = TRUE),
    median = median(pct_for_prod, na.rm = TRUE),
    IQR = IQR(pct_for_prod, na.rm = TRUE)
  )

test_join %>%
  group_by(dom_archetype) %>%
  summarise(
    count = n(),
    mean = mean(pct_rec, na.rm = TRUE),
    sd = sd(pct_rec, na.rm = TRUE),
    median = median(pct_rec, na.rm = TRUE),
    IQR = IQR(pct_rec, na.rm = TRUE)
  )

test_join %>%
  group_by(dom_archetype) %>%
  summarise(
    count = n(),
    mean = mean(pct_haz_fuel, na.rm = TRUE),
    sd = sd(pct_haz_fuel, na.rm = TRUE),
    median = median(pct_haz_fuel, na.rm = TRUE),
    IQR = IQR(pct_haz_fuel, na.rm = TRUE)
  )

test_join %>%
  group_by(dom_archetype) %>%
  summarise(
    count = n(),
    mean = mean(pct_wlife, na.rm = TRUE),
    sd = sd(pct_wlife, na.rm = TRUE),
    median = median(pct_wlife, na.rm = TRUE),
    IQR = IQR(pct_wlife, na.rm = TRUE)
  )

test_join %>%
  group_by(dom_archetype) %>%
  summarise(
    count = n(),
    mean = mean(pct_geo, na.rm = TRUE),
    sd = sd(pct_geo, na.rm = TRUE),
    median = median(pct_geo, na.rm = TRUE),
    IQR = IQR(pct_geo, na.rm = TRUE)
  )

test_join %>%
  group_by(dom_archetype) %>%
  summarise(
    count = n(),
    mean = mean(pct_water, na.rm = TRUE),
    sd = sd(pct_water, na.rm = TRUE),
    median = median(pct_water, na.rm = TRUE),
    IQR = IQR(pct_water, na.rm = TRUE)
  )

test_join %>%
  group_by(dom_archetype) %>%
  summarise(
    count = n(),
    mean = mean(pct_veg_mngt, na.rm = TRUE),
    sd = sd(pct_veg_mngt, na.rm = TRUE),
    median = median(pct_veg_mngt, na.rm = TRUE),
    IQR = IQR(pct_veg_mngt, na.rm = TRUE)
  )

kruskal.test(pct_spec_use ~ dom_archetype, data = test_join)

kruskal.test(pct_for_prod ~ dom_archetype, data = test_join)
pairwise.wilcox.test(test_join$pct_for_prod, test_join$dom_archetype,
                     p.adjust.method = "BH")

kruskal.test(pct_rec ~ dom_archetype, data = test_join)
pairwise.wilcox.test(test_join$pct_rec, test_join$dom_archetype,
                     p.adjust.method = "BH")

kruskal.test(pct_haz_fuel ~ dom_archetype, data = test_join)
pairwise.wilcox.test(test_join$pct_haz_fuel, test_join$dom_archetype,
                     p.adjust.method = "BH")

kruskal.test(pct_wlife ~ dom_archetype, data = test_join)
pairwise.wilcox.test(test_join$pct_wlife, test_join$dom_archetype,
                     p.adjust.method = "BH")

kruskal.test(pct_geo ~ dom_archetype, data = test_join)
pairwise.wilcox.test(test_join$pct_geo, test_join$dom_archetype,
                     p.adjust.method = "BH")

kruskal.test(pct_water ~ dom_archetype, data = test_join)

kruskal.test(pct_veg_mngt ~ dom_archetype, data = test_join)
pairwise.wilcox.test(test_join$pct_veg_mngt, test_join$dom_archetype,
                     p.adjust.method = "BH")



# Look at special uses by region
#----------------------------------------

sup_df <- pals_df %>%
  filter(as.Date(`INITIATION DATE`, format = "%m/%d/%Y") >= "2009-01-01") %>%
  filter(`SU Special use management – purpose` == 1)

sup_df_join <- right_join(sup_df, nepa_summ_df, by = c("FOREST_ID" = "forest_num"))

region_list <- c("01", "02", "03", "04", "05", "06", "08", "09")
arch_list <- c(1:6)


region_sup_df <- data.frame(REGION_ID = character(), 
                            `PROJECT NAME` = character(),
                            FOREST_ID = numeric(),
                            forest_name = character(),
                            `DECISION SIGNED` = character(),
                            dom_archetype = numeric())

for (i in region_list) {
  # Generate some data in each iteration
  new_row <- sup_df_join %>%
    dplyr::select(REGION_ID, `PROJECT NAME`, FOREST_ID, forest_name, `DECISION SIGNED`, dom_archetype) %>%
    filter(REGION_ID == i) %>%
    arrange(desc(as.Date(`DECISION SIGNED`, tryFormats = c("%m/%d/%Y")))) %>%
    head(., 10)
  new_row2 <- sup_df_join %>%
    dplyr::select(REGION_ID, `PROJECT NAME`, FOREST_ID, forest_name, `DECISION SIGNED`, dom_archetype) %>%
    filter(REGION_ID == i) %>%
    arrange(as.Date(`DECISION SIGNED`, tryFormats = c("%m/%d/%Y"))) %>%
    head(., 10)
  # Append the new row using rbind()
  region_sup_df <- rbind(region_sup_df, new_row, new_row2)
}

region_sup_df <- region_sup_df %>%
  mutate(arche = case_when(dom_archetype == 1 ~ "A",
                           dom_archetype == 2 ~ "B",
                           dom_archetype == 3 ~ "C",
                           dom_archetype == 4 ~ "D",
                           dom_archetype == 5 ~ "E",
                           dom_archetype == 6 ~ "F")) %>%
  mutate(arch_name = case_when(dom_archetype == 1 ~ "Private land-dominated plains",
                               dom_archetype == 2 ~ "Productive forests near urbanized areas",
                               dom_archetype == 3 ~ "Old forests in rural areas",
                               dom_archetype == 4 ~ "Forest-adjacent systems",
                               dom_archetype == 5 ~ "Urban non-forested",
                               dom_archetype == 6 ~ "Mountain forests and shrublands"))

write_csv(region_sup_df, file = here::here(paste0("outputs/tables/special_use_by_region_", Sys.Date(), ".csv")))

arch_sup_df <- data.frame(REGION_ID = character(), 
                            `PROJECT NAME` = character(),
                            FOREST_ID = numeric(),
                            forest_name = character(),
                            `DECISION SIGNED` = character(),
                            dom_archetype = numeric())

for (i in arch_list) {
  # Generate some data in each iteration
  new_row <- sup_df_join %>%
    dplyr::select(REGION_ID, `PROJECT NAME`, FOREST_ID, forest_name, `DECISION SIGNED`, dom_archetype) %>%
    filter(dom_archetype == i) %>%
    arrange(desc(as.Date(`DECISION SIGNED`, tryFormats = c("%m/%d/%Y")))) %>%
    head(., 10)
  new_row2 <- sup_df_join %>%
    dplyr::select(REGION_ID, `PROJECT NAME`, FOREST_ID, forest_name, `DECISION SIGNED`, dom_archetype) %>%
    filter(dom_archetype == i) %>%
    arrange(as.Date(`DECISION SIGNED`, tryFormats = c("%m/%d/%Y"))) %>%
    head(., 10)
  # Append the new row using rbind()
  arch_sup_df <- rbind(arch_sup_df, new_row, new_row2)
}

arch_sup_df <- arch_sup_df %>%
  mutate(arch = case_when(dom_archetype == 1 ~ "A",
                           dom_archetype == 2 ~ "B",
                           dom_archetype == 3 ~ "C",
                           dom_archetype == 4 ~ "D",
                           dom_archetype == 5 ~ "E",
                           dom_archetype == 6 ~ "F")) %>%
  mutate(arch_name = case_when(dom_archetype == 1 ~ "Private land-dominated plains",
                           dom_archetype == 2 ~ "Productive forests near urbanized areas",
                           dom_archetype == 3 ~ "Old forests in rural areas",
                           dom_archetype == 4 ~ "Forest-adjacent systems",
                           dom_archetype == 5 ~ "Urban non-forested",
                           dom_archetype == 6 ~ "Mountain forests and shrublands"))

write_csv(arch_sup_df, file = here::here(paste0("outputs/tables/special_use_by_dom_archetype_", Sys.Date(), ".csv")))

