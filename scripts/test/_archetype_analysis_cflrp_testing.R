library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(exactextractr)
library(tigris)
library(MetBrewer)
library(dgof)
library(emmeans)


# Load the CFLRP data
cflrp <- st_read(here::here("data/original/Actv_CFLRP_PL/S_USA.Actv_CFLRP_PL.shp"))

# Load the previously made NEPA sums
nepa_summ_df <- read_csv(here::here("outputs/tables/nf_nepa_projs_arch_summ_2025-09-11.csv"))

# Load the USFS boundaries
fs_nf <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeForest.shp")
fs_reg <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeRegion.shp")

projection <- "epsg: 5070"

cflrp_acres <- cflrp %>%
  filter(REGION_COD != "10") %>% # take out region 10
  st_drop_geometry() %>% # then transform the data to the ESPG - 5070
  filter(UOM == "ACRES") %>% # we only want treatments with a unit of measure of acres
  filter(FY_PLANNED >= 2009) %>% # we only want a specific date range
  mutate(forest_num = paste0(REGION_COD, ADMIN_FORE)) %>% # we need to get the forest number by combining region_cod and forest_cod
  mutate(nepa_not_reqd = case_when(NEPA_PROJE == "NOT_REQD" ~ 1,
                                   NEPA_PROJE == "Not Requir" ~ 1
                   )) %>%
  group_by(forest_num) %>% # then groupby by forest
  summarise(mean_cflrp_acres = mean(NBR_UNITS1), 
            med_cflrp_acres = median(NBR_UNITS1), 
            total_cflrp_acres = sum(NBR_UNITS1),
            cflrp_nepa_not_reqd = sum(nepa_not_reqd, na.rm = TRUE),
            cflrp_nepa = length(unique(NEPA_PROJE)) - cflrp_nepa_not_reqd) # then get the mean, median, and total acres treated. I also want the total number of projects (NEPA) or NOT REQD
  

cflrp_er <- cflrp %>%
  filter(REGION_COD != "10") %>% # take out region 10
  filter(NEPA_PROJE == "EMERG RESP") %>%
  st_drop_geometry() %>% # then transform the data to the ESPG - 5070
  filter(UOM == "ACRES") %>% # we only want treatments with a unit of measure of acres
  filter(FY_PLANNED >= 2009) %>% # we only want a specific date range
  mutate(forest_num = paste0(REGION_COD, ADMIN_FORE)) %>% # we need to get the forest number by combining region_cod and forest_cod
  mutate(nepa_not_reqd = case_when(NEPA_PROJE == "NOT_REQD" ~ 1,
                                   NEPA_PROJE == "Not Requir" ~ 1
  )) %>%
  group_by(forest_num) %>% # then groupby by forest
  summarise(mean_cflrp_acres = mean(NBR_UNITS1), 
            med_cflrp_acres = median(NBR_UNITS1), 
            total_cflrp_acres = sum(NBR_UNITS1),
            cflrp_nepa_not_reqd = sum(nepa_not_reqd, na.rm = TRUE),
            cflrp_nepa = length(unique(NEPA_PROJE)) - cflrp_nepa_not_reqd) 

forests_with_er <- cflrp_er$forest_num

cflrp_er_df <- right_join(nepa_summ_df, cflrp_er)

# join to the nepa_summ_df
test_join <- left_join(nepa_summ_df, cflrp_acres)

test_join <- test_join %>%
  filter(forest_num != "0836") %>%
  mutate(total_cflrp_acres = replace_na(total_cflrp_acres, 0), 
         med_cflrp_acres = replace_na(med_cflrp_acres, 0),
         mean_cflrp_acres = replace_na(mean_cflrp_acres, 0))

cflrp_er_df <- test_join %>%
  filter(forest_num %in% forests_with_er)

cflrp_only <- test_join %>%
  filter(med_cflrp_acres >=0.000001)

ggplot(test_join, aes(total_cflrp_acres)) +
  geom_histogram(bins = 100) + 
  facet_wrap(~region)


for (i in unique(test_join$region)) {
  print(i)
  #print("Region: " + as.character(i))
  print(cor(x = test_join$pct_area_dom_arch[test_join$region == i], y = test_join$total_cflrp_acres[test_join$region == i], use = "complete.obs"))
}

for (i in unique(cflrp_only$region)) {
  print(i)
  #print("Region: " + as.character(i))
  print(cor(x = cflrp_only$pct_area_dom_arch[cflrp_only$region == i], y = cflrp_only$total_cflrp_acres[cflrp_only$region == i], use = "complete.obs"))
}


ggplot(test_join, aes(x = entropy_all, y = total_cflrp_acres, color = region)) +
  geom_point()

ggplot(test_join, aes(x = shan_diverse_norm, y = total_cflrp_acres, color = region)) +
  geom_point()

ggplot(test_join, aes(x = pct_area_dom_arch, y = total_cflrp_acres, color = region)) +
  geom_point()

ggplot(test_join, aes(x = entropy_all, y = med_cflrp_acres, color = region)) +
  geom_point()

ggplot(test_join, aes(x = shan_diverse_norm, y = med_cflrp_acres, color = region)) +
  geom_point()

ggplot(test_join, aes(x = pct_area_dom_arch, y = med_cflrp_acres, color = region)) +
  geom_point()

ggplot(cflrp_only, aes(x = pct_area_dom_arch, y = med_cflrp_acres, color = region)) +
  geom_point() + 
  geom_hline(yintercept = sd(cflrp_only$med_cflrp_acres)) +
  geom_hline(yintercept = sd(cflrp_only$med_cflrp_acres)*2, linetype = "dashed")

ggplot(test_join, aes(x = pct_area_dom_arch, y = med_cflrp_acres, color = region)) +
  geom_point() + 
  geom_hline(yintercept = sd(test_join$med_cflrp_acres)) +
  geom_hline(yintercept = sd(test_join$med_cflrp_acres)*2, linetype = "dashed")

ggplot(test_join, aes(x = shan_diverse, y = med_cflrp_acres, color = region)) +
  geom_point() + 
  geom_hline(yintercept = sd(test_join$med_cflrp_acres)) +
  geom_hline(yintercept = sd(test_join$med_cflrp_acres)*2, linetype = "dashed")

ggplot() +
  geom_point(data = test_join, aes(x = entropy_all, y = med_cflrp_acres, color = region)) + 
  geom_point(data = cflrp_er_df, aes(x = entropy_all, y = med_cflrp_acres), color = "black") +
  geom_label(data = cflrp_er_df, aes(x = entropy_all, y = med_cflrp_acres, label = forest_num), vjust = 0.7, hjust = 1.25) +
  #gghighlight(forest_num %in% forests_with_er) +
  geom_hline(yintercept = sd(test_join$med_cflrp_acres)) +
  geom_hline(yintercept = sd(test_join$med_cflrp_acres)*2, linetype = "dashed")

hist(test_join$med_cflrp_acres)
hist(cflrp_only$med_cflrp_acres)


cflrp_2009 <- cflrp %>%
  filter(REGION_COD != "10") %>% # take out region 10
  st_drop_geometry() %>% # then transform the data to the ESPG - 5070
  filter(UOM == "ACRES") %>% # we only want treatments with a unit of measure of acres
  filter(FY_PLANNED >= 2009) %>%
  filter(NBR_UNITS1 > 1)


ggplot(cflrp_2009, aes(NBR_UNITS1)) + 
  geom_histogram(bins = 200)




# Simple CFLRP yes/no EDA
#------------------------------------------------

# in test_join add a column with a 1 or 0 for whether a forest has a cflrp or not

test_join <- test_join %>%
  mutate(clfrp_ = case_when(total_cflrp_acres == 0 ~ 0, 
                            total_cflrp_acres > 0 ~ 1))


ggplot(test_join, aes(x = clfrp_, y = tot_appeal)) + 
  geom_point()

ggplot(test_join, aes(x = clfrp_, y = tot_lit)) + 
  geom_point()

ggplot(test_join, aes(x = clfrp_, y = pct_area_dom_arch)) + 
  geom_point()

ggplot(test_join, aes(x = clfrp_, y = shan_diverse_norm)) + 
  geom_point()

ggplot(test_join, aes(x = clfrp_, y = entropy_all)) + 
  geom_point()

ggplot(test_join, aes(x = clfrp_, y = entropy_eco)) + 
  geom_point()

ggplot(test_join, aes(x = clfrp_, y = entropy_soc)) + 
  geom_point()

ggplot(test_join, aes(x = clfrp_, y = yearly_pct_med_EA_EIS)) + 
  geom_point() + 
  facet_wrap(~region)

ggplot(test_join, aes(x = clfrp_, y = yearly_mean_projs)) + 
  geom_point() 

ggplot(test_join, aes(x = clfrp_, y = yearly_med_projs)) + 
  geom_point() + 
  facet_wrap(~region)

ggplot(test_join, aes(x = clfrp_, y = yearly_med_CE)) + 
  geom_point() + 
  facet_wrap(~region)

# regression
#---------------------------

m1 <- glm( cbind(clfrp_, 1-clfrp_) ~ tot_appeal, data = test_join, 
           family = binomial)
summary(m1)

yhat.df <- emmeans(m1, ~ pct_area_dom_arch, at = list(pct_area_dom_arch = seq(0,5,by=.01)), type='response') %>%
  as.data.frame()

ggplot(test_join, aes(x = pct_area_dom_arch)) +
  geom_ribbon(data=yhat.df, aes(ymin = asymp.LCL, ymax = asymp.UCL), fill='salmon', alpha=.4) +
  geom_line(data = yhat.df, aes(y=prob), color='red') +
  geom_point(aes(y = clfrp_)) +
  labs(y='Probability of CFLRP', x = '% Area Dominant Arch.', title = 'Heterogeneity and CFLRP')

# compare distributions
#------------------------------

ggplot(test_join, aes(x = shan_diverse_norm, fill = as.factor(clfrp_))) +
  geom_histogram(alpha=0.6, position = 'identity', bins = 30) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_bw() +
  labs(fill="")

ggplot(test_join, aes(x = entropy_all, fill = as.factor(clfrp_))) +
  geom_histogram(alpha=0.6, position = 'identity', bins = 30) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_bw() +
  labs(fill="")

ggplot(test_join, aes(x = entropy_eco, fill = as.factor(clfrp_))) +
  geom_histogram(alpha=0.6, position = 'identity', bins = 30) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_bw() +
  labs(fill="")

ggplot(test_join, aes(x = entropy_soc, fill = as.factor(clfrp_))) +
  geom_histogram(alpha=0.6, position = 'identity', bins = 30) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_bw() +
  labs(fill="")

ggplot(test_join, aes(x = pct_area_dom_arch, fill = as.factor(clfrp_))) +
  geom_histogram(alpha=0.6, position = 'identity', bins = 30) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_bw() +
  labs(fill="")

# simple 2-sided K-S test - nope

yes_cflrp <- test_join %>%
  filter(total_cflrp_acres > 0) %>%
  dplyr::select(shan_diverse_norm, entropy_all, pct_area_dom_arch)
no_cflrp <- test_join %>%
  filter(total_cflrp_acres == 0)  %>%
  dplyr::select(shan_diverse_norm, entropy_all, pct_area_dom_arch)

ks.test(yes_cflrp$entropy_all, yes_cflrp$entropy_all)

# simple ANOVA

shan_div_aov <- aov(test_join$shan_diverse_norm ~ factor(test_join$clfrp_))
summary(shan_div_aov)

shan_div_aov2 <- aov(test_join$shan_diverse_norm ~ factor(test_join$clfrp_) * factor(test_join$dom_archetype))
summary(shan_div_aov2)

ent_all_aov <- aov(test_join$entropy_all ~ factor(test_join$clfrp_))
summary(ent_all_aov)

pct_dom_arch_aov <-  aov(test_join$pct_area_dom_arch ~ factor(test_join$clfrp_))
summary(pct_dom_arch_aov)

pct_dom_arch_aov2 <-  aov(test_join$pct_area_dom_arch ~ factor(test_join$dom_archetype))
summary(pct_dom_arch_aov2)

ent_all_aov2 <- aov(test_join$entropy_all ~ factor(test_join$dom_archetype))
summary(ent_all_aov2)

shan_div_aov3 <- aov(test_join$shan_diverse_norm ~ factor(test_join$dom_archetype))
summary(shan_div_aov3)
