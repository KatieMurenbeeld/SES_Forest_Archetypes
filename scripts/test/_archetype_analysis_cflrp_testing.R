library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(exactextractr)
library(tigris)
library(MetBrewer)

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
