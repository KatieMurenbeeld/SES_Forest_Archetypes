library(tidyverse)
library(ggplot2)
library(MetBrewer)

# load the data

df <- read_csv(here::here("outputs/nfbuffers_div_ent_nf_2025-08-28.csv")) 
df_year <- read_csv(here::here("outputs/tables/nf_level_pals_year_arch_summs_2025-04-24.csv"))
df_sum <- read_csv(here::here("outputs/tables/nf_level_dominant_archetypes_uncertainty_nepa_2025-04-24.csv"))
pals_df <- read_delim("~/Analysis/NEPA_Efficiency/data/original/pals_ongoing_projects_11-2022.csv", delim = ";")

# Filter for date and select Forest Number and Purposes
pals_df_2009 <- pals_df %>%
  filter(as.Date(`INITIATION DATE`, format = "%m/%d/%Y") >= "2009-01-01")

df_year <- df_year %>%
  dplyr::select(`SIGNED FY`:forest_name) %>%
  mutate(total_proj = (DM + DN + ROD),
         pct_EA_EIS = ((ROD + DN)/(ROD + DN + DM)) * 100,
         EA_EIS_med_time = ((DN + ROD) / med_nepa_time_all_types),
         EA_EIS_mean_time = ((DN + ROD) / med_nepa_time_all_types),
         pctEAEIS_med_time = (pct_EA_EIS / med_nepa_time_all_types),
         pctEAEIS_mean_time = (pct_EA_EIS / med_nepa_time_all_types))

df_sum <- df_sum %>%
  dplyr::select(FOREST_ID:forest_name)

df_test <- left_join(df, df_sum, join_by("FORESTORGC" == "FOREST_ID"))

df_test <- df_test %>%
  mutate(pct_EA_EIS = ((ROD + DN)/(ROD + DN + DM)) * 100,
         EA_EIS_med_time = ((DN + ROD) / med_nepa_time),
         EA_EIS_mean_time = ((DN + ROD) / mean_nepa_time),
         pctEAEIS_med_time = (pct_EA_EIS / med_nepa_time),
         pctEAEIS_mean_time = (pct_EA_EIS / mean_nepa_time)
  )

# calculate correlations

for (i in unique(df_test$REGION)) {
  print(i)
  #print("Region: " + as.character(i))
  print(cor(x = df_test$ent_all[df_test$REGION == i], y = df_test$pct_EA_EIS[df_test$REGION == i], use = "complete.obs"))
}

for (i in unique(df_test$REGION)) {
  print(i)
  #print("Region: " + as.character(i))
  print(cor(x = df_test$ent_all[df_test$REGION == i], y = df_test$ROD[df_test$REGION == i], use = "complete.obs"))
}

cor(x = df_test$shan_dv_all, y = df_test$pct_EA_EIS, use = "complete.obs")
cor(x = df_test$ent_all, y = df_test$ROD, use = "complete.obs")
cor(x = (df_test$ent_all + df_test$shan_dv_all), y = df_test$DM, use = "complete.obs")


df_test <- df_test %>%
  filter(FORESTORGC != "0836")

df_test %>%
  ggplot(aes(x = ent_all, y = ROD, color = as.factor(REGION))) +
  geom_point()

df_test %>%
  ggplot(aes(x = shan_dv_all, y = pct_EA_EIS, color = as.factor(REGION))) +
  geom_point()

df_year %>%
  ggplot(aes(x = `SIGNED FY`, y = total_proj, color = as.factor(FOREST_ID))) +
  geom_line() + 
  facet_wrap(~REGION) +
  theme(legend.position="none")

df_year %>%
  ggplot(aes(x = `SIGNED FY`, y = pct_EA_EIS, fill = FOREST_ID, color = as.factor(FOREST_ID))) +
  geom_line() + 
  facet_wrap(~REGION) +
  theme(legend.position="none")
