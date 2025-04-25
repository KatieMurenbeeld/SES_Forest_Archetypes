library(tidyverse)
library(ggplot2)
library(MetBrewer)

# load the data

df <- read_csv(here::here("outputs/tables/nf_level_dominant_archetypes_uncertainty_nepa_2025-04-24.csv")) 
df_year <- read_csv(here::here("outputs/tables/nf_level_pals_year_arch_summs_2025-04-24.csv"))
pals_df <- read_delim("~/Analysis/NEPA_Efficiency/data/original/pals_ongoing_projects_11-2022.csv", delim = ";")

# Filter for date and select Forest Number and Purposes
pals_df_2009 <- pals_df %>%
  filter(as.Date(`INITIATION DATE`, format = "%m/%d/%Y") >= "2009-01-01")

pals_filt <- pals_df_2009 %>%
  filter(FOREST_ID %in% c("0402", "0412", "0407", "0408", "0410"))

df_year <- df_year %>%
  mutate(total_proj = (DM + DN + ROD),
         pct_EA_EIS = ((ROD + DN)/(ROD + DN + DM)) * 100,
         EA_EIS_med_time = ((DN + ROD) / med_nepa_time_all_types),
         EA_EIS_mean_time = ((DN + ROD) / med_nepa_time_all_types),
         pctEAEIS_med_time = (pct_EA_EIS / med_nepa_time_all_types),
         pctEAEIS_mean_time = (pct_EA_EIS / med_nepa_time_all_types))

df <- df %>%
  mutate(pct_EA_EIS = ((ROD + DN)/(ROD + DN + DM)) * 100,
         EA_EIS_med_time = ((DN + ROD) / med_nepa_time),
         EA_EIS_mean_time = ((DN + ROD) / mean_nepa_time),
         pctEAEIS_med_time = (pct_EA_EIS / med_nepa_time),
         pctEAEIS_mean_time = (pct_EA_EIS / mean_nepa_time)
         )

df_year %>%
  ggplot(aes(x = `SIGNED FY`, y = total_proj, color = as.factor(FOREST_ID))) +
  geom_line() + 
  facet_wrap(~REGION) +
  theme(legend.position="none")

df_year %>%
  ggplot(aes(x = `SIGNED FY`, y = total_proj, fill = FOREST_ID, color = as.factor(dom_archetype))) +
  geom_line(aes(linewidth = shan_diverse_norm, alpha = 0.5)) + 
  facet_wrap(~REGION) 

draft_pct_eaeis <- df_year %>%
  ggplot(aes(x = `SIGNED FY`, y = pct_EA_EIS, fill = FOREST_ID, color = as.factor(dom_archetype))) +
  geom_line() + 
  facet_wrap(~REGION)
draft_pct_eaeis
#ggsave(here::here(paste0("outputs/plots/draft_pcteaeis_for_arch_",
#                         Sys.Date(), ".png")),
#       draft_pct_eaeis, width = 12, height = 9, dpi = 300)

draft_pct_eaeis_divent <- df_year %>%
  ggplot(aes(x = `SIGNED FY`, y = pct_EA_EIS, fill = FOREST_ID, color = as.factor(div_to_ent))) +
  geom_line() + 
  facet_wrap(~REGION)
draft_pct_eaeis_divent

df_year %>%
  ggplot(aes(x = `SIGNED FY`, y = pct_EA_EIS, fill = FOREST_ID, color = as.factor(div_to_ent))) +
  geom_line()

df_year %>%
  ggplot(aes(x = `SIGNED FY`, y = pct_EA_EIS, fill = FOREST_ID, color = as.factor(div_to_ent))) +
  geom_point()

df %>%
  ggplot(aes(x = entropy_all, y = pct_EA_EIS, color = as.factor(dom_archetype))) +
  geom_point()

df %>%
  ggplot(aes(x = shan_diverse, y = ROD, color = as.factor(dom_archetype))) +
  geom_point()

df %>%
  ggplot(aes(x = (df$ent_all_sc + df$shan_div_sc), y = pct_EA_EIS, color = as.factor(div_to_ent))) +
  geom_point()

cor(x = df$shan_diverse, y = df$pct_EA_EIS, use = "complete.obs")
cor(x = df$entropy_all, y = df$ROD, use = "complete.obs")
cor(x = df$shan_diverse, y = df$ROD, use = "complete.obs")
cor(x = (df$ent_all_sc + df$shan_div_sc), y = df$DM, use = "complete.obs")

df_year %>%
  ggplot(aes(x = div_to_ent, y = pct_EA_EIS)) +
  geom_boxplot()

df_year %>%
  ggplot(aes(x = `SIGNED FY`, y = pctEAEIS_med_time, fill = FOREST_ID, color = as.factor(dom_archetype))) +
  geom_line() + 
  facet_wrap(~REGION)

draft_eaeis_medtime <- df_year %>%
  ggplot(aes(x = `SIGNED FY`, y = EA_EIS_med_time, fill = FOREST_ID, color = as.factor(dom_archetype))) +
  geom_line() + 
  facet_wrap(~REGION)
draft_eaeis_medtime
ggsave(here::here(paste0("outputs/plots/draft_eaeis_medtime_for_arch_",
                         Sys.Date(), ".png")),
       draft_eaeis_medtime, width = 12, height = 9, dpi = 300)

df %>%
  group_by(region) %>%
  ggplot() %>%
  geom_histogram(med_nepa_time) %>%
  facet_wrap(~region)

# for forests of interest 
# boise = 0402, payette = 0412 , fishlake = 0408, dixie = 0407, manti-la sal = 0410

df_filt <- df %>%
  filter(FOREST_ID %in% c("0402", "0412", "0407", "0408", "0410"))

df_year_filt <- df_year %>%
  filter(FOREST_ID %in% c("0402", "0412", "0407", "0408", "0410")) %>%
  mutate(forest_fct = factor(FOREST_ID, levels = c("0402", "0412", "0407", "0408", "0410"))) %>%
  mutate(forname_fct = factor(forest_name, levels = c("Boise National Forest", 
                                                    "Payette National Forest",
                                                    "Dixie National Forest", 
                                                    "Fishlake National Forest",
                                                    "Manti-La Sal National Forest")))

draft_for_proj_year <- df_year_filt %>%
  ggplot() +
  geom_line(aes(x = `SIGNED FY`, y = ROD, color = "ROD")) +
  geom_line(aes(x = `SIGNED FY`, y = total_proj, color = "total_proj")) +
  geom_line(aes(x = `SIGNED FY`, y = DN, color = "DN")) + 
  geom_line(aes(x = `SIGNED FY`, y = DM, color = "DM")) + 
  facet_wrap(~forname_fct, ncol = 2) + 
  ylab("Number of NEPA Projects") + 
  xlab("Year") +
  theme(legend.position = "right")
draft_for_proj_year
ggsave(here::here(paste0("outputs/plots/draft_forests_proj_year_",
                         Sys.Date(), ".png")),
       draft_for_proj_year, width = 9, height = 12, dpi = 300)

draft_for_proj_metric_year <- df_year_filt %>%
  ggplot() +
  geom_line(aes(x = `SIGNED FY`, y = EA_EIS_med_time, color = "EA_EIS_med_time")) +
  geom_line(aes(x = `SIGNED FY`, y = EA_EIS_mean_time, color = "EA_EIS_mean_time")) +
  facet_wrap(~forname_fct, ncol = 2) + 
  ylab("Number of EA and EIS divided by NEPA times") + 
  xlab("Year") +
  theme(legend.position = "right")
draft_for_proj_metric_year
ggsave(here::here(paste0("outputs/plots/draft_forests_proj_metric_year_",
                         Sys.Date(), ".png")),
       draft_for_proj_metric_year, width = 9, height = 12, dpi = 300)

draft_for_pctproj_metric_year <- df_year_filt %>%
  ggplot() +
  geom_line(aes(x = `SIGNED FY`, y = pctEAEIS_med_time, color = "pctEAEIS_med_time")) +
  geom_line(aes(x = `SIGNED FY`, y = pctEAEIS_mean_time, color = "pctEAEIS_mean_time")) +
  facet_wrap(~forname_fct, ncol = 2) + 
  ylab("% Projects that are EA and EIS divided by NEPA times") + 
  xlab("Year") +
  theme(legend.position = "right")
draft_for_pctproj_metric_year
ggsave(here::here(paste0("outputs/plots/draft_forests_pctproj_metric_year_",
                         Sys.Date(), ".png")),
       draft_for_pctproj_metric_year, width = 9, height = 12, dpi = 300)

draft_for_pctproj_year <- df_year_filt %>%
  ggplot() +
  geom_line(aes(x = `SIGNED FY`, y = pct_EA_EIS, color = forname_fct)) +
  ylab("% EA and EIS Projects") + 
  xlab("Year") +
  theme(legend.position = "right")
draft_for_pctproj_year
ggsave(here::here(paste0("outputs/plots/draft_forests_pctproj_year_",
                         Sys.Date(), ".png")),
       draft_for_pctproj_year, width = 10, height = 6, dpi = 300)




# censored data from forests of interest
pals_filt %>%
  group_by(FOREST_ID) %>%
  summarise(sum_na = sum(is.na(`SIGNED FY`)))


