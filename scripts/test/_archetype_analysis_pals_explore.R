library(tidyverse)
library(ggplot2)
library(MetBrewer)

# load the data

df <- read_csv(here::here("outputs/tables/nf_level_dominant_archetypes_uncertainty_nepa_2025-01-30.csv")) 
df_year <- read_csv(here::here("outputs/tables/nf_level_pals_year_arch_summs_2025-01-30.csv"))
pals_df <- read_delim("~/Analysis/NEPA_Efficiency/data/original/pals_ongoing_projects_11-2022.csv", delim = ";")

# Filter for date and select Forest Number and Purposes
pals_df_2009 <- pals_df %>%
  filter(as.Date(`INITIATION DATE`, format = "%m/%d/%Y") >= "2009-01-01")

pals_filt <- pals_df_2009 %>%
  filter(FOREST_ID %in% c("0402", "0412", "0407", "0408", "0410"))

df_year <- df_year %>%
  mutate(total_proj = (DM + DN + ROD),
         pct_EA_EIS = ((ROD + DN)/(ROD + DN + DM)) * 100,
         EA_EIS_med_time = ((DN + ROD) / med_nepa_time),
         EA_EIS_mean_time = ((DN + ROD) / mean_nepa_time),
         pctEAEIS_med_time = (pct_EA_EIS / med_nepa_time),
         pctEAEIS_mean_time = (pct_EA_EIS / mean_nepa_time))

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

df_year %>%
  ggplot(aes(x = `SIGNED FY`, y = pct_EA_EIS, fill = FOREST_ID, color = as.factor(dom_archetype))) +
  geom_line() + 
  facet_wrap(~REGION)

df_year %>%
  ggplot(aes(x = `SIGNED FY`, y = pctEAEIS_med_time, fill = FOREST_ID, color = as.factor(dom_archetype))) +
  geom_line() + 
  facet_wrap(~REGION)

df_year %>%
  ggplot(aes(x = `SIGNED FY`, y = pctEAEIS_mean_time, fill = FOREST_ID, color = as.factor(dom_archetype))) +
  geom_line() + 
  facet_wrap(~REGION)


# for forests of interest 
# boise = 0402, payette = 0412 , fishlake = 0408, dixie = 0407, manti-la sal = 0410

df_filt <- df %>%
  filter(FOREST_ID %in% c("0402", "0412", "0407", "0408", "0410"))

df_year_filt <- df_year %>%
  filter(FOREST_ID %in% c("0402", "0412", "0407", "0408", "0410"))

df_year_filt %>%
  ggplot() +
  geom_line(aes(x = `SIGNED FY`, y = ROD), color = "blue4") +
  geom_line(aes(x = `SIGNED FY`, y = DN), color = "goldenrod") + 
  geom_line(aes(x = `SIGNED FY`, y = DM), color = "forestgreen") + 
  facet_wrap(~FOREST_ID)

# censored data from forests of interest
pals_filt %>%
  group_by(FOREST_ID) %>%
  summarise(sum_na = sum(is.na(`SIGNED FY`)))


