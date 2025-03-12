library(tidyverse)
library(ggplot2)
library(patchwork)
library(RColorBrewer)
library(viridis)
library(ggpattern)
library(MetBrewer)


dom_arch_df <- read_csv(here::here("outputs/tables/nf_level_dominant_archetypes_uncertainty_2024-11-14.csv"))

dom_arch_df <- dom_arch_df %>%
  mutate(ent_all_sc = scale(entropy_all)[1:109],
         shan_div_sc = scale(shan_diverse)[1:109]) %>%
  mutate(div_to_ent = case_when(ent_all_sc < 0 & shan_div_sc < 0 ~ "low_ent_low_div",
                                ent_all_sc < 0 & shan_div_sc > 0 ~ "low_ent_high_div",
                                ent_all_sc > 0 & shan_div_sc < 0 ~ "high_ent_low_div",
                                ent_all_sc > 0 & shan_div_sc > 0 ~ "high_ent_high_div"))
#write_csv(dom_arch_df, here::here(paste0("outputs/tables/nf_level_dominant_archetypes_uncertainty_", Sys.Date(), ".csv")))
dom_arch_df <- read_csv(here::here("outputs/tables/nf_level_dominant_archetypes_uncertainty_2024-12-11.csv"))


axLabels <- c("", "Low","", "", "","High", "")
div_ent <- dom_arch_df %>%
  ggplot() +
  geom_point(aes(x=scale(entropy_all), y=scale(shan_diverse), 
                 color=as.factor(dom_archetype), size = pct_area_dom_arch, 
                 alpha = 0.25), show.legend = TRUE) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  #scale_color_brewer(palette = "Set2") + 
  scale_color_met_d("Hokusai3") +
  #scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3), labels = axLabels) +
  #scale_y_continuous(breaks = c(-3,-2,-1,0,1,2,3), labels = axLabels) +
  xlim(-3, 3) +
  ylim(-3, 3) +
  scale_size_continuous(name = "% Area Dom. Archetype") + 
  theme_bw() +
  guides(alpha = "none") +
  guides(size = "none") +
  annotate("text", x = 2, y = 2, label= "high-high") + 
  annotate("text", x = -2, y = -3, label = "low-low") +
  annotate("text", x = -1.25, y = 2, label= "high-low") + 
  annotate("text", x = 2, y = -3, label = "low-high") +
  theme(legend.position = "inside",
        legend.position.inside = c(0.11, 0.787)) +
  labs(x = "Entropy (scaled)",
       y = "Diveristy (scaled)",
       color = "Archetype",
       size = "% Area Dom. Archetype")
div_ent

ggsave(here::here(paste0("outputs/plots/diverse_entropy_sc_scatter_", Sys.Date(), ".png")),
       div_ent, height = 4, width = 4, dpi = 300)
