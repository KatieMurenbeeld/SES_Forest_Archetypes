library(tidyverse)
library(ggplot2)
library(patchwork)
library(RColorBrewer)
library(viridis)
library(ggpattern)
library(MetBrewer)




# Load the data
dom_arch_df <- read_csv(here::here("outputs/tables/nf_level_dominant_archetypes_uncertainty_2025-04-24.csv"))

# Set axis labels
axLabels <- c("", "Low","", "", "","High", "")

# Rename the archetypes from numbers to letters
dom_arch_df <- dom_arch_df %>%
  mutate(dom_arch_alpha = case_when(dom_archetype == 1 ~ "A", 
                                 dom_archetype == 2 ~ "B",
                                 dom_archetype == 3 ~ "C",
                                 dom_archetype == 4 ~ "D",
                                 dom_archetype == 5 ~ "E", 
                                 dom_archetype == 6 ~ "F"))

# Scatter plot of forests by high and low entropy and diversity scores
div_ent <- dom_arch_df %>%
  ggplot() +
  geom_point(aes(x=scale(entropy_all), y=scale(shan_diverse), 
                 color=as.factor(dom_arch_alpha), fill=as.factor(dom_arch_alpha), 
                 size = pct_area_dom_arch, alpha = 0.25), show.legend = TRUE) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  scale_color_met_d("Hokusai3") +
  xlim(-3, 3) +
  ylim(-3, 3) +
  scale_size_continuous(name = "% Area Dom. Archetype") + 
  theme_bw() +
  guides(alpha = "none") +
  guides(size = "none") +
  guides(fill = "none") +
  annotate("text", x = 2, y = 2, label= "high-high", size = 8/.pt) + 
  annotate("text", x = -2, y = -3, label = "low-low", size = 8/.pt) +
  annotate("text", x = -1.25, y = 2, label= "high-low", size = 8/.pt) + 
  annotate("text", x = 2, y = -3, label = "low-high", size = 8/.pt) +
  theme(text = element_text(size = 8),
        legend.position = "inside",
        legend.position.inside = c(0.11, 0.787),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"),
        axis.text = element_text(size = 8)
        ) +
  labs(x = "Entropy (scaled)",
       y = "Diveristy (scaled)",
       color = "Archetype",
       size = "% Area Dom. Archetype")
div_ent

ggsave(here::here(paste0("outputs/plots/archetype_analysis_fig3_testing_", Sys.Date(), ".jpeg")),
       div_ent, height = 100, width = 85, dpi = 300, units = "mm", device = "jpeg")
