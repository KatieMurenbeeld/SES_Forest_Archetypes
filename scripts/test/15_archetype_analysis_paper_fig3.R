library(tidyverse)
library(ggplot2)
library(patchwork)
library(RColorBrewer)
library(viridis)
library(ggpattern)
library(MetBrewer)




# Load the data
dom_arch_df <- read_csv(here::here("outputs/tables/nf_level_dominant_archetypes_uncertainty_2024-12-11.csv"))

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
       div_ent, height = 6, width = 6, dpi = 300)
