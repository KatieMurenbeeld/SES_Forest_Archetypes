library(tidyverse)
library(ggplot2)
library(MetBrewer)
library(cowplot)
library(patchwork)

# load the data

nf_year_summ_arch <- read_csv(here::here("outputs/tables/nf_nepa_projs_arch_summ_2025-04-24.csv"))

# correlations
cor(nf_year_summ_arch$shan_diverse, nf_year_summ_arch$yearly_pct_med_EA_EIS, use = "complete.obs")
cor(nf_year_summ_arch$entropy_all, nf_year_summ_arch$yearly_pct_med_EA_EIS, use = "complete.obs")
cor(nf_year_summ_arch$shan_diverse, nf_year_summ_arch$total_EIS, use = "complete.obs")
cor(nf_year_summ_arch$entropy_all, nf_year_summ_arch$total_EIS, use = "complete.obs")

# plot
p1 <- nf_year_summ_arch %>%
  filter(div_to_ent == "high_ent_high_div" | div_to_ent == "low_ent_low_div") %>%
  ggplot(aes(x = yearly_pct_med_EA_EIS)) +
  geom_histogram(aes(y = ..density.., fill = div_to_ent, alpha = 0.6), bins = 10) +
  geom_density(aes(color = div_to_ent, alpha = 0.6)) +
  xlab("Yearly Median % EIS + EA") + 
  ylab("Density") +
  scale_fill_manual(labels = c("High - High", "Low - Low"), values=met.brewer("Hokusai1", 2)) +
  scale_color_manual(labels = c("High - High", "Low - Low"),values=met.brewer("Hokusai1", 2)) +
  guides(fill = "none",
         alpha = "none", 
         color = "none") +
  theme_minimal() + 
  theme_bw() +
  theme(text = element_text(size = 8),
        axis.text = element_text(size = 8)
  )
p1

p2 <- nf_year_summ_arch %>%
  filter(div_to_ent == "high_ent_high_div" | div_to_ent == "low_ent_low_div") %>%
  ggplot(aes(x = total_EIS)) +
  geom_histogram(aes(y = ..density.., fill = div_to_ent, alpha = 0.6), bins = 10) +
  geom_density(aes(color = div_to_ent, alpha = 0.6)) +
  xlab("Total EIS") + 
  ylab("") +
  scale_fill_manual(labels = c("High - High", "Low - Low"), values=met.brewer("Hokusai1", 2)) +
  scale_color_manual(labels = c("High - High", "Low - Low"),values=met.brewer("Hokusai1", 2)) +
  guides(fill = guide_legend(title = "Diversity - Entropy"),
         alpha = "none", 
         color = "none") +
  theme_minimal() + 
  theme_bw() +
  theme(text = element_text(size = 8),
        axis.text = element_text(size = 8)
  )
p2

panel <- p1 | p2 
  #plot_layout(heights = unit(c(10, 1), c('cm', 'null')))
panel

# save
ggsave(here::here(paste0("outputs/plots/archetype_analysis_fig6_testing_", Sys.Date(), ".jpeg")), 
       plot = panel,  height = 100, width = 140, dpi = 500, units = "mm", device = "jpeg", bg = "white")
