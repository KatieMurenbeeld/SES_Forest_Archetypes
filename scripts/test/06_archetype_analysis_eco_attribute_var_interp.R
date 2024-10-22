library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(ggpattern)
library(distributional)
library(ggdist)
library(ggsci)
library(tigris)

# Load the data
sgfcm_all_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")
sgfcm_all_attri_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")
sgfcm_eco_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_eco_attributes_2024-10-08.tif")
sgfcm_eco_attri_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_eco_attributes_scaled_2024-10-08.tif")

sgfcm_eco_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_eco_result_2024-10-12.tif")

plot(sgfcm_eco_result)

sgfcm_eco_result_mod <- readRDS(here::here("outputs/SGFCM_eco_attr_2024-10-21.rds"))


# Variable interp plot
#data <- as.data.frame(sgfcm_all_attri_sc)
# looking at all attributes
data <- as.data.frame(scale(sgfcm_all_attri))
data$groups <- sgfcm_eco_result_mod$Groups

data$groups <- gsub('V', 'A', data$groups)

means_long <- data %>%
  group_by(groups) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups, names_to = "var_name", values_to = "mean")

sd_long <- data %>%
  group_by(groups) %>%
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups, names_to = "var_name", values_to = "sd")

df_long <- left_join(means_long, sd_long)


var_interp <- ggplot(df_long, aes(x = var_name, y = mean, fill = groups)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  facet_wrap(~groups) +
  theme(text = element_text(size = 20),
        legend.position = "none", 
        axis.title.y = element_blank()) 

var_interp
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_eco-all_var_interp_", Sys.Date(), ".png"), 
       plot = var_interp, width = 12, height = 8, dpi = 300) 

# for just the eco attributes
data_eco <- as.data.frame(scale(sgfcm_eco_attri))
data_eco$groups <- sgfcm_eco_result_mod$Groups

data_eco$groups <- gsub('V', 'A', data$groups)

eco_means_long <- data_eco %>%
  group_by(groups) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups, names_to = "var_name", values_to = "mean")

eco_sd_long <- data_eco %>%
  group_by(groups) %>%
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups, names_to = "var_name", values_to = "sd")

eco_df_long <- left_join(eco_means_long, eco_sd_long)


eco_var_interp <- ggplot(eco_df_long, aes(x = var_name, y = mean, fill = groups)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  facet_wrap(~groups) +
  theme(text = element_text(size = 20),
        legend.position = "none", 
        axis.title.y = element_blank()) 

eco_var_interp
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_eco_var_interp_", Sys.Date(), ".png"), 
       plot = eco_var_interp, width = 12, height = 8, dpi = 300) 


#----Create violin plots-------------
df_eco <- as.data.frame(scale(sgfcm_eco_attri), na.rm = TRUE)
vplots <- violinPlots(df_eco, sgfcm_eco_result_mod$Groups)

for ( i in 1:8){
  print(i)
  vplots_tmp <- vplots[[i]] +
    scale_x_discrete(labels=c("A 1", "A 2", 
                              "A 3")) + 
    scale_fill_brewer(labels=c("A 1", "A 2", 
                               "A 3"),
                      palette = "Set2") +
    geom_violin() + 
    geom_hline(yintercept = 0) +
    labs(title = "SGFCM k = 3", 
         subtitle = names(df_eco)[i],
         fill = "Archetype") +
    theme_bw() + 
    theme(text = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 16), 
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 16))
  #plot(vplots_tmp)
  ggsave(paste0("~/Analysis/SES_Forest_Archetypes/outputs/plots/SGFCM_eco_vplot_", names(df_eco)[i], "_", Sys.Date(), ".png"), 
         plot = vplots_tmp, width = 12, height = 12, dpi = 300)
}

