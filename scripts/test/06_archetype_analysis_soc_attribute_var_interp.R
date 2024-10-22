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
sgfcm_soc_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_soc_attributes_2024-10-08.tif")
sgfcm_soc_attri_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_soc_attributes_scaled_2024-10-08.tif")

sgfcm_soc_result_k3 <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_soc_result_k3_2024-10-22.tif")
sgfcm_soc_result_k6 <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_soc_result_k6_2024-10-22.tif")

plot(sgfcm_soc_result_k3)
plot(sgfcm_soc_result_k6)

sgfcm_soc_result_k3_mod <- readRDS(here::here("outputs/SGFCM_soc_attr_k3_2024-10-22.rds"))
sgfcm_soc_result_k6_mod <- readRDS(here::here("outputs/SGFCM_soc_attr_k6_2024-10-22.rds"))


# Variable interp plot
#data <- as.data.frame(sgfcm_all_attri_sc)
# looking at all attributes
data <- as.data.frame(scale(sgfcm_all_attri))
data$groups_k3 <- sgfcm_soc_result_k3_mod$Groups
data$groups_k6 <- sgfcm_soc_result_k6_mod$Groups

data$groups_k3 <- gsub('V', 'A', data$groups_k3)
data$groups_k6 <- gsub('V', 'A', data$groups_k6)

k3_means_long <- data %>%
  group_by(groups_k3) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k3, names_to = "var_name", values_to = "mean")

k3_sd_long <- data %>%
  group_by(groups_k3) %>%
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k3, names_to = "var_name", values_to = "sd")

k3_long <- left_join(k3_means_long, k3_sd_long)

k6_means_long <- data %>%
  group_by(groups_k6) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k6, names_to = "var_name", values_to = "mean")

k6_sd_long <- data %>%
  group_by(groups_k6) %>%
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k6, names_to = "var_name", values_to = "sd")

k6_long <- left_join(k6_means_long, k6_sd_long)

k3_var_interp <- ggplot(k3_long, aes(x = var_name, y = mean, fill = groups_k3)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  facet_wrap(~groups_k3) +
  theme(text = element_text(size = 20),
        legend.position = "none", 
        axis.title.y = element_blank()) 


ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_soc-all_k3_var_interp_", Sys.Date(), ".png"), 
       plot = k3_var_interp, width = 12, height = 8, dpi = 300) 

k6_var_interp <- ggplot(k6_long, aes(x = var_name, y = mean, fill = groups_k6)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  facet_wrap(~groups_k6) +
  theme(text = element_text(size = 20),
        legend.position = "none", 
        axis.title.y = element_blank()) 


ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_soc-all_k6_var_interp_", Sys.Date(), ".png"), 
       plot = k6_var_interp, width = 12, height = 8, dpi = 300) 

# for just the soc attributes
data_soc <- as.data.frame(scale(sgfcm_soc_attri))
data_soc$groups_k3 <- sgfcm_soc_result_k3_mod$Groups
data_soc$groups_k6 <- sgfcm_soc_result_k6_mod$Groups

data_soc$groups_k3 <- gsub('V', 'A', data$groups_k3)
data_soc$groups_k6 <- gsub('V', 'A', data$groups_k6)

k3_soc_means_long <- data_soc %>%
  group_by(groups_k3) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k3, names_to = "var_name", values_to = "mean")

k3_soc_sd_long <- data_soc %>%
  group_by(groups_k3) %>%
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k3, names_to = "var_name", values_to = "sd")

k3_soc_long <- left_join(k3_soc_means_long, k3_soc_sd_long)

k6_soc_means_long <- data_soc %>%
  group_by(groups_k6) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k6, names_to = "var_name", values_to = "mean")

k6_soc_sd_long <- data_soc %>%
  group_by(groups_k6) %>%
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k6, names_to = "var_name", values_to = "sd")

k6_soc_long <- left_join(k6_soc_means_long, k6_soc_sd_long)


soc_var_interp_k3 <- ggplot(k3_soc_long, aes(x = var_name, y = mean, fill = groups_k3)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  facet_wrap(~groups_k3) +
  theme(text = element_text(size = 20),
        legend.position = "none", 
        axis.title.y = element_blank()) 

soc_var_interp_k3
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_soc_var_interp_k3_", Sys.Date(), ".png"), 
       plot = soc_var_interp_k3, width = 12, height = 8, dpi = 300) 

soc_var_interp_k6 <- ggplot(k6_soc_long, aes(x = var_name, y = mean, fill = groups_k6)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  facet_wrap(~groups_k6) +
  theme(text = element_text(size = 20),
        legend.position = "none", 
        axis.title.y = element_blank()) 

soc_var_interp_k6
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_soc_var_interp_k6_", Sys.Date(), ".png"), 
       plot = soc_var_interp_k6, width = 12, height = 8, dpi = 300) 


#----Create violin plots-------------
df_soc <- as.data.frame(scale(sgfcm_soc_attri), na.rm = TRUE)
vplots_k3 <- violinPlots(df_soc, sgfcm_soc_result_k3_mod$Groups)
vplots_k6 <- violinPlots(df_soc, sgfcm_soc_result_k6_mod$Groups)

for ( i in 1:13){
  print(i)
  vplots_tmp <- vplots_k3[[i]] +
    scale_x_discrete(labels=c("A 1", "A 2", 
                              "A 3")) + 
    scale_fill_brewer(labels=c("A 1", "A 2", 
                               "A 3"),
                      palette = "Set2") +
    geom_violin() + 
    geom_hline(yintercept = 0) +
    labs(title = "SGFCM k = 3", 
         subtitle = names(df_soc)[i],
         fill = "Archetype") +
    theme_bw() + 
    theme(text = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 16), 
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 16))
  #plot(vplots_tmp)
  ggsave(paste0("~/Analysis/SES_Forest_Archetypes/outputs/plots/SGFCM_soc_vplot_k3_", names(df_soc)[i], "_", Sys.Date(), ".png"), 
         plot = vplots_tmp, width = 12, height = 12, dpi = 300)
}


for ( i in 1:13){
  print(i)
  vplots_tmp <- vplots_k6[[i]] +
    scale_x_discrete(labels=c("A 1", "A 2", 
                              "A 3", "A 4", 
                              "A 5", "A 6")) + 
    scale_fill_brewer(labels=c("A 1", "A 2", 
                               "A 3", "A 4",
                               "A 5", "A 6"),
                      palette = "Set2") +
    geom_violin() + 
    geom_hline(yintercept = 0) +
    labs(title = "SGFCM k = 6", 
         subtitle = names(df_soc)[i],
         fill = "Archetype") +
    theme_bw() + 
    theme(text = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 16), 
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 16))
  #plot(vplots_tmp)
  ggsave(paste0("~/Analysis/SES_Forest_Archetypes/outputs/plots/SGFCM_soc_vplot_k6_", names(df_soc)[i], "_", Sys.Date(), ".png"), 
         plot = vplots_tmp, width = 12, height = 12, dpi = 300)
}

