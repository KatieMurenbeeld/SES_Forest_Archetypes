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
library(forcats)

# Load the data
sgfcm_all_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")
sgfcm_all_attri_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")

sgfcm_all_k6_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_all_result_k6_2024-10-15.tif")
sgfcm_all_k8_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_all_result_k8_2024-10-15.tif")

plot(sgfcm_all_k6_result)
plot(sgfcm_all_k8_result)

sgfcm_all_result_k6_mod <- readRDS(here::here("outputs/SGFCM_all_attr_k6_2024-10-15.rds"))
sgfcm_all_result_k8_mod <- readRDS(here::here("outputs/SGFCM_all_attr_k8_2024-10-15.rds"))


# Variable interp plot
#data <- as.data.frame(sgfcm_all_attri_sc)
data <- as.data.frame(scale(sgfcm_all_attri))
data$groups_k6 <- sgfcm_all_result_k6_mod$Groups
data$groups_k8 <- sgfcm_all_result_k8_mod$Groups

data$groups_k6 <- gsub('V', 'A', data$groups_k6)
data$groups_k8 <- gsub('V', 'A', data$groups_k8)

k6_means_long <- data %>%
  group_by(groups_k6) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k6, names_to = "var_name", values_to = "mean")

k6_sd_long <- data %>%
  group_by(groups_k6) %>%
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k6, names_to = "var_name", values_to = "sd")

k6_med_long <- data %>%
  group_by(groups_k6) %>%
  summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k6, names_to = "var_name", values_to = "median")

k6_long <- left_join(k6_means_long, k6_sd_long)
k6_long <- left_join(k6_long, k6_med_long)

k8_means_long <- data %>%
  group_by(groups_k8) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k8, names_to = "var_name", values_to = "mean")

k8_sd_long <- data %>%
  group_by(groups_k8) %>%
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k8, names_to = "var_name", values_to = "sd")

k8_long <- left_join(k8_means_long, k8_sd_long)

# reorder the variables
k6_long_reorder <- k6_long %>% 
  mutate(var_name = fct_relevel(var_name, 
                            "treecov", "forprod", "tempseas", 
                            "precseas", "rough", "whp", 
                            "forgain", "distcrit", "distwild",
                            "pm25", "fedrich", 
                            "treeage", "pct_forpay", "pct_delmill",
                            "netmig", "comm_cap", "aip", 
                            "travtime", "hsbrd", "engbrd"))

k6_long_reorder <- k6_long_reorder %>%
  mutate(ostrom = case_when(var_name == "treecov" | var_name == "forprod" | var_name == "tempseas" | var_name == "precseas" | var_name == "rough" | var_name == "whp" ~ "resource system",
                            var_name == "distwild" | var_name == "distcrit" | var_name == "forgain" ~ "resource unit",
                            var_name == "fedrich" | var_name == "pm25" | var_name == "treeage" | var_name == "pct_forpay" | var_name == "pct_delmill" | var_name == "netmig" | var_name == "comm_cap" | var_name == "aip" | var_name == "travtime" | var_name == "hsbrd" | var_name == "engbrd" | var_name == "lesshs" ~ "users"))


k6_var_interp <- ggplot(k6_long_reorder, aes(x = var_name, y = mean, fill = ostrom)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  #scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  facet_wrap(~groups_k6) +
  theme(text = element_text(size = 20),
        legend.position = "right", 
        axis.title.y = element_blank()) 

k6_var_interp
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_all_k6_var_interp_", Sys.Date(), ".png"), 
       plot = k6_var_interp, width = 12, height = 8, dpi = 300) 

k6_var_interp_sd <- ggplot(k6_long_reorder, aes(x = var_name, y = sd, fill = ostrom)) +
  geom_col() +
  #scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  facet_wrap(~groups_k6) +
  theme(text = element_text(size = 20),
        legend.position = "right", 
        axis.title.y = element_blank()) 

k6_var_interp_sd
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_all_k6_var_interp_sd_", Sys.Date(), ".png"), 
       plot = k6_var_interp_sd, width = 12, height = 8, dpi = 300) 

k6_var_interp_med <- ggplot(k6_long_reorder, aes(x = var_name, y = median, fill = ostrom)) +
  geom_col() +
  geom_errorbar(aes(ymin=median-sd, ymax=median+sd), width=.2,
                position=position_dodge(.9)) +
  #scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  facet_wrap(~groups_k6) +
  theme(text = element_text(size = 20),
        legend.position = "right", 
        axis.title.y = element_blank()) 

k6_var_interp_med
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_all_k6_var_interp_med_", Sys.Date(), ".png"), 
       plot = k6_var_interp_med, width = 12, height = 8, dpi = 300)

ggplot(k8_long, aes(x = var_name, y = mean, fill = groups_k8)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  coord_flip() +
  facet_wrap(~groups_k8, ncol = 4) +
  theme(legend.position = "none", 
        axis.title.y = element_blank()) 

#----Create violin plots-------------
df_all <- as.data.frame(scale(rst), na.rm = TRUE)
vplots_k6 <- violinPlots(df_all, sgfcm_all_result_k6_mod$Groups)
vplots_k8 <- violinPlots(df_all, sgfcm_all_result_k8_mod$Groups)

# for k = 6
for ( i in 1:21){
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
         subtitle = names(df_all)[i],
         fill = "Archetype") +
    theme_bw() + 
    theme(text = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 16), 
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 16))
  plot(vplots_tmp)
  ggsave(paste0("~/Analysis/SES_Forest_Archetypes/outputs/plots/SGFCM_k6_vplot_", names(df_all)[i], "_", Sys.Date(), ".png"), 
         plot = vplots_tmp, width = 12, height = 12, dpi = 300)
}

# for k = 8
for ( i in 1:21){
  print(i)
  vplots_tmp <- vplots_k8[[i]] +
    scale_x_discrete(labels=c("A 1", "A 2", 
                              "A 3", "A 4",
                              "A 5", "A 6", 
                              "A 7", "A 8")) + 
    scale_fill_brewer(labels=c("A 1", "A 2", 
                               "A 3", "A 4",
                               "A 5", "A 6", 
                               "A 7", "A 8"),
                      palette = "Set2") +
    geom_violin() + 
    geom_hline(yintercept = 0) +
    labs(title = "SGFCM k = 8", 
         subtitle = names(df_all)[i],
         fill = "Archetype") +
    theme_bw() + 
    theme(text = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 16), 
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 16))
  #plot(vplots_tmp)
  ggsave(paste0("~/Analysis/SES_Forest_Archetypes/outputs/plots/SGFCM_k8_vplot_", names(df_all)[i], "_", Sys.Date(), ".png"), 
         plot = vplots_tmp, width = 12, height = 12, dpi = 300)
}
