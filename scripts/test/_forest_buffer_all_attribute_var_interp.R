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
sgfcm_all_attri_sc <- rast("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/processed/nf_buffers_all_attributes_scaled_2025-08-15.tif")
sgfcm_all_attri <- rast("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/processed/nf_buffers_all_attributes_2025-08-15.tif")

sgfcm_all_k3_result <- rast("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/outputs/nfbuffers_SGFCM_all_result_k3_2025-08-12.tif")

plot(sgfcm_all_k3_result)


sgfcm_all_result_k3_mod <- readRDS(here::here("outputs/nfbuffers_SGFCM_all_attr_k3_2025-08-12.rds"))



# Variable interp plot
#data <- as.data.frame(sgfcm_all_attri_sc)
data <- as.data.frame(scale(sgfcm_all_attri))
data$groups_k3 <- sgfcm_all_result_k3_mod$Groups

data$groups_k3 <- gsub('V', 'A', data$groups_k3)

k3_long_df <- data %>%
  #dplyr::select(-groups_k8) %>%
  pivot_longer(!groups_k3, names_to = "var_name")

k3_means_long <- data %>%
  group_by(groups_k3) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k3, names_to = "var_name", values_to = "mean")

k3_sd_long <- data %>%
  group_by(groups_k3) %>%
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k3, names_to = "var_name", values_to = "sd")

k3_med_long <- data %>%
  group_by(groups_k3) %>%
  summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k3, names_to = "var_name", values_to = "median")

k3_long <- left_join(k3_means_long, k3_sd_long)
k3_long <- left_join(k3_long, k3_med_long)

# reorder the variables
k3_long_reorder <- k3_long %>% 
  mutate(var_name = fct_relevel(var_name, 
                                "treecov", "forprod", "tempseas", 
                                "precseas", "rough", "whp", 
                                "forgain", "distcrit", "distwild",
                                "pm25", "fedrich", 
                                "treeage", "pct_forpay", "pct_delmill",
                                "netmig", "comm_cap", "aip", 
                                "travtime", "hsbrd", "engbrd", "lesshs"))

k3_long_reorder <- k3_long_reorder %>%
  mutate(ostrom = case_when(var_name == "treecov" | var_name == "forprod" | var_name == "tempseas" | var_name == "precseas" | var_name == "rough" | var_name == "whp" ~ "resource system",
                            var_name == "distwild" | var_name == "distcrit" | var_name == "forgain" ~ "resource unit",
                            var_name == "fedrich" | var_name == "pm25" | var_name == "treeage" | var_name == "pct_forpay" | var_name == "pct_delmill" | var_name == "netmig" | var_name == "comm_cap" | var_name == "aip" | var_name == "travtime" | var_name == "hsbrd" | var_name == "engbrd" | var_name == "lesshs" ~ "users"))


k3_var_interp <- ggplot(k3_long_reorder, aes(x = var_name, y = mean, fill = ostrom)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  #scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  facet_wrap(~groups_k3) +
  theme(text = element_text(size = 20),
        legend.position = "right", 
        axis.title.y = element_blank()) 

k3_var_interp
#ggsave(paste0("~/Analysis/Archetype_Analysis/figures/nfbuffer_sgfcm_all_k3_var_interp_", Sys.Date(), ".png"), 
#       plot = k3_var_interp, width = 12, height = 8, dpi = 300) 

k3_var_interp_sd <- ggplot(k3_long_reorder, aes(x = var_name, y = sd, fill = ostrom)) +
  geom_col() +
  #scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  facet_wrap(~groups_k3) +
  theme(text = element_text(size = 20),
        legend.position = "right", 
        axis.title.y = element_blank()) 

k3_var_interp_sd
#ggsave(paste0("~/Analysis/Archetype_Analysis/figures/nfbuffer_sgfcm_all_k3_var_interp_sd_", Sys.Date(), ".png"), 
#       plot = k3_var_interp_sd, width = 12, height = 8, dpi = 300) 

k3_var_interp_med <- ggplot(k3_long_reorder, aes(x = var_name, y = median, fill = ostrom)) +
  geom_col() +
  geom_errorbar(aes(ymin=median-sd, ymax=median+sd), width=.2,
                position=position_dodge(.9)) +
  #scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  facet_wrap(~groups_k3) +
  theme(text = element_text(size = 20),
        legend.position = "right", 
        axis.title.y = element_blank()) 

k3_var_interp_med
#ggsave(paste0("~/Analysis/Archetype_Analysis/figures/nfbuffer_sgfcm_all_k3_var_interp_med_", Sys.Date(), ".png"), 
#       plot = k3_var_interp_med, width = 12, height = 8, dpi = 300)


# Create bar plots of the variables where IQR != 0
check_iqr_overlap <- function(x) {
  # Calculate the 25th and 75th percentiles
  lower <- quantile(x, 0.25, na.rm = TRUE)
  upper <- quantile(x, 0.75, na.rm = TRUE)
  
  # Check if the IQR range overlaps 0
  return(lower <= 0 & upper >= 0)
}

overlap <- k3_long_df %>% 
  group_by(groups_k3, var_name) %>% 
  summarise(overlap = check_iqr_overlap(value), .groups="drop")


k3_long_join <- k3_long_df %>% 
  left_join(overlap) %>% 
  filter(overlap == FALSE)

# reorder the variables
k3_long_overlap_reorder <- k3_long_join %>% 
  mutate(var_name = fct_relevel(var_name, 
                                "treecov", "forprod", "tempseas", 
                                "precseas", "rough", "whp", 
                                "forgain", 
                                #"distcrit", 
                                "distwild",
                                "pm25", "fedrich", 
                                "treeage", "pct_forpay", "pct_delmill",
                                "netmig", "comm_cap", "aip",
                                "travtime", "hsbrd", "engbrd"
                                #"lesshs"
  ))

k3_long_overlap_reorder <- k3_long_overlap_reorder %>% # need to remove dist to critical habitat and less hs
  mutate(ostrom = case_when(var_name == "treecov" | var_name == "forprod" | var_name == "tempseas" | var_name == "precseas" | var_name == "rough" | var_name == "whp" ~ "resource system",
                            var_name == "distwild" | var_name == "forgain" ~ "resource unit",
                            var_name == "fedrich" | var_name == "pm25" | var_name == "treeage" | var_name == "pct_forpay" | var_name == "pct_delmill" | var_name == "netmig" | var_name == "comm_cap" | var_name == "aip" | var_name == "travtime" | var_name == "hsbrd" | var_name == "engbrd" | var_name == "lesshs" ~ "users"))


k3_iqr_no_overlap <- ggplot(data=k3_long_overlap_reorder, mapping = aes(x=var_name, y=value, fill=ostrom)) +
  geom_boxplot(outliers = FALSE, coef=0) +
  geom_hline(yintercept = 0, linetype=2) +
  #scale_fill_manual(values=c15)+
  coord_flip()+
  theme_bw()+
  facet_wrap(vars(groups_k3))

k3_iqr_no_overlap
#ggsave(paste0("~/Analysis/Archetype_Analysis/figures/nfbuffer_sgfcm_all_k3_var_interp_iqr_no_overlap_", Sys.Date(), ".png"), 
#       plot = k3_iqr_no_overlap, width = 12, height = 8, dpi = 300)

# Create bar plots of the standard deviation for the values that do overlap with 0
k3_long_join_overlap_true <- k3_long_df %>% 
  left_join(overlap) %>% 
  filter(overlap == TRUE)

k3_sd_overlap <- k3_long_join_overlap_true %>% 
  group_by(groups_k3, var_name) %>% 
  summarise(sd = sd(value), .groups="drop")

# reorder the variables
k3_sd_overlap_reorder <- k3_sd_overlap %>% 
  mutate(var_name = fct_relevel(var_name, 
                                #"treecov", 
                                "forprod", "tempseas", 
                                "precseas", "rough", "whp", 
                                "forgain", 
                                "distcrit", 
                                "distwild",
                                #"pm25", 
                                "fedrich", 
                                "treeage", "pct_forpay", 
                                #"pct_delmill",
                                "netmig", "comm_cap", "aip", 
                                "travtime", "hsbrd", "engbrd", "lesshs"))

k3_sd_long_overlap_reorder <- k3_sd_overlap_reorder %>% # need to remove treecover, pm25, and pct_delmill
  mutate(ostrom = case_when(var_name == "forprod" | var_name == "tempseas" | var_name == "precseas" | var_name == "rough" | var_name == "whp" ~ "resource system",
                            var_name == "distwild" | var_name == "distcrit" |  var_name == "forgain" ~ "resource unit",
                            var_name == "fedrich" | var_name == "treeage" | var_name == "pct_forpay" | var_name == "netmig" | var_name == "comm_cap" | var_name == "aip" | var_name == "travtime" | var_name == "hsbrd" | var_name == "engbrd" | var_name == "lesshs" ~ "users"))


k3_sd_overlap <- ggplot(k3_sd_long_overlap_reorder, aes(x = var_name, y = sd, fill = ostrom)) +
  geom_col() +
  #scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  facet_wrap(~groups_k3) +
  theme(text = element_text(size = 20),
        legend.position = "right", 
        axis.title.y = element_blank()) 

k3_sd_overlap
#ggsave(paste0("~/Analysis/Archetype_Analysis/figures/nfbuffer_sgfcm_all_k3_var_interp_sd_overlap_", Sys.Date(), ".png"), 
#       plot = k3_sd_overlap, width = 12, height = 8, dpi = 300)


ggplot(data=k3_long_overlap_reorder, mapping = aes(x=var_name, y=sd, fill=ostrom)) +
  geom_boxplot(outliers = FALSE, coef=0) +
  geom_hline(yintercept = 0, linetype=2) +
  #scale_fill_manual(values=c15)+
  coord_flip()+
  theme_bw()+
  facet_wrap(vars(groups_k3))


