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
groupvar <- "groups"
vars <- names(data)

k6_means_long <- data %>%
  group_by(groups_k6) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k6, names_to = "var_name", values_to = "mean")

k6_sd_long <- data %>%
  group_by(groups_k6) %>%
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k6, names_to = "var_name", values_to = "sd")

k6_long <- left_join(k6_means_long, k6_sd_long)

k8_means_long <- data %>%
  group_by(groups_k8) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k8, names_to = "var_name", values_to = "mean")

k8_sd_long <- data %>%
  group_by(groups_k8) %>%
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k8, names_to = "var_name", values_to = "sd")

k8_long <- left_join(k8_means_long, k8_sd_long)


ggplot(k6_long, aes(x = var_name, y = mean, fill = groups_k6)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  coord_flip() +
  facet_wrap(~groups_k6)

ggplot(k8_long, aes(x = var_name, y = mean, fill = groups_k8)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  coord_flip() +
  facet_wrap(~groups_k8, ncol = 4)



data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df2 <- data_summary(data, varname=, 
                    groupnames=c("groups_k6", "dose"))
