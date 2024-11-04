library(tidyverse)
library(terra)
library(tidyterra)
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
library(psych)

# Load the data
sgfcm_all_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")
sgfcm_all_attri_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")

# load the raster results
sgfcm_all_k6_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_all_result_k6_2024-10-15.tif")

result_stk <- c(sgfcm_all_attri, sgfcm_all_k6_result)
result_stk_sc <- c(sgfcm_all_attri_sc, sgfcm_all_k6_result)
plot(result_stk_sc$aip)
plot(result_stk_sc$Groups)

a1_sc <- result_stk_sc %>%
  filter(Groups == 1)
a1 <- result_stk %>%
  filter(Groups == 1)
plot(a1$precseas)

a2 <- result_stk_sc %>%
  filter(Groups == 2)

a3 <- result_stk_sc %>%
  filter(Groups == 3)

a4 <- result_stk_sc %>%
  filter(Groups == 4)

a1_df <- as.data.frame(a1_sc)
a1_df <- a1_df %>%
  select(-Groups)

a2_df <- as.data.frame(a2)
a2_df <- a2_df %>%
  select(-Groups)

a3_df <- as.data.frame(a3)
a3_df <- a3_df %>%
  select(-Groups)

a4_df <- as.data.frame(a4)
a4_df <- a4_df %>%
  select(-Groups)

cor(a1_df)

ev <- eigen(cor(a2_df)) # get eigenvalues
ev$values
scree(a2_df, pc=FALSE)

Nfacs <- 1  

fit_a1 <- factanal(a1_sc_df, Nfacs, rotation = "promax")
fit_a2 <- factanal(a2_df, Nfacs, rotation = "promax")
fit_a3 <- factanal(a3_df, Nfacs, rotation = "promax")
fit_a4 <- factanal(a4_df, Nfacs, rotation = "promax")

print(fit_a1, digits=2, cutoff=0.3, sort=TRUE)
print(fit_a2, digits=2, cutoff=0.3, sort=TRUE)

loads_a1 <- fit_a1$loadings
fa.diagram(loads_a1)

loads_a2 <- fit_a2$loadings
fa.diagram(loads_a2)

loads_a3 <- fit_a3$loadings
fa.diagram(loads_a3)

loads_a4 <- fit_a4$loadings
fa.diagram(loads_a4)

fit_all <- factanal(as.data.frame(sgfcm_all_attri_sc), 6, rotations = "promax")
loads_all <- fit_all$loadings
fa.diagram(loads_all)
