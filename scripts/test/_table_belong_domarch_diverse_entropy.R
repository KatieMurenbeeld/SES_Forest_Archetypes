library(tidyverse)
library(ggplot2)

# read in the table with shannon entropy, diversity, and dominant archetypes created
# in the _dom_archetype_forestlevel.R script

dom_arch_df <- read_csv(here::here("outputs/tables/nf_level_dominant_archetypes_uncertainty_2024-11-14.csv"))

# read in the table with the mean and median of the max belongings for each forest
# created in the _undecided_forests_testing.R script

ave_belong_df <- read_csv(here::here("outputs/tables/usfs_nf_max_belongings_2024-12-02.csv"))

# join the tables by the forest number

dom_arch_diverse_ent_belong_df <- dom_arch_df %>%
  select(-region) %>%
  left_join(., ave_belong_df, by = c("forest_num" = "forest"))

#----save the data frame as a csv----
write_csv(dom_arch_diverse_ent_belong_df, here::here(paste0("outputs/tables/usfs_nf_dom_arch_diverse_ent_belong_", Sys.Date(), ".csv")))
dom_arch_diverse_ent_belong_df <- read_csv(here::here("outputs/tables/usfs_nf_dom_arch_diverse_ent_belong_2024-12-02.csv"))
# quickly check the mean_max to the ent_all

test_scatter <- dom_arch_diverse_ent_belong_df %>%
  ggplot(aes(x=ave_max, y=entropy_all, group=as.factor(region), color=as.factor(region))) +
  geom_point() +
  #xlim(0, 0.5) +
  #ylim(0.5, 1) +
  theme_bw() +
  theme(legend.position="bottom")
test_scatter

test_scatter2 <- dom_arch_diverse_ent_belong_df %>%
  ggplot(aes(x=ave_max, y=entropy_all, group=as.factor(dom_archetype), color=as.factor(dom_archetype))) +
  geom_point() +
  #xlim(0, 0.5) +
  #ylim(0.5, 1) +
  theme_bw() +
  theme(legend.position="bottom")
test_scatter2
