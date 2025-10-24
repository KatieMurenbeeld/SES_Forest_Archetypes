library(tidyverse)
library(ggplot2)
library(MetBrewer)
library(data.table)
library(forcats)
library(gghighlight)
library(ggcharts)
library(patchwork)

# load the data

# need to write the script that saves the nf_level_dominant_archetypes_uncertainty_nepa_2025-01-30.csv
df <- read_csv(here::here("outputs/tables/nf_level_dominant_archetypes_uncertainty_nepa_2025-04-24.csv")) 
# need to write the script that saves the nf_level_pals_year_arch_summs_2025-01-30.csv
df_year <- read_csv(here::here("outputs/tables/nf_level_pals_year_arch_summs_2025-04-24.csv"))
pals_df <- read_delim("~/Analysis/NEPA_Efficiency/data/original/pals_ongoing_projects_11-2022.csv", delim = ";")
nepa_summ_df <- read_csv(here::here("outputs/tables/nf_nepa_projs_arch_summ_2025-09-11.csv"))


pals_df_test <- pals_df %>%
  filter(as.Date(`INITIATION DATE`, format = "%m/%d/%Y") >= "2009-01-01") %>%
  filter(REGION_ID != "10" & REGION_ID != "13" & REGION_ID != "24" & REGION_ID != "00") %>%
  dplyr::select(`SIGNED FY`, FOREST_ID, REGION_ID, `ELAPSED DAYS`, `DECISION TYPE`, 
                `FC Facility management – purpose`, 
                `FR Research – purpose`, `HF Fuels management – purpose`, `HR Heritage resource management – purpose`,
                `LM Land ownership management – purpose`, `LW Land acquisition – purpose`,
                `MG Minerals and geology – purpose`, `PN Land management planning – purpose`,
                `RD Road management – purpose`, `RG Grazing management – purpose`, `RO Regulations, directives, orders – purpose`,
                `RU Special area management – purpose`, `RW Recreation management – purpose`,
                `SU Special use management – purpose`, `TM Forest products – purpose`, 
                `VM Vegetation management (non-forest products) – purpose`,
                `WF Wildlife, fish, rare plants – purpose`, `WM Water management – purpose`) %>%
  group_by(`SIGNED FY`, FOREST_ID, `DECISION TYPE`) %>%
  summarise(REGION = mean(as.numeric(REGION_ID)),
            mean_assess_time = mean(`ELAPSED DAYS`),
            med_assess_time = median(`ELAPSED DAYS`),
            p_facilities = sum(`FC Facility management – purpose`), 
            p_research = sum(`FR Research – purpose`),
            p_haz_fuels = sum(`HF Fuels management – purpose`),
            p_heritage = sum(`HR Heritage resource management – purpose`),
            p_land_own = sum(`LM Land ownership management – purpose`), 
            p_land_acqui = sum(`LW Land acquisition – purpose`), 
            p_min_geo = sum(`MG Minerals and geology – purpose`), 
            p_land_mngt_plan = sum(`PN Land management planning – purpose`),
            p_road = sum(`RD Road management – purpose`), 
            p_grazing = sum(`RG Grazing management – purpose`),
            p_regulations = sum(`RO Regulations, directives, orders – purpose`),
            p_spec_area = sum(`RU Special area management – purpose`), 
            p_recreation = sum(`RW Recreation management – purpose`), 
            p_spec_use = sum(`SU Special use management – purpose`),
            p_forest_prod = sum(`TM Forest products – purpose`),
            p_veg_mngt = sum(`VM Vegetation management (non-forest products) – purpose`), 
            p_wildlife = sum(`WF Wildlife, fish, rare plants – purpose`), 
            p_water = sum(`WM Water management – purpose`))


nf_pals_df <- left_join(df, pals_df_test, by = "FOREST_ID")

homog <- nf_pals_df %>%
  filter(pct_area_dom_arch >= 70) %>% 
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(class = "Homog", 
         pct_purpose = values/sum(values) * 100)

hetero <- nf_pals_df %>%
  filter(pct_area_dom_arch <= 40) %>% 
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(class = "Hetero", 
         pct_purpose = values/sum(values) * 100)

mid_div <- nf_pals_df %>%
  filter(between(pct_area_dom_arch, 40, 70)) %>% 
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(class = "Moderate", 
         pct_purpose = values/sum(values) * 100)

hetero_df <- rbind(homog, hetero, mid_div)

a1 <-  nf_pals_df %>%
  filter(dom_archetype == 1) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "A", 
         pct_purpose = values/sum(values) * 100)
# strip the p_ off of the names?

a2 <-  nf_pals_df %>%
  filter(dom_archetype == 2) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value, na.rm = TRUE)) %>%
  mutate(archetype = "B", 
         pct_purpose = values/sum(values) * 100)

a3 <-  nf_pals_df %>%
  filter(dom_archetype == 3) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "C", 
         pct_purpose = values/sum(values) * 100)

a4 <-  nf_pals_df %>%
  filter(dom_archetype == 4) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "D", 
         pct_purpose = values/sum(values) * 100)

a5 <-  nf_pals_df %>%
  filter(dom_archetype == 5) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "E", 
         pct_purpose = values/sum(values) * 100)

a6 <-  nf_pals_df %>%
  filter(dom_archetype == 6) %>%
  pivot_longer(cols = starts_with("p_"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "F", 
         pct_purpose = values/sum(values) * 100)

archs <- rbind(a1, a2, a3, a4, a5, a6)

# need to filter out purposes where all archetypes are below 3%
## probably a much easier way to do this, but I got it to work.
dt_archs <- as.data.table(archs)

low_p_A <- dt_archs[pct_purpose < 3 & archetype == "A"]
low_p_B <- dt_archs[pct_purpose < 3 & archetype == "B"]
low_p_C <- dt_archs[pct_purpose < 3 & archetype == "C"]
low_p_D <- dt_archs[pct_purpose < 3 & archetype == "D"]
low_p_E <- dt_archs[pct_purpose < 3 & archetype == "E"]
low_p_F <- dt_archs[pct_purpose < 3 & archetype == "F"]

test_join <- left_join(low_p_A, low_p_B, by = "purpose") %>%
  left_join(., low_p_C, by='purpose') %>%
  left_join(., low_p_D, by='purpose') %>%
  left_join(., low_p_E, by='purpose') %>%
  left_join(., low_p_F, by='purpose') %>%
  drop_na()

for (p in test_join$purpose){
  archs_filt <- archs %>% 
    filter(purpose != p)
}

archs_filt <- archs %>%
  filter(purpose != test_join$purpose[1]) %>%
  filter(., purpose != test_join$purpose[2]) %>%
  filter(., purpose != test_join$purpose[3]) %>%
  filter(., purpose != test_join$purpose[4]) %>%
  filter(., purpose != test_join$purpose[5]) %>%
  filter(., purpose != test_join$purpose[6]) %>%
  filter(., purpose != test_join$purpose[7])

# rename the purposes

archs_filt <- archs_filt %>%
  mutate(purpose_newname = case_when(purpose == "p_facilities" ~ "Facilities", 
                                     purpose == "p_forest_prod" ~ "Forest Products",
                                     purpose == "p_grazing" ~ "Grazing",
                                     purpose == "p_haz_fuels" ~ "Hazardous. Fuels",
                                     purpose == "p_min_geo" ~ "Mining & Geology", 
                                     purpose == "p_recreation" ~ "Recreation",
                                     purpose == "p_road" ~ "Roads",
                                     purpose == "p_spec_use" ~ "Special Use Permits",
                                     purpose == "p_veg_mngt" ~ "Vegetation Management",
                                     purpose == "p_water" ~ "Water",
                                     purpose == "p_wildlife" ~ "Wildlife"))

# plot
archs_purpose_plot <- ggplot(archs_filt, aes(x=purpose_newname, y = pct_purpose, fill = archetype)) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge(), color = "black") +
  scale_fill_discrete(limits=c("A", "B", "C", "D", "E", "F")) + 
  scale_fill_met_d("Hokusai3") +
  ylim(0, 40) +
  xlab("Purpose") + 
  ylab("% Projects with Purpose") +
  guides(fill=guide_legend(title="Archetype")) +
  theme_minimal() + 
  theme_bw() +
  theme(text = element_text(size = 8),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.text = element_text(size = 8)
        )
archs_purpose_plot

#ggsave(here::here(paste0("outputs/plots/archetype_analysis_fig5_testing_", Sys.Date(), ".jpeg")),
#       archs_purpose_plot,  height = 100, width = 140, dpi = 500, units = "mm", device = "jpeg", bg = "white")

## Trying to create a horizontal bar chart
#----------------------------
archs_filt <- archs_filt %>%
  mutate(pct_purpose = round(pct_purpose, 1),
         hghlght = case_when(pct_purpose >= 10 ~ TRUE,
                               pct_purpose < 10 ~ FALSE))

test_archs_purpose_plot <- ggplot(archs_highlight_10, 
                                  aes(x = archetype, 
                                      y = pct_purpose, 
                                      color = purpose_newname,
                                      fill = highlight
                                      )
                                  ) +
  geom_bar(stat = "identity", width = 1, position = position_dodge()) +
  geom_text(aes(label = purpose_newname, vjust = "up")) +
  coord_flip() +
  #scale_fill_discrete(limits=c("A", "B", "C", "D", "E", "F")) + 
  #scale_fill_met_d("Hokusai3") +
  #gghighlight(pct_purpose >= 10.0, keep_scales = TRUE) +
  scale_fill_manual(values = c("grey", "blue4")) +
  ylim(0, 40) +
  xlab("Archetype") + 
  ylab("% Projects with Purpose") +
  guides(fill=guide_legend(title=">10%", reverse = TRUE)) +
  guides(color = guide_legend(reverse = TRUE)) +
  theme_minimal() + 
 # theme_bw() +
  theme(text = element_text(size = 8),
        axis.text.x = element_text(),
        axis.text = element_text(size = 8),
        strip.text.x = element_blank(),     # Removes top (column) facet labels
        strip.background.x = element_blank(),
  ) +
  facet_wrap(~archetype, scales = "free_y", ncol = 1)
test_archs_purpose_plot

archs_highlight_10 <- archs_filt %>%
  mutate(highlight = case_when(pct_purpose >= 10 ~ TRUE, 
                               pct_purpose < 10 ~ FALSE))


# I need to reorder by archetype?
sorted_archs_df <- archs_filt %>% 
  group_by(archetype) %>%
  arrange(desc(pct_purpose), .by_group = TRUE) %>%
  mutate(archetype_num = case_when(archetype == "A" ~ 1,
                                   archetype == "B" ~ 2,
                                   archetype == "C" ~ 3,
                                   archetype == "D" ~ 4,
                                   archetype == "E" ~ 5,
                                   archetype == "F" ~ 6),
                                   
         reord = as.numeric(pct_purpose),
         purpose_newname = fct_reorder(purpose_newname, reord, .desc = FALSE))
# keep looking at this https://stackoverflow.com/questions/72411477/how-to-reorder-bars-by-multiple-variables


sorted_archs_purpose_plot <- ggplot(sorted_archs_df, 
                                    aes(x = archetype, 
                                        y = pct_purpose, 
                                        fill = purpose_newname)) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge(), color = "black") +
  coord_flip() +
  #scale_fill_discrete(limits=c("A", "B", "C", "D", "E", "F")) + 
  #scale_fill_met_d("Hokusai3") +
  ylim(0, 40) +
  xlab("Archetype") + 
  ylab("% Projects with Purpose") +
  guides(fill=guide_legend(title="Purpose")) +
  theme_minimal() + 
  theme_bw() +
  theme(text = element_text(size = 8),
        axis.text.x = element_text(),
        axis.text = element_text(size = 8)
  )
sorted_archs_purpose_plot

sorted_archs_df %>%
  filter(archetype == "A") %>%
  ggplot(aes(x = archetype, y = pct_purpose, fill = purpose_newname)) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge(), color = "black") +
  #geom_col() +
  coord_flip() +
  #scale_fill_discrete(limits=c("A", "B", "C", "D", "E", "F")) + 
  #scale_fill_met_d("Hokusai3") +
  ylim(0, 40) +
  #xlab("Archetype") + 
  ylab("% Projects with Purpose") +
  guides(fill=guide_legend(title="Purpose")) +
  theme_minimal() + 
  theme_bw() +
  theme(text = element_text(size = 8),
        axis.text.x = element_text(),
        axis.text = element_text(size = 8)
  )
#sorted_archs_purpose_plot



ggplot(sorted_archs_df, aes(x = purpose_newname, y = pct_purpose, color = archetype)) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge(), color = "black") +
  coord_flip() +
  #scale_fill_discrete(limits=c("A", "B", "C", "D", "E", "F")) + 
  #scale_fill_met_d("Hokusai3") +
  ylim(0, 40) +
  xlab("Archetype") + 
  ylab("% Projects with Purpose") +
  guides(fill=guide_legend(title="Purpose")) +
  theme_minimal() + 
  theme_bw() +
  theme(text = element_text(size = 8),
        axis.text.x = element_text(),
        axis.text = element_text(size = 8)
  )

## try with ggcharts
#----------------------------------------
archs_filt$pct_purpose <- round(archs_filt$pct_purpose, digits = 2)

# rename the archetypes

archs_filt <- archs_filt %>%
  mutate(archetype = case_when(archetype == "A" ~ "A: Private land-dominated plains", 
                               archetype == "B" ~ "B: Productive forests near urbanized areas",
                               archetype == "C" ~ "C: Old forests in rural areas",
                               archetype == "D" ~ "D: Forest-adjacent systems",
                               archetype == "E" ~ "E: Urban non-forested areas", 
                               archetype == "F" ~ "F: Mountain forests and shrublands"))
                               

chart <- archs_filt %>%
  bar_chart(purpose_newname, pct_purpose, facet = archetype, 
            top_n = 5, label = pct_purpose) +
  geom_text(aes(label = pct_purpose, hjust = 1.2), color = "white", size = 3.25) + 
  labs(
    x = NULL,
    y = "Projects with Purpose (%)"
    #title = "Top 5 Project Purposes for Each Archetype"
  ) +
  theme_classic(base_size = 12) + 
  # theme_bw() +
  theme(#text = element_text(size = 12),
        panel.grid = element_blank(),
        axis.text.x = element_text(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        #axis.text = element_text(size = 12),
        #strip.text.x = element_blank(),     # Removes top (column) facet labels
        strip.background.x = element_blank(),
  )
chart
ggsave(here::here(paste0("outputs/plots/_test_redo_archetype_projects_", Sys.Date(), ".png")), 
       width = 12, height = 4, dpi = 300)

chart2 <- archs_filt %>%
  filter(!purpose == "p_spec_use") %>%
  bar_chart(purpose_newname, pct_purpose, facet = archetype, 
            top_n = 5, label = pct_purpose) +
  geom_text(aes(label = pct_purpose, hjust = 1.2), color = "white", size = 3.25) + 
  labs(
    x = NULL,
    y = "Projects with Purpose (%)"
    #title = "Top 5 Project Purposes for Each Archetype"
  ) +
  theme_classic(base_size = 12) + 
  # theme_bw() +
  theme(#text = element_text(size = 12),
    panel.grid = element_blank(),
    axis.text.x = element_text(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    #axis.text = element_text(size = 12),
    #strip.text.x = element_blank(),     # Removes top (column) facet labels
    strip.background.x = element_blank(),
  )
chart2
ggsave(here::here(paste0("outputs/plots/_test_redo_archetype_projects_no_spec_use_",
                         Sys.Date(), ".png")), 
       width = 14, height = 4, dpi = 300)


chart3 <- archs_filt %>%
  bar_chart(purpose_newname, pct_purpose, facet = archetype, 
            label = pct_purpose, highlight = pct_purpose > 10) +
  geom_text(aes(label = pct_purpose, hjust = 1.2), color = "white") + 
  labs(
    x = NULL,
    y = "Projects with Purpose (%)",
    title = "Top 5 Project Purposes"
  ) +
  theme_minimal(base_size = 14) + 
  # theme_bw() +
  theme(#text = element_text(size = 12),
    axis.text.x = element_text(),
    #axis.text = element_text(size = 12),
    #strip.text.x = element_blank(),     # Removes top (column) facet labels
    strip.background.x = element_blank(),
  )
chart3

# Add boxplots underneath the bar charts
test_pct_year_nf <- pals_df_test %>%
  group_by(FOREST_ID, `SIGNED FY`) %>%
  summarise(tot_spec_use = sum(p_spec_use),
            tot_for_prod = sum(p_forest_prod),
            tot_rec = sum(p_recreation),
            tot_haz_fuel = sum(p_haz_fuels),
            tot_wlife = sum(p_wildlife),
            tot_geo = sum(p_min_geo),
            tot_veg_mngt = sum(p_veg_mngt),
            tot_water = sum(p_water),
            tot_all_proj = sum(c_across(starts_with("p_")))) %>%
  mutate(pct_spec_use = (tot_spec_use / tot_all_proj)*100,
         pct_for_prod = (tot_for_prod / tot_all_proj)*100,
         pct_rec = (tot_rec / tot_all_proj)*100, 
         pct_haz_fuel = (tot_haz_fuel / tot_all_proj)*100,
         pct_wlife = (tot_wlife / tot_all_proj)*100,
         pct_geo = (tot_geo / tot_all_proj)*100,
         pct_water = (tot_water / tot_all_proj)*100,
         pct_veg_mngt = (tot_veg_mngt / tot_all_proj)*100) %>%
  drop_na()

test_join <- right_join(test_pct_year_nf, nepa_summ_df, by = c("FOREST_ID" = "forest_num"))
test_join <- test_join %>%
  mutate(dom_archetype = case_when(dom_archetype == 1 ~ "A",
                                   dom_archetype == 2 ~ "B",
                                   dom_archetype == 3 ~ "C",
                                   dom_archetype == 4 ~ "D",
                                   dom_archetype == 5 ~ "E",
                                   dom_archetype == 6 ~ "F"))

for_prod_box <- test_join %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(dom_archetype), pct_for_prod), notch = FALSE) + 
  ylim(0, 100) + 
  theme_minimal() + 
  #theme(axis.title.x = element_blank()) + 
  labs(subtitle = "Forest Products",
       x = " ", 
       y = " ")
for_prod_box

rec_box <- test_join %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(dom_archetype), pct_rec), notch = FALSE) + 
  ylim(0, 100) + 
  theme_minimal() + 
  labs(subtitle = "Recreation", 
       x = "Dominant Cluster", 
       y = "Project Purpose (%)")
rec_box

haz_fules_box <- test_join %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(dom_archetype), pct_haz_fuel), notch = FALSE) + 
  ylim(0, 100) + 
  theme_minimal() + 
  labs(subtitle = "Hazardous Fuels", 
       x = "Dominant Cluster", 
       y = "Project Purpose (%)")
haz_fules_box

veg_mngt_box <- test_join %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(dom_archetype), pct_veg_mngt), notch = FALSE) + 
  ylim(0, 100) + 
  theme_minimal() + 
  labs(subtitle = "Veg. Management",
       x = "Dominant Cluster", 
       y = "Project Purpose (%)")
veg_mngt_box

wildlife_box <- test_join %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(dom_archetype), pct_wlife), notch = FALSE) + 
  ylim(0, 100) + 
  theme_minimal() + 
  labs(subtitle = "Wildlife",
       x = "Dominant Cluster",
       y = " ")
wildlife_box

water_box <- test_join %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(dom_archetype), pct_water), notch = FALSE) + 
  ylim(0, 100) + 
  theme_minimal() + 
  labs(subtitle = "Water",
       x = "Dominant Cluster", 
       y = "Project Purpose (%)")
water_box

mine_box <- test_join %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(dom_archetype), pct_geo), notch = FALSE) + 
  ylim(0, 100) + 
  theme_minimal() + 
  labs(subtitle = "Mining or Geology",
       x = " ", 
       y = "Project Purpose (%)")
mine_box

(chart2) / 
  (free(mine_box | for_prod_box, type = "label") /
  free(veg_mngt_box | wildlife_box, type = "label")) +
  plot_layout(heights = unit(c(6, 6), c('cm', 'null'))) +
  plot_annotation(tag_levels = 'A')

cluster_val_fig <- (chart2) / 
  (free(mine_box | for_prod_box, type = "label") /
     free(veg_mngt_box | wildlife_box, type = "label")) +
  plot_layout(heights = unit(c(6, 6), c('cm', 'null'))) +
  plot_annotation(tag_levels = 'A')

ggsave(here::here(paste0("outputs/plots/archetype_analysis_fig3_testing_", Sys.Date(), ".png")), 
       plot = cluster_val_fig, width = 13, height = 8, dpi = 300)

# Test with the diversity class (hetero, homog, mid)
#-----------------------------------------------------

hetero_df <- hetero_df %>%
  mutate(purpose_newname = case_when(purpose == "p_facilities" ~ "Facilities", 
                                     purpose == "p_forest_prod" ~ "Forest Products",
                                     purpose == "p_grazing" ~ "Grazing",
                                     purpose == "p_haz_fuels" ~ "Hazardous. Fuels",
                                     purpose == "p_min_geo" ~ "Mining & Geology", 
                                     purpose == "p_recreation" ~ "Recreation",
                                     purpose == "p_road" ~ "Roads",
                                     purpose == "p_spec_use" ~ "Special Use Permits",
                                     purpose == "p_veg_mngt" ~ "Vegetation Management",
                                     purpose == "p_water" ~ "Water",
                                     purpose == "p_wildlife" ~ "Wildlife"))

hetero_df <- hetero_df %>%
  mutate(pct_purpose = round(pct_purpose, 2))

# plot
hetero_purpose_plot <- ggplot(hetero_df, aes(x=purpose_newname, y = pct_purpose, fill = class)) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge(), color = "black") +
  #scale_fill_discrete(limits=c("A", "B", "C", "D", "E", "F")) + 
  #scale_fill_met_d("Hokusai3") +
  ylim(0, 40) +
  xlab("Purpose") + 
  ylab("% Projects with Purpose") +
  guides(fill=guide_legend(title="Diversity Class")) +
  theme_minimal() + 
  theme_bw() +
  theme(text = element_text(size = 8),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.text = element_text(size = 8)
  )
hetero_purpose_plot

chart3 <- hetero_df %>%
  bar_chart(purpose_newname, pct_purpose, facet = class, 
            top_n = 5, label = pct_purpose) +
  geom_text(aes(label = pct_purpose, hjust = 1.2), color = "white", size = 3.25) + 
  labs(
    x = NULL,
    y = "Projects with Purpose (%)"
    #title = "Top 5 Project Purposes for Each Archetype"
  ) +
  theme_classic(base_size = 12) + 
  # theme_bw() +
  theme(#text = element_text(size = 12),
    panel.grid = element_blank(),
    axis.text.x = element_text(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    #axis.text = element_text(size = 12),
    #strip.text.x = element_blank(),     # Removes top (column) facet labels
    strip.background.x = element_blank(),
  )
chart3




# Look into SUP
#-----------------------------------

pals_df_sup <- pals_df %>%
  filter(as.Date(`INITIATION DATE`, format = "%m/%d/%Y") >= "2009-01-01") %>%
  filter(REGION_ID != "10" & REGION_ID != "13" & REGION_ID != "24" & REGION_ID != "00") %>%
  filter(`SU Special use management – purpose` == 1)
