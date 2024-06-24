library(tidyverse)
library(scales)
library(patchwork)
library(foreach)
library(geofacet)
library(data.table)
library(clipr)
library(tidycensus)

setwd("YOUR-FIGURE-FOLDER-HERE")

########## Determine impacts of filters #################################
wetlands_pre_filter <- readr::read_csv("E:/EDF/wotus_wetlands/analysis/results/conus/Tables/post_selected_pre_filtered_wetlands.csv")
wetlands_post_filter <- readr::read_csv("E:/EDF/wotus_wetlands/analysis/results/conus/Tables/post_selected_post_3factor_wetlands.csv")
wetlands_post_imp_filter <- readr::read_csv("E:/EDF/wotus_wetlands/analysis/results/conus/Tables/post_selected_post_3factor_and_impfilter_wetlands.csv")

# Vegetated wetlands - pre-filter for 3-factor
pre_filtered_area <- wetlands_pre_filter %>% 
  filter(WETLAND_TYPE != "Lake",
         WETLAND_TYPE != "Riverine",
         WETLAND_TYPE != "Estuarine and Marine Deepwater") %>% 
  group_by(STATE) %>% 
  summarize(area = sum(ACRES, na.rm=T)) 

post_3factor_filtered_area <- wetlands_post_filter %>% 
  group_by(STATE) %>% 
  summarize(area = sum(ACRES, na.rm=T)) 

post_3factor_and_imp_filtered_area <- wetlands_post_imp_filter %>% 
  group_by(STATE) %>% 
  summarize(area = sum(ACRES, na.rm=T)) 

# Copy table for supplemental
pre_filtered_area %>% 
  rename(pre_area = area) %>% 
  left_join(post_3factor_filtered_area, by = c("STATE")) %>% 
  rename(three_factor_area = area) %>% 
  mutate(perc_kept_3factor = three_factor_area/pre_area * 100) %>% 
  clipr::write_clip()

######### Read wetlands from Dryad #####################
wetlands_dt  <-  readr::read_csv("E:/EDF/wotus_wetlands/analysis/results/conus/Tables/wetlands_for_dryad.csv") %>% as.data.table()

######### Create state summaries #######################
create_state_wetland_summary <- function(x, water_regime_cutoff = 1, states = c("NC", "VA")){
  wr_variable <- paste0("wr_",water_regime_cutoff, "_cut_flow_class")
  cat(water_regime_cutoff,"\n")
  state_summaries <- foreach(i = states, .combine = "bind_rows") %do% {
    cat(i,"\n")
    state_nt_sum <- x %>% 
      filter(STATE == i) %>% 
      filter(type == "nontidal") %>% 
      pull(ACRES) %>% 
      sum(na.rm=T)
    
      x %>% 
        filter(STATE == i) %>% 
        filter(type == "nontidal") %>% 
        filter(!!sym("water_regime_number") >  water_regime_cutoff) %>% 
        group_by(!!sym(wr_variable)) %>% 
        summarize(area = sum(ACRES, na.rm=T),
                  wr_cut = water_regime_cutoff) %>% 
        pivot_wider(names_from = !!sym(wr_variable), values_from = area, names_prefix = "fc_", values_fill = 0) %>% 
        mutate(total_nt = state_nt_sum) %>% 
        add_column(state = i, .before = 1) 
  }
  return(state_summaries)
}

states <-  wetlands_dt$STATE %>% unique() %>% sort()

grouped_summaries <- foreach(i = states, .combine = "bind_rows") %do% {
  cat(i,"\n")
  state_nt_sum <- wetlands_dt %>% 
    filter(STATE == i) %>% 
    filter(type == "nontidal") %>% 
    pull(ACRES) %>% 
    sum(na.rm=T)
  
    wetlands_dt %>% 
    filter(STATE == i) %>% 
      filter(type == "nontidal") %>% 
      group_by(group_flow_class) %>% 
    summarize(area = sum(ACRES, na.rm=T),
              wr_cut = 0) %>% 
    pivot_wider(names_from = group_flow_class, values_from = area, names_prefix = "fc_", values_fill = 0) %>% 
    mutate(total_nt = state_nt_sum) %>% 
    add_column(state = i, .before = 1)
}

full_summary_table <- foreach(i = 1:7, .combine = "bind_rows") %do% {
  bind_rows(
    create_state_wetland_summary(wetlands_dt, water_regime_cutoff = i, states = wetlands_dt$STATE %>% unique() %>% sort())
  )
} %>% 
  bind_rows(grouped_summaries)

full_summary_table %>% write_rds("conus_summary_table.rds")
# full_summary_table2 <- read_rds("conus_summary_table.rds")

############# Plots ###########################

############### Figure 1 ##################
# Total non-tidal wetland area
total_nt <- full_summary_table %>%
  replace(is.na(.), 0) %>%
  mutate(
    low_est = (total_nt - (fc_1 + fc_2 + fc_3)),
    mid_est = (total_nt - (fc_1 + fc_2)),
    high_est = (total_nt - (fc_1))
  ) %>%
  group_by(wr_cut) %>%
  summarize(
    fc_1 = sum(fc_1, na.rm = T),
    fc_2 = sum(fc_2, na.rm = T),
    fc_3 = sum(fc_3, na.rm = T),
    fc_4 = sum(fc_4, na.rm = T),
    fc_5 = sum(fc_5, na.rm = T),
    fc_6 = sum(fc_6, na.rm = T),
    total_nt = sum(total_nt, na.rm = T)
  ) %>%
  mutate(
    low_perc_est = (total_nt - (fc_1 + fc_2 + fc_3)) / total_nt * 100,
    mid_perc_est = (total_nt - (fc_1 + fc_2)) / total_nt * 100,
    high_perc_est = (total_nt - (fc_1)) / total_nt * 100
  ) %>%
  ungroup() %>%
  slice(1) %>%
  pull(total_nt)

national_data <- full_summary_table %>%
  replace(is.na(.), 0) %>%
  group_by(wr_cut) %>%
  summarize(
    fc_1 = sum(fc_1, na.rm = T),
    fc_2 = sum(fc_2, na.rm = T),
    fc_3 = sum(fc_3, na.rm = T),
    fc_4 = sum(fc_4, na.rm = T),
    fc_5 = sum(fc_5, na.rm = T),
    fc_6 = sum(fc_6, na.rm = T),
    total_nt = sum(total_nt, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(total_nt = max(total_nt, na.rm = T)) %>%
  group_by(wr_cut) %>%
  mutate(
    low_perc_est = (total_nt - (fc_1 + fc_2 + fc_3)) / total_nt * 100,
    mid_perc_est = (total_nt - (fc_1 + fc_2)) / total_nt * 100,
    high_perc_est = (total_nt - (fc_1)) / total_nt * 100
  ) %>%
  mutate(
    low_est = (total_nt - (fc_1 + fc_2 + fc_3)),
    mid_est = (total_nt - (fc_1 + fc_2)),
    high_est = (total_nt - (fc_1))
  ) %>%
  mutate(wr_cut = factor(wr_cut))


all_states_acres <- national_data %>%
  ggplot() +
  geom_pointrange(
    aes(
      x = wr_cut,
      ymax = high_perc_est / 100,
      y = mid_perc_est / 100,
      ymin = low_perc_est / 100
    ),
    lwd = 0.75
  ) + 
  theme_classic() +
  theme(
    axis.line = element_line(),
    text = element_text(size = 8, family = "Arial"),
    panel.border = element_rect(fill = NA, colour = "grey20"),
    panel.grid = element_line(colour = "grey92"),
    panel.grid.minor.y = element_line(linewidth = rel(0.5)),
    panel.grid.major.y = element_line(linewidth = rel(1))
  ) +
  theme(
    strip.background = element_rect(fill = NA, color = NA),
    strip.text.x = element_text(face = "bold", angle = 0, size = 12),
    panel.grid = element_line(colour = "grey92"),
    panel.grid.minor.y = element_line(linewidth = 0.25),
    panel.grid.major.y = element_line(linewidth = 1),
    panel.grid.major.x = element_line(linewidth = 1)
  ) +
  ylab("Nontidal wetland area estimated\nnon-jurisdictional (%)") +
  xlab("Minimum wetland water regime") +
  scale_x_discrete(
    breaks = c(0, 1, 2, 3, 4, 5, 6, 7),
    labels = c(
      "None",
      "Seasonally\nsaturated",
      "Continuously\nsaturated",
      "Seasonally\nflooded",
      "Seasonally\nflooded/saturated",
      "Semi-permanently\nflooded",
      "Intermittently\nexposed",
      "Permanently\nflooded"
    )
  ) + 
  theme(
    axis.text.x = element_text(
      angle = 45,
      size = 6,
      hjust = 1
    ),
    panel.grid.major.x = element_blank()
  ) +
  geom_segment(
    aes(
      x = 2,
      xend = 8,
      y = .04,
      yend = .04
    ),
    lwd = 0.5,
    arrow = arrow(
      angle = 30,
      length = unit(0.1, "inches"),
      ends = "last",
      type = "closed"
    ),
    color = "#383838"
  ) +
  geom_text(
    aes(x = 5, y = 0.04),
    label = 'Excludes "drier" wetlands',
    vjust = -.5,
    color = "#383838"
  ) +
  scale_y_continuous(
    labels = label_percent(),
    sec.axis = sec_axis(
      name = "Acres",
      trans = ~ . * total_nt,
      labels = scales::label_number(scale_cut = cut_short_scale())
    ),
    limits = c(0, NA)
  ) +
  theme(text = element_text(family = "Arial"))

all_states_acres

ggsave(
  "figure_1.png",
  all_states_acres,
  width = 120,
  height = 90,
  units = "mm",
  dpi = 300,
  bg = "transparent"
)  

ggsave(
  "production_figures/figure_1.eps",
  all_states_acres,
  width = 120,
  height = 90,
  units = "mm",
  device = cairo_ps,
  bg = "white"
)  
############ Figure 2 ####################
state_polygons <- get_acs(
  geography = "state",
  variables = "B01002_001",
  year = 2019,
  survey = "acs1",
  geometry = TRUE
)

state_protection_status <-
  sf::st_read("E:/EDF/wotus_wetlands/analysis/results/conus/boundaries.gdb",
              layer = "states_proj") %>%
  as_tibble() %>%
  select(-SHAPE) %>%
  left_join(state_polygons %>% sf::st_transform(sf::st_crs( sf::st_read("E:/EDF/wotus_wetlands/analysis/results/conus/boundaries.gdb", layer = "states_proj"))) %>% select(NAME, geometry))
   
  
percent_map <- full_summary_table %>%
  replace(is.na(.), 0) %>%
  mutate(
    low_perc_est = (total_nt - (fc_1 + fc_2 + fc_3)) / total_nt * 100,
    mid_perc_est = (total_nt - (fc_1 + fc_2)) / total_nt * 100,
    high_perc_est = (total_nt - (fc_1)) / total_nt * 100
  ) %>%
  mutate(
    low_est = (total_nt - (fc_1 + fc_2 + fc_3)),
    mid_est = (total_nt - (fc_1 + fc_2)),
    high_est = (total_nt - (fc_1))
  ) %>%
  mutate(wr_cut = factor(wr_cut)) %>%
  left_join(
    state_protection_status %>% select(STUSPS, state_protections, geometry) %>% mutate(state_protections = factor(state_protections)),
    by = c("state" = "STUSPS")
  ) %>%
  filter(wr_cut %in% c(0, 3, 5)) %>%
  mutate(wr_cut_description = ifelse(
    wr_cut == 0,
    "No minimum wetland\nwater regime",
    ifelse(
      wr_cut == 3,
      "Wetlands must be\n'Seasonally flooded'\nor wetter",
      "Wetlands must be\n'Semi-permanently\nflooded' or wetter"
    )
  )) %>%
  mutate(mid_perc_est_binned = cut(mid_perc_est, breaks = c(0, 20, 40, 60, 80, 100))) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = mid_perc_est), color = "white") +
  facet_wrap(. ~ wr_cut_description) +
  scale_fill_fermenter(
    palette = "OrRd",
    direction = 1,
    name = "Non-tidal wetland area estimated\nnon-jurisdictional (%)",
    breaks = c(0, 20, 40, 60, 80, 100),
    limits = c(0, 100)
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.justification = "right",
    legend.key.width = unit(0.75, "cm"),
    legend.key.height = unit(0.35, "cm"),
    legend.margin = margin(4, 4, 4, 4),
    text = element_text(family = "Arial"),
    strip.text = element_text(size = 7),
    legend.title = element_text(
      family = "Arial",
      size = 8,
      vjust = 1,
      face = "bold"
    ),
    legend.text = element_text(size = 7)
  )
  

area_map <- full_summary_table %>%
  replace(is.na(.), 0) %>%
  mutate(
    low_perc_est = (total_nt - (fc_1 + fc_2 + fc_3)) / total_nt * 100,
    mid_perc_est = (total_nt - (fc_1 + fc_2)) / total_nt * 100,
    high_perc_est = (total_nt - (fc_1)) / total_nt * 100
  ) %>%
  mutate(
    low_est = (total_nt - (fc_1 + fc_2 + fc_3)),
    mid_est = (total_nt - (fc_1 + fc_2)),
    high_est = (total_nt - (fc_1))
  ) %>%
  mutate(wr_cut = factor(wr_cut)) %>%
  left_join(
    state_protection_status %>% select(STUSPS, state_protections, geometry) %>% mutate(state_protections = factor(state_protections)),
    by = c("state" = "STUSPS")
  ) %>%
  filter(wr_cut %in% c(0, 3, 5)) %>%
  mutate(wr_cut_description = ifelse(
    wr_cut == 0,
    "No minimum wetland\nwater regime",
    ifelse(
      wr_cut == 3,
      "Wetlands must be\n'Seasonally flooded'\nor wetter",
      "Wetlands must be\n'Semi-permanently\nflooded' or wetter"
    )
  )) %>%
  mutate(centroids = sf::st_point_on_surface(geometry)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = mid_est), color = "white") +
  facet_wrap(. ~ wr_cut_description) +
  scale_fill_fermenter(
    palette = "RdPu",
    breaks = c(0, 1E6, 2E6, 3E6, 4E6, 6E6, 9E6),
    direction = 1,
    labels = label_number(scale_cut = cut_short_scale()),
    name = "Non-tidal wetland area estimated\nnon-jurisdictional (Acres)",
    limits = c(0, 10637866)
  ) +
  scale_color_manual(values = c("white", "black")) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.justification = "right",
    legend.key.width = unit(0.75, "cm"),
    legend.key.height = unit(0.35, "cm"),
    legend.margin = margin(4, 4, 4, 4),
    text = element_text(family = "Arial"),
    strip.text = element_text(size = 7),
    legend.title = element_text(
      family = "Arial",
      size = 8,
      vjust = 1,
      face = "bold"
    ),
    legend.text = element_text(size = 7))
  
protections_map <- full_summary_table %>%
  replace(is.na(.), 0) %>%
  mutate(
    low_perc_est = (total_nt - (fc_1 + fc_2 + fc_3)) / total_nt * 100,
    mid_perc_est = (total_nt - (fc_1 + fc_2)) / total_nt * 100,
    high_perc_est = (total_nt - (fc_1)) / total_nt * 100
  ) %>%
  mutate(
    low_est = (total_nt - (fc_1 + fc_2 + fc_3)),
    mid_est = (total_nt - (fc_1 + fc_2)),
    high_est = (total_nt - (fc_1))
  ) %>%
  mutate(wr_cut = factor(wr_cut)) %>%
  left_join(
    state_protection_status %>% select(STUSPS, state_protections, geometry) %>% mutate(state_protections = factor(state_protections)),
    by = c("state" = "STUSPS")
  ) %>%
  filter(wr_cut == 0) %>%
  mutate(centroids = sf::st_point_on_surface(geometry)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = state_protections), color = "white") +
  facet_wrap(. ~ wr_cut) +
  scale_fill_manual(
    values = c("#cccccc", "#9ecae1", "#3182bd"),
    labels = c("None", "Limited", "Broad"),
    name = "Coverage of state\nwetlands protections"
  ) +
  theme_void() +
  theme(
    strip.text = element_blank(),
    legend.position = "right",
    legend.justification = "right",
    text = element_text(family = "Arial"),
    title = element_text(size = 8, vjust = 1, face = "bold"),
    plot.caption = element_text(
      family = "Arial",
      size = 6,
      face = "plain"
    ),
    legend.text = element_text(size = 7)
  ) +
  labs(caption = "Modified from Kihslinger et al., 2023")
  
full_map <-
  percent_map / area_map / free(protections_map)  + plot_layout(heights = c(1, 1, 1)) + plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 10, face = "bold", family = "Arial"))

# Save as png
ggsave(
  "full_map.png",
  full_map,
  width = 120,
  height = 150,
  units = "mm",
  dpi = 300,
  bg = "white"
)  

# Save as eps
ggsave(
  "production_figures/figure_2.eps",
  full_map,
  width = 120,
  height = 150,
  units = "mm",
  device = cairo_ps,
  bg = "white"
)  

################  Table S6  ######################
full_summary_table %>%
  replace(is.na(.), 0) %>%
  transmute(
    State = state,
    Scenario = wr_cut,
    low_perc_est = (total_nt - (fc_1 + fc_2 + fc_3)) / total_nt * 100,
    mid_perc_est = (total_nt - (fc_1 + fc_2)) / total_nt * 100,
    high_perc_est = (total_nt - (fc_1)) / total_nt * 100,
    low_est = (total_nt - (fc_1 + fc_2 + fc_3)),
    mid_est = (total_nt - (fc_1 + fc_2)),
    high_est = (total_nt - (fc_1))
  ) %>%
  left_join(
    state_protection_status %>% select(STUSPS, state_protections) %>% mutate(state_protections = factor(state_protections)),
    by = c("State" = "STUSPS")
  ) %>%
  filter(Scenario == 3) %>%
  arrange(-mid_est) %>%
  select(
    State,
    state_protections,
    low_est,
    low_perc_est,
    mid_est,
    mid_perc_est,
    high_est,
    high_perc_est
  ) %>%
  clipr::write_clip()


############## Figure 3 #############################
state_protection_status <- sf::st_read("E:/EDF/wotus_wetlands/analysis/results/conus/boundaries.gdb", layer = "states_proj") %>% 
  as_tibble()

wetlands_nt_ownership <- wetlands_dt %>% 
  filter(type == "nontidal") %>% 
  left_join(state_protection_status %>% select(STUSPS, state_protections) %>% mutate(state_protections = factor(state_protections)), by = c("STATE" = "STUSPS")) %>% 
  mutate(public = !is.na(Mang_Type))

wetlands_nt_gap_plot_data <-
  wetlands_nt_ownership %>%
  mutate(jurisdictional = (group_flow_class < 3)) %>%
  group_by(public, jurisdictional, GAP_Sts, state_protections) %>%
  summarize(area = sum(ACRES, na.rm = T)) %>%
  mutate(wr = 0) %>% 
  bind_rows(wetlands_nt_ownership %>%
              mutate(jurisdictional = wr_1_cut_flow_class < 3) %>%
              group_by(public, jurisdictional, GAP_Sts, state_protections) %>%
              summarize(area = sum(ACRES, na.rm = T)) %>%
              mutate(wr = 1)) %>% 
  bind_rows(wetlands_nt_ownership %>%
              mutate(jurisdictional = wr_2_cut_flow_class < 3) %>%
              group_by(public, jurisdictional, GAP_Sts, state_protections) %>%
              summarize(area = sum(ACRES, na.rm = T)) %>%
              mutate(wr = 2)) %>% 
  bind_rows(wetlands_nt_ownership %>%
              mutate(jurisdictional = wr_3_cut_flow_class < 3) %>%
              group_by(public, jurisdictional, GAP_Sts, state_protections) %>%
              summarize(area = sum(ACRES, na.rm = T)) %>%
              mutate(wr = 3)) %>% 
  bind_rows(wetlands_nt_ownership %>%
              mutate(jurisdictional = wr_4_cut_flow_class < 3) %>%
              group_by(public, jurisdictional, GAP_Sts, state_protections) %>%
              summarize(area = sum(ACRES, na.rm = T)) %>%
              mutate(wr = 4)) %>% 
  bind_rows(wetlands_nt_ownership %>%
              mutate(jurisdictional = wr_5_cut_flow_class < 3) %>%
              group_by(public, jurisdictional, GAP_Sts, state_protections) %>%
              summarize(area = sum(ACRES, na.rm = T)) %>%
              mutate(wr = 5)) %>% 
  bind_rows(wetlands_nt_ownership %>%
              mutate(jurisdictional = wr_6_cut_flow_class < 3) %>%
              group_by(public, jurisdictional, GAP_Sts, state_protections) %>%
              summarize(area = sum(ACRES, na.rm = T)) %>%
              mutate(wr = 6)) %>% 
  bind_rows(wetlands_nt_ownership %>%
              mutate(jurisdictional = wr_7_cut_flow_class < 3) %>%
              group_by(public, jurisdictional, GAP_Sts, state_protections) %>%
              summarize(area = sum(ACRES, na.rm = T)) %>%
              mutate(wr = 7)
              )

gs <- tibble("GAP_Sts" = c(1,2,3,4, NA),
                       "gap_status" = c("1","2","3","4", "NP"))

gap_status <- gs %>%
  mutate(gap_status = factor(
    gap_status,
    levels = gs %>%
      filter(!is.na(GAP_Sts)) %>%
      arrange(gap_status) %>%
      bind_rows(gs %>%
                  filter(is.na(GAP_Sts))) %>%
      pull(gap_status)
  ))


state_split <- wetlands_nt_gap_plot_data %>% 
  filter(jurisdictional == F) %>%
  left_join(gap_status) %>% 
  mutate(state_protections = ifelse(state_protections == 0,"None",
                                    ifelse(state_protections == 1, "Limited", "Broad"))) %>% 
  mutate(wr = factor(wr)) %>% 
  group_by(state_protections, wr) %>% 
  summarize(area = sum(area)) %>% 
  ggplot()+
  geom_col(aes(x=wr, y = area/total_nt, fill = state_protections)) +
  ylab("Non-tidal wetland area estimated\nnon-jurisdictional (%)")+
  
  scale_y_continuous(labels = label_percent(), sec.axis = sec_axis(name = "Acres", trans = ~.*total_nt, labels = scales::label_number(scale_cut = cut_short_scale())))+
  scale_fill_manual(values = rev(c("#cccccc","#9ecae1", "#3182bd")),na.translate = F)+
  theme_classic()+
  theme(text = element_text(size = 8, family = "Arial"),
        strip.background = element_rect(fill = NA, color = NA),
        strip.text.x = element_text(face = "bold", angle = 0, size = 12))+
  guides(fill=guide_legend(title="Coverage of state\nwetlands protections"))+
  theme(axis.text.x = element_text(angle = 0, size = 6),
        legend.position = c(0.05, 0.85), legend.justification = "left")+
  xlab("Minimum wetland water regime")+
  scale_alpha_manual(values = c(0.3, 0.5, 0.7, 0.85, 1))+
  scale_x_discrete(breaks = c(0,1,2,3,4,5,6,7), labels = c("None", "Seasonally\nsaturated", "Continuously\nsaturated", "Seasonally\nflooded", "Seasonally\nflooded/saturated", "Semi-permanently\nflooded", "Intermittently\nexposed", "Permanently\nflooded"))+#,"G","H"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        legend.key.height = unit(0.5, "cm"),
        legend.text = element_text(size = 7),
        legend.title = element_text(size=8, face = "bold"),
        legend.background = element_blank())

gap_split <- wetlands_nt_gap_plot_data %>% 
  filter(jurisdictional == F, state_protections == 0) %>%
  left_join(gap_status) %>% 
  mutate(state_protections = ifelse(state_protections == 0,"No state\nprotections",
                                    ifelse(state_protections == 1, "Limited\nstate protections", "Broad state\nprotections"))) %>% 
  mutate(wr = factor(wr)) %>% 
  group_by(gap_status, wr) %>% 
  summarize(area = sum(area)) %>% 
  ggplot()+
  geom_col(aes(x=wr, y = area/total_nt, fill = gap_status)) +
  ylab("Non-tidal wetland area estimated\nnon-jurisdictional (%)")+
  scale_x_discrete(breaks = c(0,1,2,3,4,5,6,7), labels = c("None", "Seasonally\nsaturated", "Continuously\nsaturated", "Seasonally\nflooded", "Seasonally\nflooded/saturated", "Semi-permanently\nflooded", "Intermittently\nexposed", "Permanently\nflooded"))+#,"G","H"))+
  scale_y_continuous(labels = label_percent(), sec.axis = sec_axis(name = "Acres", trans = ~.*total_nt, labels = scales::label_number(scale_cut = cut_short_scale())))+
  scale_fill_grey(end = 0.85)+
  theme_classic()+
  theme(text = element_text(size = 8, family = "Arial"),
        strip.background = element_rect(fill = NA, color = NA),
        strip.text.x = element_text(face = "bold", angle = 0, size = 12))+
  guides(fill=guide_legend(title="GAP Status"))+
  theme(axis.text.x = element_text(angle = 45, size = 6, hjust = 1),
        legend.position = c(0.05, 0.8), 
        legend.justification = "left",
        legend.key.height = unit(0.35, "cm"),
        legend.text = element_text(size = 7),
        legend.title = element_text(size=8, face = "bold"),
        legend.background = element_blank())+
  xlab("Minimum wetland water regime")
  

gap_fig <- state_split + gap_split + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 10, face = "bold", family = "Arial"))

ggsave("production_figures/figure_3.eps", 
       gap_fig,
       width = 180,
       height = 90,
       units = "mm",
       device = cairo_ps)  

ggsave("figure_3.png", 
       gap_fig,
       width = 180,
       height = 90,
       units = "mm")  

# Range for unprotected area (no state of parcel protections) if GAP statuses of 3 and 4 aren't considered "protective"
wetlands_nt_gap_plot_data %>% 
  filter(jurisdictional == F, state_protections == 0) %>%
  left_join(gap_status) %>% 
  mutate(state_protections = ifelse(state_protections == 0,"No state\nprotections",
                                    ifelse(state_protections == 1, "Limited\nstate protections", "Broad state\nprotections"))) %>% 
  mutate(wr = factor(wr)) %>% 
  group_by(gap_status %in% c(1,2), wr) %>% 
  summarize(area = sum(area)) %>% 
  filter(`gap_status %in% c(1, 2)` == F) %>% 
  left_join(wetlands_nt_gap_plot_data %>% 
              filter(jurisdictional == F) %>%
              left_join(gap_status) %>% 
              mutate(state_protections = ifelse(state_protections == 0,"None",
                                                ifelse(state_protections == 1, "Limited", "Broad"))) %>% 
              mutate(wr = factor(wr)) %>% 
              group_by(wr) %>% 
              summarize(total_area = sum(area))) %>% 
  mutate(perc = area/total_area * 100)

##################### Figure 4 ##########################
calc_group_wetland_patch_size <- function(x){
  cat("wr = ", 0,"\n")
  dt <- data.table::as.data.table(x)
  y <- dt[dt$type == "nontidal",]
  y[,"jurisdiction"] <-  y$group_flow_class < 3
  y[,"total_area" := sum(ACRES, na.rm=T), by = c("GROUP_ID", "jurisdiction")] 
  y[,"group_acres_binned"] <- cut(y$total_area, breaks = c(0,0.25, 0.5, 1, 2, 3, 4, 5, 10, 20, 30,50,100, 10000000))
  y[,"acres_binned"] <- cut(y$ACRES, breaks = c(0,0.25, 0.5, 1, 2, 3, 4, 5, 10, 20, 30,50,100, 10000000))
  
  y %>%
    as_tibble() %>%
    select(jurisdiction, total_area, acres_binned) %>%
    distinct() %>% 
    group_by(acres_binned, jurisdiction) %>%
    summarize(
      count = n(),
      total_area = sum(total_area, na.rm=T),
    ) %>%
    group_by(acres_binned) %>% 
    transmute(jurisdiction = jurisdiction,
              acres_binned = acres_binned,
              perc = count/sum(count) * 100,
              count_nj = count,
              perc_area = total_area / sum(total_area) * 100,
              area_nj = total_area,
              wr = 0,
              grouped = F) %>% 
    bind_rows(y %>%
                as_tibble() %>%
                select(jurisdiction, total_area, group_acres_binned) %>%
                distinct() %>% 
                group_by(group_acres_binned, jurisdiction) %>%
                summarize(
                  count = n(),
                  total_area = sum(total_area, na.rm=T),
                ) %>%
                group_by(group_acres_binned) %>% 
              transmute(jurisdiction = jurisdiction,
                        acres_binned = group_acres_binned,
                        perc = count/sum(count) * 100,
                        count_nj = count,
                        perc_area = total_area / sum(total_area) * 100,
                        area_nj = total_area,
                        wr = 0,
                        grouped = T) %>% 
                ungroup() %>% 
                select(-group_acres_binned))
    
}

calc_wetland_patch_size <- function(x, wr = 1){
  cat("wr = ", wr,"\n")
  dt <- data.table::as.data.table(x)
  y <- dt[dt$type == "nontidal",]
  y[,"jurisdiction"] <-  y[[paste0("wr_", wr, "_cut_flow_class")]] < 3
  y[,"total_area" := sum(ACRES, na.rm=T), by = c(paste0("wr_",wr,"_cut_GROUP_ID"), "jurisdiction")] 
  y[,"group_acres_binned"] <- cut(y$total_area, breaks = c(0,0.25, 0.5, 1, 2, 3, 4, 5, 10, 20, 30,50,100, 10000000))
  y[,"acres_binned"] <- cut(y$ACRES, breaks = c(0,0.25, 0.5, 1, 2, 3, 4, 5, 10, 20, 30,50,100, 10000000))
  
  y %>%
    as_tibble() %>%
    select(jurisdiction, total_area, acres_binned) %>%
    distinct() %>%
    group_by(acres_binned, jurisdiction) %>%
    summarize(
      count = n(),
      total_area = sum(total_area, na.rm=T),
    ) %>%
    group_by(acres_binned) %>% 
    transmute(jurisdiction = jurisdiction,
              acres_binned = acres_binned,
              perc = count/sum(count) * 100,
              count_nj = count,
              perc_area = total_area / sum(total_area) * 100,
              area_nj = total_area,
              wr = wr,
              grouped = F) %>% 
    bind_rows(
      y %>%
        as_tibble() %>%
        select(jurisdiction, total_area, group_acres_binned) %>%
        distinct() %>%
        group_by(group_acres_binned,jurisdiction) %>%
        summarize(
          count = n(),
          total_area = sum(total_area, na.rm=T),
        ) %>%
        group_by(group_acres_binned) %>% 
        transmute(jurisdiction = jurisdiction,
                  acres_binned = group_acres_binned,
                  perc = count/sum(count) * 100,
                  count_nj = count,
                  perc_area = total_area / sum(total_area) * 100,
                  area_nj = total_area,
                  wr = wr,
                  grouped = T) %>% 
        ungroup() %>% 
        select(-group_acres_binned)
    )
}

wetland_size_data <- calc_group_wetland_patch_size(wetlands_dt) %>% 
  bind_rows(calc_wetland_patch_size(wetlands_dt, wr = 1)) %>% 
  bind_rows(calc_wetland_patch_size(wetlands_dt, wr = 2)) %>% 
  bind_rows(calc_wetland_patch_size(wetlands_dt, wr = 3)) %>% 
  bind_rows(calc_wetland_patch_size(wetlands_dt, wr = 4)) %>% 
  bind_rows(calc_wetland_patch_size(wetlands_dt, wr = 5)) %>% 
  bind_rows(calc_wetland_patch_size(wetlands_dt, wr = 6)) %>% 
  bind_rows(calc_wetland_patch_size(wetlands_dt, wr = 7)) %>% 
  filter(jurisdiction == F)

levels(wetland_size_data$acres_binned) <-   sub(".(.+),(.+).", "\\1-\\2"  , levels(wetland_size_data$acres_binned))
levels(wetland_size_data$acres_binned)[13] <- "100+"


area_perc_not_jd <- wetland_size_data %>% 
  filter(grouped == T) %>% 
  mutate(wr = factor(wr)) %>% 
  ggplot()+
  geom_tile(aes(x=wr, y = acres_binned, fill = perc), color = "white")+
  theme_classic()+
  theme(strip.background = element_rect(fill = NA, color = NA),
        strip.text.x = element_text(face = "bold", angle = 0, size = 12))+
  scale_fill_distiller(palette = "OrRd", name = "Non-tidal wetlands estimated\nnon-jurisdictional (%)", direction = 1,
                       guide = guide_colorbar(
                         direction = "horizontal",
                         title.position = "top",
                         draw.ulim = F,
                         draw.llim = F
                       ),
                       breaks = c(0,25,50,75,100),
                       labels = c(0,25,50,75,100),
                       limits = c(0, 100))+
  scale_x_discrete(breaks = c(0,1,2,3,4,5,6,7), labels = c("None", "Seasonally\nsaturated", "Continuously\nsaturated", "Seasonally\nflooded", "Seasonally\nflooded/saturated", "Semi-permanently\nflooded", "Intermittently\nexposed", "Permanently\nflooded"))+#,"G","H"))+
  
  coord_equal()+
  theme(text = element_text(size = 8))+
  scale_color_manual(values = c("black", "white"), guide = "none")+
  xlab("Minimum wetland water regime")+
  ylab("Wetland size (Acres)")+
  theme(axis.text.x = element_text(angle = 45, size = 5, hjust = 1, vjust = 1, lineheight=1),
        legend.position = "top",
        legend.key.height = unit(0.35, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.title = element_text(family = "Arial", size = 8, vjust = 1, face = "bold"),
        legend.text = element_text(size = 7))

area_area_not_jd <- wetland_size_data %>% 
  filter(grouped == T) %>% 
  mutate(wr = factor(wr)) %>% 
  ungroup() %>% 
  ggplot()+
  geom_tile(aes(x=wr, y = acres_binned, fill = area_nj), color = "white")+
  theme(strip.background = element_rect(fill = NA, color = NA),
        strip.text.x = element_text(face = "bold", angle = 0, size = 12))+
  scale_fill_distiller(palette = "RdPu", name = "Non-tidal wetlands estimated\nnon-jurisdictional (Acres)", direction = 1, trans = "log10", labels = label_number(scale_cut = cut_short_scale()),
                       guide = guide_colorbar(
                         direction = "horizontal",
                         title.position = "top"
                       ))+
  scale_color_manual(values = c("black", "white"), guide = "none")+
  scale_x_discrete(breaks = c(0,1,2,3,4,5,6,7), labels = c("None", "Seasonally\nsaturated", "Continuously\nsaturated", "Seasonally\nflooded", "Seasonally\nflooded/saturated", "Semi-permanently\nflooded", "Intermittently\nexposed", "Permanently\nflooded"))+#,"G","H"))+
  theme_classic()+
  coord_equal()+
  theme(text = element_text(size = 8, family = "Arial"))+
  xlab("Minimum wetland water regime")+
  ylab("Wetland size (Acres)")+
  theme(axis.text.x = element_text(angle = 45, size = 5, hjust = 1, vjust = 1, lineheight=1),
        legend.position = "top",
        legend.key.height = unit(0.35, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.title = element_text(family = "Arial", size = 8, vjust = 1, face = "bold"),
        legend.text = element_text(size = 7))

new_area_plot <- area_perc_not_jd + area_area_not_jd & plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10, face = "bold", family = "Arial"))

ggsave("area_plot.png", 
       new_area_plot,
       width = 180,
       height = 150,
       units = "mm",
       dpi = 300,
       bg = "transparent")  

ggsave("production_figures/figure_4.eps", 
       new_area_plot,
       width = 180,
       height = 150,
       units = "mm",
       device = cairo_ps,
       bg = "transparent")  
