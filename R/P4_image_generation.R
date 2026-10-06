# R version 4.6.0 (2026-04-24)
# rm(list = ls()) # used to clear the R environment.
#
# Spatio-Temporal Modeling of the Aedes spp. egg density index using GAM with
# autoregressive components in Londrina, Brazil
# by: Claudia Stoeglehner Sahd, Edson Kenji Kawabata, João Antonio Cyrino Zequi
# Elisangela Aparecida da Silva Lizzi.
#
#
###########################
### Object names legend ###
###########################
#####
##### "ovitrap_londrina" processed dataset containing ovitrap and climatic data (imported from RDS).
##### "streets" spatial shapefile containing Londrina's road network.
##### "hydrography" spatial shapefile containing Londrina's hydrography.
##### "monthly_data" dataset grouped and classified by Egg Density Index (EDI)
#####levels per month.
##### "total_months" vector containing all unique months present in the dataset.
##### "plot_monthly_data" list used to store the generated ggplot map objects
#####for each month.
##### "current_month_data" temporary spatial dataset filtered for the current
#####month in the loop.
##### "plot_m" temporary ggplot object for the current month's map.
#####
#############################
### Variable names legend ###
#############################
#####
##### "address" column identifying the installation address of the ovitraps.
##### "yearm_month" column representing the specific year and month of the
#####observation.
##### "month_EDI" monthly Egg Density Index (EDI).
##### "lat" / "lon" columns identifying separated latitude and longitude
#####coordinates.
##### "class_level" categorical variable classifying the EDI into "Satisfactory"
#####, "Alert", or "Risk".

if (!require(pacman)) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  fpp3,
  sf,
  terra,
  duckplyr,
  igraph,
  ggnewscale,
  ggtext,
  showtext,
  ggspatial,
  magick,
  cowplot
)


#####################################
### Map and month EDI time series ###
#####################################

ovitrap_londrina <- readRDS("./data/RDS/df_ovitrampas_v5.RDS") |>
  glimpse()

plot_map <- ggdraw() + draw_image("./data/Qgis_Mapa_Loc_Londrina.png")

df_ts <- ovitrap_londrina |>
  as.data.frame() |>
  ungroup() |>
  filter(!is.na(yearm_month)) |>
  select(month_EDI, month_OPI, yearm_month) |>
  group_by(yearm_month) |>
  summarise(
    month_EDI_tot = mean(month_EDI, na.rm = TRUE),
    month_OPI_tot = mean(month_OPI, na.rm = TRUE)
  ) |>
  unique() |>
  glimpse()


plot_ts <- ggplot(df_ts, aes(x = yearm_month, y = month_EDI_tot)) +
  geom_line(color = "#00000071", linewidth = 1) +
  geom_point(color = "#000000b0", size = 2.5) +
  labs(
    title = "",
    x = "Month / Year",
    y = "Mean EDI"
  ) +
  theme(
    plot.title = element_markdown(
      family = "serif",
      hjust = 0.5,
      size = 10,
      face = "bold"
    ),
    plot.title.position = "plot",
    legend.title = element_text(family = "serif", size = 8, hjust = 0.5),
    legend.text = element_text(family = "serif", size = 6),
    panel.grid = element_line(colour = "#0000001e"),
    panel.background = element_blank(),
    # axis.title = element_blank(),
    # axis.text = element_text(family = "serif", size = 2),
    # axis.text.y = element_text(angle = 90, hjust = 0.5),
    # axis.ticks = element_line(linewidth = 0.1),
    # axis.ticks.length = unit(0.01, "cm")
  )


plot_ts

map_panel <- plot_grid(
  plot_map,
  plot_ts,
  ncol = 1,
  rel_heights = c(3, 1),
  labels = "AUTO"
)

ggsave(
  "./fig/panel_map_ts.png",
  plot = map_panel,
  width = 10,
  height = 9,
  dpi = 300,
  bg = "white"
)


####################################
### Map - evolution of month EDI ###
####################################

streets <- st_read("data/shp/Londrina_ruas_31982.shp")
hydrography <- st_read("data/shp/Londrina_hidrografia_31982.shp")

monthly_data <- ovitrap_londrina |>
  ungroup() |>
  select(address, yearm_month, month_EDI, lat, lon) |>
  unique() |>
  mutate(
    yearm_month = as.Date(yearm_month),
    month = month(yearm_month),
    year = year(yearm_month)
  ) |>
  mutate(
    quarter = case_when(
      month %in% 1:3 ~ 1,
      month %in% 4:6 ~ 2,
      month %in% 7:9 ~ 3,
      month %in% 10:12 ~ 4
    ),
    label = paste0("Q", quarter, " ", year)
  ) |>
  group_by(address, year, quarter, label) |>
  summarise(
    lat = first(lat),
    lon = first(lon),
    quarter_EDI = mean(month_EDI, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    class_level = case_when(
      quarter_EDI < 21 ~ "Satisfactory",
      quarter_EDI < 35 ~ "Alert",
      TRUE ~ "Risk"
    ),
    class_level = fct_relevel(class_level, "Satisfactory", "Alert", "Risk")
  ) |>
  arrange(year, quarter) |>
  glimpse()


monthly_data$label <- factor(
  monthly_data$label,
  levels = unique(monthly_data$label)
)


monthly_sf <- monthly_data |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs = 31982) |>
  st_buffer(dist = 250)

valid_t <- monthly_data |>
  ungroup() |>
  select(year, quarter) |>
  unique()

valid_street <- merge(
  streets,
  valid_t,
  by = NULL
) |>
  st_as_sf()


plot_q <- ggplot() +
  geom_sf(
    data = valid_street,
    color = "#aaaaaaca",
    fill = NA,
    linewidth = 0.06
  ) +
  geom_sf(
    data = monthly_sf,
    aes(fill = class_level),
    color = "#6d6d6d16",
    linewidth = 0.1,
    alpha = 0.6
  ) +
  scale_fill_manual(
    name = "Egg Infestation Level",
    values = c(
      "Risk" = "#c91013",
      "Alert" = "#dde026",
      "Satisfactory" = "#00e00b"
    )
  ) +
  facet_grid(year ~ paste0("Q", quarter)) +
  labs(
    title = "" #EDI level evolution in Londrina (2022-2025)
  ) +
  annotation_scale(
    data = valid_street,
    location = "br",
    line_width = .15,
    height = unit(0.1, "cm"),
    text_family = "serif",
    text_cex = 0.5
  ) +
  annotation_north_arrow(
    data = valid_street,
    location = "tr",
    which_north = "true",
    height = unit(0.8, "cm"),
    width = unit(0.8, "cm"),
    pad_x = unit(0.05, "in"),
    pad_y = unit(0.05, "in"),
    style = north_arrow_fancy_orienteering(
      text_family = "serif",
      text_size = 5
    )
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(
      family = "serif",
      size = 16,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    legend.position = "bottom",
    legend.title = element_text(family = "serif", size = 12, face = "bold"),
    legend.text = element_text(family = "serif", size = 10),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.text.x = element_text(family = "serif", size = 12, face = "bold"),
    strip.text.y = element_text(
      family = "serif",
      size = 12,
      face = "bold",
      angle = 270
    ),
    strip.background = element_rect(fill = "white", color = "gray80")
  )

ggsave(
  "./fig/panel_EDI_map_londrina.png",
  plot = plot_q,
  width = 12,
  height = 12,
  dpi = 600,
  bg = "white"
)
