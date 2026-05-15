# R version 4.6.0 (2026-04-24)
# rm(list = ls()) # used to clear the R environment.
#
# Spatio-Temporal Modeling of the Aedes spp. egg density index using GAM with
# autoregressive components in Londrina, Brazil
# by: Claudia Stoeglehner Sahd, Edson Kenji Kawabata, João Antonio Cyrino Zequi
# Elisangela Aparecida da Silva Lizzi.
#
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
  ggspatial
)

ovitrap_londrina <- readRDS("data/RDS/df_ovitrampas_v5.RDS")

streets <- st_read("data/shp/Londrina_ruas_31982.shp")
hydrography <- st_read("data/shp/Londrina_hidrografia_31982.shp")

monthly_data <- ovitrap_londrina |>
  ungroup() |>
  select(address, yearm_month, month_EDI, lat, lon) |>
  unique() |>
  group_by(address, yearm_month) |>
  mutate(
    class_level = case_when(
      month_EDI < 21 ~ "Satisfactory",
      month_EDI < 35 ~ "Alert",
      TRUE ~ "Risk"
    )
  ) |>
  ungroup() |>
  mutate(
    class_level = fct_relevel(class_level, "Satisfactory", "Alert", "Risk")
  ) |>
  arrange(yearm_month, class_level) |>
  glimpse()

total_months <- unique(monthly_data$yearm_month)
# total_months <- yearmonth("2025 jul") # test

plot_monthly_data <- list()

for (i in seq_along(total_months)) {
  current_month <- total_months[i]

  current_month_data <- monthly_data |>
    filter(yearm_month == current_month) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    st_transform(crs = 31982) |>
    st_buffer(dist = 250)

  plot_m <- ggplot() +
    geom_sf(
      data = streets,
      aes(color = "Roads"),
      fill = NA,
      # size = 0.06
      linewidth = 0.06
    ) +
    scale_color_manual(
      name = "Legend",
      values = c("Roads" = "#aaaaaaca"),
      guide = guide_legend(ncol = 1)
    ) +
    geom_sf(
      data = current_month_data,
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
    # coord_sf(
    #   xlim = corte_mapa_londrina[, 'X'],
    #   ylim = corte_mapa_londrina[, 'Y'],
    #   datum = 31982,
    #   expand = FALSE
    # ) +
    annotation_scale(
      location = "br",
      line_width = .15,
      height = unit(0.1, "cm"),
      text_family = "serif",
      text_cex = 0.5
    ) +
    annotation_north_arrow(
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
    labs(
      title = format(as.Date(este_mes), "%Y/%m")
    ) +
    theme_bw() +
    theme(
      plot.title = element_markdown(
        family = "serif",
        hjust = 0.5,
        size = 10,
        face = "bold"
      ),
      plot.title.position = "plot",
      # plot.subtitle = element_text(family = "serif", hjust = 0.5, size = 12),
      # legend.position = "none", # adicionado para o Loop para montar os quadrantes
      # Config de posição de legendas e etc alteradas, para caber no arquivo
      legend.title = element_text(family = "serif", size = 8, hjust = 0.5),
      # legend.title.position = "top",
      legend.text = element_text(family = "serif", size = 6),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.title = element_blank(),
      axis.text = element_text(family = "serif", size = 2),
      axis.text.y = element_text(angle = 90, hjust = 0.5),
      axis.ticks = element_line(linewidth = 0.1),
      axis.ticks.length = unit(0.01, "cm")
    )

  map_month <- as.character(current_month)
  plot_monthly_data[[map_month]] <- plot_m

  clean_num <- format(as.Date(current_month), "%Y_%m")
  clean_char <- str_replace_all(map_month, " ", "_")

  ggsave(
    filename = paste0(
      "fig/plot_month_EDI_1600dpi/map_edi_",
      clean_num,
      # "_or_",
      # clean_char,
      ".png"
    ),
    plot = plot_m,
    width = 5.8,
    height = 5,
    dpi = 1600
  )

  ggsave(
    filename = paste0(
      "fig/plot_month_EDI_600dpi/map_edi_",
      clean_num,
      "_ou_",
      clean_char,
      ".png"
    ),
    plot = plot_m,
    width = 5.8,
    height = 5,
    dpi = 600
  )
}


# Gif_construction -------------------------------------------------------
pacman::p_load(magick)

img_local <- list.files(
  path = "fig/plot_month_EDI_600dpi",
  pattern = "\\.png",
  full.names = TRUE
)

image_read(img_local) |>
  image_animate(fps = 1, optimize = TRUE) |>
  image_write("fig/gif_morth0_fps1.gif")
