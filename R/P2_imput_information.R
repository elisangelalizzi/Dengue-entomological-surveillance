# R version 4.6.0 (2026-04-24)
# rm(list = ls()) # used to clear the R environment.
#
# Spatio-Temporal Modeling of the Aedes spp. egg density index using GAM with
# autoregressive components in Londrina, Brazil
# by: Claudia Stoeglehner Sahd, Edson Kenji Kawabata, João Antonio Cyrino Zequi
# Elisangela Aparecida da Silva Lizzi.
#
# Codes used to acquire climatic data
#
#
#
###########################
### Object names legend ###
###########################
#####
##### "d_2022", "d_2023", "d_2024" and "d_2025" used to designate the original
#####data structure.
#####
##### "c_2022", "c_2023", "c_2024" and "c_2025" used to designate the organized
#####and restructured data.
#####
##### "clim" summarized daily climatic data obtained from NASA POWER API.
#####
##### "df_ovitrampas", "df_ovitrampas_v2", "df_ovitrampas_v3", "df_ovitrampas_v4
#####" and "df_ovitrampas_v5" used to designate the data cleaning processes
#####
####### "df_ovitrampas" organized and merged ovitrap data.
#######
####### "df_ovitrampas_v2" dataset updated with recovered missing coordinates.
#######
####### "df_ovitrampas_v3" dataset formatted as a tsibble (time-series tibble)
#######with filled time gaps.
#######
####### "df_ovitrampas_v4" dataset resulting from the join of ovitrap data with
#######climatic data.
#######
####### "df_ovitrampas_v5" final processed dataset, including calculated
#######entomological indices (EDI and OPI).
#######
#####
##### "temp_df_ovitrampas_long", "temp_df_ovitrampas", "temp_df_ovitrampas_coord
#####" and "temp_df_ovitrampas_v4" used as auxiliary temporal objects
#####
#############################
### Variable names legend ###
#############################
#####
##### "address" column used for identify the installation address of the ovitrap
#####
##### "c_address" column used for identify the installation address of the
#####ovitraps, but in this case, with clean and usable names.
#####
##### "neighborhood" column used for identify the installation neighborhood name
#####of the ovitraps;
#####
##### "coordinate" column used for identify the location of ovitrap installation
#####coordinates (organized decimal degrees, separated into latitude and
#####longitude)
#####
##### "lat" / "lon" columns used to identify separated latitude and longitude
#####coordinates.
#####
##### "epi_week" column representing the epidemiological week (yearweek format).
#####
##### "yearm_month" column representing the specific year and month of the
#####observation.
#####
##### "trap_install_date" calculated date when the ovitrap was installed in the
#####field.
#####
##### "trap_colect_date" date when the ovitrap was collected.
#####
##### "temp_mean" / "temp_max" / "temp_min" mean, maximum, and minimum
#####temperatures (Celsius) for the epidemiological week.
#####
##### "acc_prec" accumulated precipitation (mm) for the epidemiological week.
#####
##### "wind_spe" average wind speed (m/s) for the epidemiological week.
#####
##### "month_EDI" monthly Egg Density Index (EDI), calculating the mean number
#####of eggs per positive trap.
#####
##### "month_OPI" monthly Ovitrap Positivity Index (OPI), representing the
#####percentage of traps with at least one egg.

if (!require(pacman)) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  janitor,
  fpp3,
  nasapower,
  duckplyr
)


temp_df_ovitrampas_v4 <- readRDS("data/RDS/df_ovitrampas_v3.RDS") |>
  ungroup() |>
  select(-year, -week) |>
  filter(!is.na(date)) |>
  as_tsibble(index = epi_week, key = address) |>
  fill_gaps() |>
  group_by(address) |>
  filter(!is.na(lon)) |>
  filter(!is.na(lat)) |>
  mutate(
    lat = as.numeric(lat),
    lon = as.numeric(lon),
    trap_colect_date = date,
    trap_install_date = lag(date),
    trap_install_date = coalesce(trap_install_date, trap_colect_date - 7),
  ) |>
  glimpse()

## Climatic data ---------------------------------------------------------

### get data -------------------------------------------------------------

# Using the Power API (get_power), for more information, consult the NASAPower
#package documentation.

# "T2M" - temp. mean
# "T2M_MAX" - temp. máx
# "T2M_MIN" - temp. min
# "PRECTOTCORR" - accumulated precipitation
# "WS2M" - wind speed

# - commented: original part of the first script -
#
#c_ovitrampa <- readRDS("data/original_ovitrampas_londrina.RDS") |>
# mutate(data = as.Date(epi_sem)) |>
#   distinct() |>
#   group_by(endereco) |>
#   filter(!is.na(lon)) |>
#   filter(!is.na(lat)) |>
#   mutate(
#     lat = as.numeric(lat),
#     lon = as.numeric(lon)
#   )
#
# calc_nasa_clima <- function(df, lat, lon) {
#   min_data <- as.Date(min(df$instalacao))
#   max_data <- as.Date(max(df$coleta))
#   dados_diarios <- get_power(
#     community = "ag",
#     lonlat = c(lon, lat),
#     pars = c("T2M", "T2M_MAX", "T2M_MIN", "PRECTOTCORR", "WS2M"),
#     dates = c(min_data, max_data),
#     temporal_api = "daily"
#   )
# }

# clima <- c_ovitrampa |>
#   select(endereco, lat, lon) |>
#   unique() |>
#   mutate(
#     clima = pmap(
#       list(lat, lon),
#       ~ calc_nasa_clima(..1, ..2)
#     )
#   ) |>
#   ungroup() |>
#   unnest(clima)

# saveRDS(clima, "clima.RDS")
#
# -/-/-/- #

### Calc climatic data ---------------------------------------------------

clim <- readRDS("data/RDS/clima.RDS") |>
  select(-LON, -LAT, -instalacao, -coleta) |>
  mutate(
    epi_week = yearweek(YYYYMMDD, week_start = 7)
  ) |>
  rename(address = endereco) |>
  summarise(
    .by = c(epi_week, address),
    temp_mean = mean(T2M, na.rm = TRUE),
    temp_max = mean(T2M_MAX, na.rm = TRUE),
    temp_min = mean(T2M_MIN, na.rm = TRUE),
    acc_prec = sum(PRECTOTCORR, na.rm = TRUE),
    wind_spe = mean(WS2M, na.rm = TRUE),
    date = min(YYYYMMDD)
  ) |>
  group_by(epi_week, address)


### Union Clim. data with Ovitrap ----------------------------------------

df_ovitrampas_v4 <- temp_df_ovitrampas_v4 |>
  ungroup() |>
  left_join(clim, by = c("address", "epi_week")) |>
  unique()


# Data processing - last adjustments -------------------------------------

df_ovitrampas_v5 <- df_ovitrampas_v4 |>
  ungroup() |>
  filter(!is.na(n_eggs)) |>
  group_by(address, yearm_month) |>
  mutate(
    month_EDI = ifelse(
      is.na(sum(n_eggs, na.rm = TRUE) / sum(n_eggs > 0, na.rm = TRUE)),
      0,
      (sum(n_eggs, na.rm = TRUE) / sum(n_eggs > 0, na.rm = TRUE))
    ),
    month_OPI = ifelse(
      is.na((sum(n_eggs > 0, na.rm = TRUE) / sum(n_eggs >= 0, na.rm = TRUE))),
      0,
      (sum(n_eggs > 0, na.rm = TRUE) / sum(n_eggs >= 0, na.rm = TRUE) * 100)
    )
  )

# saveRDS(df_ovitrampas_v5, "data/RDS/df_ovitrampas_v5.RDS")

# Later, we added information on "human land use and influence", but the data
# were not used in the modeling. These data were acquired from:
# Presotto, A., Hamilton, S. & Izar, P. A 10-meter resolution human footprint dataset to support biodiversity and conservation studies in Brazil. Sci Data 12, 1754 (2025).
# https://doi.org/10.1038/s41597-025-06034-0
#
# You can view the full information in "data/csv/dados_cal_ovitrampa.csv" (the
# original and first version, before translation into English)
