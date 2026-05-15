# R version 4.6.0 (2026-04-24)
# rm(list = ls()) # used to clear the R environment.
#
# Spatio-Temporal Modeling of the Aedes spp. egg density index using GAM with
# autoregressive components in Londrina, Brazil
# by: Claudia Stoeglehner Sahd, Edson Kenji Kawabata, João Antonio Cyrino Zequi
# Elisangela Aparecida da Silva Lizzi.
#
# Codes used in data processing and acquisition
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

# Required package list --------------------------------------------------

if (!require(pacman)) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  readxl,
  janitor,
  fpp3,
  tidygeocoder,
  nasapower,
  duckplyr
)


# Loading and Processing Ovitrap data ------------------------------------

## Considering the original data structure, the information was loaded in parts.
##Each year was loaded and organized separately, and then fully merged.

## Year 2022 -------------------------------------------------------------

d_2022 <- readxl::read_excel(
  "data/OVITRAMPAS 22 23 24 25.xlsx",
  sheet = 1,
  na = "",
  n_max = 71
) |>
  janitor::clean_names() |>
  rename(
    "address" = "endereco",
    "neighborhood" = "localidade"
  ) |>
  glimpse()

c_2022 <- d_2022
{
  colnames(c_2022) <- paste0("2022_", str_remove(colnames(d_2022), "x"))

  c_2022 <- c_2022 |>
    rename("address" = "2022_address") |>
    mutate(
      c_address = janitor::make_clean_names(address)
    ) |>
    relocate(c_address, "2022_neighborhood") |>
    glimpse()
}

## Year 2023 -------------------------------------------------------------

d_2023 <- readxl::read_excel(
  "data/OVITRAMPAS 22 23 24 25.xlsx",
  sheet = 2,
  na = "",
  n_max = 455
) |>
  janitor::clean_names() |>
  rename(
    "address" = "endereco",
    "neighborhood" = "localidade"
  ) |>
  glimpse()

c_2023 <- d_2023
{
  # cell "c_2023[67, "x51"]"  with an incorrectly typed dot, resulting in a misinterpretation. We decided to remove the dot of that cell.
  c_2023 <- c_2023 |>
    mutate(x51 = as.numeric(x51))

  c_2023[67, "x51"] <- 16

  colnames(c_2023) <- paste0("2023_", str_remove(colnames(d_2023), "x"))

  # We identified duplicated address and "NA" values with no possibility of recovering that individual information. Therefore, we removed them from the data.

  ## The duplicated addresses were:
  c_2023 |>
    get_dupes("2023_address")

  c_2023 <- c_2023 |>
    rename("address" = "2023_address") |>
    filter(!is.na(address)) |>
    mutate(
      c_address = janitor::make_clean_names(address)
    ) |>
    relocate(c_address, "2023_neighborhood") |>
    group_by(c_address) |>
    filter(n() == 1) |>
    ungroup() |>
    glimpse()
}

## Year 2024 -------------------------------------------------------------

d_2024 <- readxl::read_excel(
  "data/OVITRAMPAS 22 23 24 25.xlsx",
  sheet = 3,
  na = "",
  range = "A1:BA1020"
) |>
  janitor::clean_names() |>
  rename(
    "address" = "endereco",
    "neighborhood" = "localidade",
    "coordinate" = "coordenadas"
  ) |>
  glimpse()

c_2024 <- d_2024
{
  # We removed the column "lira" which referred to the week in which the LIRAa (The Brazilian Rapid Survey of Aedes aeygpti Infestation Index) was performed (week 02).
  c_2024 <- c_2024 |>
    select(-lira)

  # We identified cells with incorrect typed information, that results in data misinterpretation. Therefore, we removed them from the data

  temp <- c_2024 |>
    select(x1:last_col()) |>
    select(where(is.character)) |>
    colnames()

  l <- list()
  for (i in temp) {
    l[[i]] <- which(!str_detect(c_2024[[i]], "^[0-9]+$"))
  }

  l # cells with incorrect typed information.

  rm("temp", "l", "i")

  c_2024[813, "x41"] <- NA # the original value was "1==2".
  c_2024[539, "x45"] <- NA # the original value was ".6".
  c_2024[293, "x46"] <- NA # the original value was "/8".

  c_2024 <- c_2024 |>
    mutate(
      x41 = as.numeric(x41),
      x45 = as.numeric(x45),
      x46 = as.numeric(x46)
    ) |>
    glimpse()

  colnames(c_2024) <- paste0("2024_", str_remove(colnames(c_2024), "x"))

  # We identified duplicated address and "NA" values with no possibility of recovering those individual information. Therefore, we removed it from the data.

  c_2024 |>
    get_dupes("2024_address")

  c_2024 <- c_2024 |>
    rename(
      address = "2024_address",
      coordinate = "2024_coordinate"
    ) |>
    filter(!is.na(address)) |>
    mutate(
      c_address = janitor::make_clean_names(address)
    ) |>
    relocate(c_address, "2024_neighborhood") |>
    group_by(c_address) |>
    filter(n() == 1) |>
    ungroup() |>
    separate_wider_delim(
      coordinate,
      ", ",
      names = c("lat", "lon"),
      cols_remove = FALSE
    ) |>
    glimpse()
}

## Year 2025 -------------------------------------------------------------

d_2025 <- readxl::read_excel(
  "data/OVITRAMPAS 22 23 24 25.xlsx",
  sheet = 4,
  na = "",
  n_max = 1090
) |>
  clean_names() |>
  rename(
    "address" = "endereco"
  ) |>
  glimpse()

c_2025 <- d_2025

{
  colnames(c_2025) <- paste0("2025_", str_remove(colnames(d_2025), "x"))

  # We identified duplicated address and "NA" values with no possibility of recovering those individual information. Therefore, we removed them from the data.

  c_2025 |>
    get_dupes("2025_address")

  c_2025 <- c_2025 |>
    rename("address" = "2025_address") |>
    filter(!is.na(address)) |>
    mutate(
      c_address = janitor::make_clean_names(address)
    ) |>
    relocate(c_address) |>
    group_by(c_address) |>
    filter(n() == 1) |>
    ungroup() |>
    mutate(`2025_1` = NA) |> # here we insert an "NA" column to ensure that no wrong values were imputed
    glimpse()
}


## Data union ------------------------------------------------------------

temp_df_ovitrampas <- c_2024 |>
  left_join(c_2022, by = "c_address") |>
  select(
    c_address,
    neighborhood = `2024_neighborhood`,
    everything(),
    -`2022_neighborhood`,
    -address.x,
    -address.y
  ) |>
  left_join(c_2023, by = "c_address") |>
  select(
    c_address,
    everything(),
    -`2023_neighborhood`,
    -address
  ) |>
  glimpse()

df_ovitrampas <- c_2025 |>
  left_join(temp_df_ovitrampas, by = "c_address") |>
  select(
    c_address,
    address,
    neighborhood,
    lat,
    lon,
    coordinate,
    everything(),
  ) |>
  glimpse()

## Lat&Lon get -----------------------------------------------------------

# - commented part of the script -

# Given the presence of addresses without coordinates (Lat. and Lon), we sought
#to identify this geospatial information based on the addresses using a Google API.

# df_ovitrampas_temp <- df_ovitrampas |>
#   filter(is.na(coordinate)) |>
#   mutate(
#     ende_completo = paste(c_address, "Londrina", "Paraná", "Brasil", sep = ", ")
#   ) |>
#   select(ende_completo, c_address) |>
#   geocode(
#     address = ende_completo,
#     method = "google" # This process requires a Google Maps API configuration.
#   )

# # We saved an RDS file with the results
# saveRDS(df_ovitrampas_temp, ".data/RDS/coordenadas_faltando_google.RDS")

# -/-/-/- #

temp_df_ovitrampas_coord <- readRDS(
  "data/RDS/coordenadas_faltando_google.RDS"
) |>
  rename(
    lon = long,
    full_address = ende_completo,
    address = ende_2025 # "ende_2025" was the old name for the "address" variable
  ) |>
  filter(!str_detect(full_address, "s_n")) |>
  select(-full_address) |>
  mutate(lat = as.character(lat), lon = as.character(lon)) |>
  distinct()

df_ovitrampas_v2 <- df_ovitrampas |>
  rows_patch(temp_df_ovitrampas_coord, by = "address") |>
  glimpse()


## Data Wide to long conversion ------------------------------------------

temp_df_ovitrampas_long <- df_ovitrampas_v2 |>
  select(-address) |> # removing the original "address" column
  select(
    address = c_address, # here we are renaming the variable "c_address" to "address"
    everything(),
    -coordinate
  ) |>
  pivot_longer(
    cols = c(-address, -neighborhood, -lat, -lon),
    names_to = "data",
    values_to = "n_eggs"
  ) |>
  separate_wider_delim(
    data,
    "_",
    names = c("year", "week")
  ) |>
  mutate(temp_epi_week = paste0(year, " W", week))


## Final organization of data --------------------------------------------

df_ovitrampas_v3 <- temp_df_ovitrampas_long |>
  mutate(
    epi_week = yearweek(
      temp_df_ovitrampas_long$temp_epi_week,
      week_start = 7
    )
  ) |>
  mutate(
    date = as.Date(epi_week),
    yearm_month = yearmonth(epi_week)
  ) |>
  select(-temp_epi_week) |>
  as_tsibble(
    key = address,
    index = epi_week
  ) |>
  fill_gaps() |>
  mutate(n_eggs = as.integer(n_eggs)) |>
  relocate(address, epi_week, n_eggs, lat, lon, neighborhood, year, week)

# write_csv2(df_ovitrampas_v3, "data/csv/ovitrap_londrina.csv")
# saveRDS(df_ovitrampas_v3, "data/RDS/df_ovitrampas_v3.RDS")
