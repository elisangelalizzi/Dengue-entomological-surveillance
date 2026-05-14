# Spatiotemporal Modeling of *Aedes spp.* Egg Density Index (EDI) in Londrina (PR), Brazil, Using Generalized Additive Models (GAM)

## Overview

This repository contains the scripts, datasets organization, and statistical modeling workflow used in the study:

> **Spatiotemporal modeling of the *Aedes spp.* Egg Density Index (EDI) in Londrina (PR), Brazil, using Generalized Additive Models (GAM)**

The project investigates the association between climatic variables and the Egg Density Index (EDI) of *Aedes spp.* in Londrina, Paraná, Brazil, between 2022 and 2025, using spatiotemporal statistical modeling.

The analytical framework combines:

- Generalized Additive Models (GAM);
- Negative Binomial regression;
- Spatial smoothing;
- Temporal autoregressive structures (AR1–AR3);
- Climatic covariates;
- Georeferenced ovitrap surveillance data.

---

## Research Objective

To evaluate the influence of climatic variables on *Aedes spp.* Egg Density Index (EDI) while accounting for:

- nonlinear temporal trends;
- short-term temporal dependence;
- spatial heterogeneity.

---

## Methodological Summary

The modeling strategy was developed using weekly georeferenced ovitrap data collected in Londrina (PR), Brazil.

### Variables Included

#### Response Variable
- Egg Density Index (EDI)

#### Climatic Covariates
- Mean temperature
- Accumulated precipitation
- Mean wind speed

#### Spatial Information
- Latitude
- Longitude

#### Temporal Structures
- Epidemiological week
- Lagged EDI variables:
  - AR1
  - AR2
  - AR3

---

## Statistical Approach

The models were implemented using:

- Generalized Additive Models (GAM)
- Negative Binomial distribution
- Log-link function

# Spatiotemporal Modeling of *Aedes spp.* Egg Density Index (EDI) in Londrina (PR), Brazil, Using Generalized Additive Models (GAM)

## Overview

This repository contains the scripts, datasets organization, and statistical modeling workflow used in the study:

> **Spatiotemporal modeling of the *Aedes spp.* Egg Density Index (EDI) in Londrina (PR), Brazil, using Generalized Additive Models (GAM)**

The project investigates the association between climatic variables and the Egg Density Index (EDI) of *Aedes spp.* in Londrina, Paraná, Brazil, between 2022 and 2025, using spatiotemporal statistical modeling.

The analytical framework combines:

- Generalized Additive Models (GAM);
- Negative Binomial regression;
- Spatial smoothing;
- Temporal autoregressive structures (AR1–AR3);
- Climatic covariates;
- Georeferenced ovitrap surveillance data.

---

## Research Objective

To evaluate the influence of climatic variables on *Aedes spp.* Egg Density Index (EDI) while accounting for:

- nonlinear temporal trends;
- short-term temporal dependence;
- spatial heterogeneity.

---

## Methodological Summary

The modeling strategy was developed using weekly georeferenced ovitrap data collected in Londrina (PR), Brazil.

### Variables Included

#### Response Variable
- Egg Density Index (EDI)

#### Climatic Covariates
- Mean temperature
- Accumulated precipitation
- Mean wind speed

#### Spatial Information
- Latitude
- Longitude

#### Temporal Structures
- Epidemiological week
- Lagged EDI variables:
  - AR1
  - AR2
  - AR3

---

## Statistical Approach

The models were implemented using:

- Generalized Additive Models (GAM)
- Negative Binomial distribution
- Log-link function

├── data/
│   ├── raw/                 # Raw ovitrap and climatic datasets
│   ├── processed/           # Cleaned and aggregated datasets
│
├── scripts/
│   ├── 01_data_cleaning.R
│   ├── 02_feature_engineering.R
│   ├── 03_exploratory_analysis.R
│   ├── 04_gam_modeling.R
│   ├── 05_spatial_analysis.R
│   ├── 06_model_diagnostics.R
│
├── outputs/
│   ├── figures/
│   ├── tables/
│   ├── maps/
│   ├── diagnostics/
│
├── manuscript/
│   ├── poster/
│   ├── abstract/
│
├── README.md
└── LICENSE

## Citation

Sahd, C. S.; Kawabata, E. K.; Zequi, J. A. C.; Lizzi, E. A. S.
Spatiotemporal modeling of the Aedes spp. Egg Density Index (EDI)
in Londrina (PR), Brazil, using Generalized Additive Models (GAM).

## Authors
Claudia Stoeglehner Sahd
ORCID: 0000-0002-7134-4199

Edson Kenji Kawabata
ORCID: 0009-0003-9860-3618

João Antonio Cyrino Zequi
ORCID: 0000-0002-1480-7660

Elisangela Aparecida da Silva Lizzi
ORCID: 0000-0001-7064-263X

*Keywords*
Aedes aegypti
GAM
Spatiotemporal Modeling
Epidemiological Surveillance
Negative Binomial Models
Spatial Statistics
Vector Ecology
Entomological Surveillance
