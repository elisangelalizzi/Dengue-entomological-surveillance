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

### Spatial Component
A two-dimensional smooth function was used to model continuous spatial heterogeneity:

```r
s(longitude, latitude)
