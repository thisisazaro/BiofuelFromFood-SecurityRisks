# Biofuel_and_FoodSecurity

This repository contains the full workflow and dataset used in the analysis of the relationship between biofuel production from food crops and global food security indicators.

## Research Objective

To evaluate the **economic and food security implications** of using agricultural products (such as corn, sugarcane, soy) in the production of **bioethanol** and **biodiesel**, with a focus on:

- Identifying trends in biofuel production
- Analyzing macroeconomic and food availability indicators
- Investigating the trade-offs between energy transition and food security

## Contents

- `R` scripts to download and clean data from:
  - [World Bank WDI](https://databank.worldbank.org/source/world-development-indicators)
  - [FAOSTAT](https://www.fao.org/faostat/en/#data)
- A unified dataset `full_data.xlsx` combining:
  - GDP per capita
  - Food production index
  - Agricultural employment
  - Undernourishment prevalence
  - Cereal yield
  - Renewable energy use
  - Food supply adequacy (FAO)
- Exploratory plots and model-ready data

## Tools

- **R**
- Libraries: `dplyr`, `WDI`, `readr`, `writexl`, `ggplot2`, `stringr`

## Example Variables

| Variable | Description |
|----------|-------------|
| `GDP_per_capita` | GDP per capita (current US$) |
| `Food_Production_Index` | FAO food production index (base 2004–2006 = 100) |
| `Food_Supply_Adequacy` | Average dietary energy supply adequacy (%) |
| `Cereal_Yield` | Cereal yield (kg per hectare) |
| `Renewable_Energy_Share` | Renewable energy consumption (% of total) |

## Goal

This dataset will support statistical modeling (e.g., panel regressions) to test hypotheses about the **impact of biofuel-related crop use on global food availability**.

---

