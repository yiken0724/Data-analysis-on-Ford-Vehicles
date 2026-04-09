# Ford Vehicle Pricing Analysis

An R-based statistical data analysis project examining the pricing dynamics and key attributes of used Ford vehicles. Built as part of the MH3511 Data Analysis with Computer group project at NTU (Group 37).

---

## Overview

This project investigates what drives the resale price of used Ford cars using a dataset of 17,965 observations sourced from Kaggle. The analysis explores the relationship between `price` and attributes such as model, transmission type, fuel type, mileage, fuel efficiency, and year of manufacture — using statistical tests and linear regression models implemented in R.

---

## Research Questions

1. Is the price of a Ford car dependent on any of its attributes (model, transmission type, mileage, fuel type, or mpg)?
2. Does the year of manufacture affect Ford car prices?
3. Which attribute influences price distribution the most, and by how much?

---

## Dataset

The dataset (`ford.csv`) was sourced from [Kaggle](https://www.kaggle.com) and contains used Ford vehicle records from **1996 to 2020**.

| Variable | Description |
|----------|-------------|
| `model` | Model of the car |
| `year` | Year of manufacture |
| `price` | Resale price (£) — main variable of interest |
| `transmission` | Type of gearbox (Manual, Automatic, Semi-Auto) |
| `mileage` | Total distance covered (miles) |
| `fuelType` | Type of fuel (Petrol, Diesel, etc.) |
| `mpg` | Miles per gallon — fuel consumption rate |

> Note: `tax` and `engineSize` columns were excluded as irrelevant to this analysis.

---

## Analysis Pipeline

### 1. Data Cleaning & Preparation
- Log-transform `price` → `ln_price` to reduce right skew
- Remove `ln_price` outliers using the IQR boxplot rule (~1.89% of data removed)
- Filter car models with fewer than 200 records (~1.13% removed)
- Retain only Petrol and Diesel fuel types
- Apply cube of log transform to `mileage` → `ln_cube_mileage` for normality
- Remove the erroneous year entry of 2060 and years with fewer than 30 records

### 2. Statistical Tests
- **model vs. ln_price** — one-way ANOVA and pairwise t-tests
- **transmission vs. ln_price** — one-way ANOVA and pairwise t-tests
- **fuelType vs. ln_price** — Levene's F-test for equal variance, Welch's two-sample t-test
- **mileage vs. ln_price** — scatter plot and simple linear regression
- **year vs. ln_price** — one-way ANOVA and pairwise t-tests

### 3. Regression Modelling
- Correlation matrix with scatter plots (`psych::pairs.panels`)
- Simple linear regression: `ln_price ~ ln_cube_mileage` and `ln_price ~ mpg`
- Multiple linear regression: `ln_price ~ mpg + ln_cube_mileage`
- Backward stepwise selection to identify the best-fit model

---

## Setup & Running

### Prerequisites
- R (version 4.0 or later recommended)
- The following R packages:
  - `psych`
  - `dplyr`

Install packages if needed:
```r
install.packages(c("psych", "dplyr"))
```

### Running the Script

1. Place `ford.csv` and `data-analysis-on-ford-vehicles.R` in the same folder
2. Open the `.R` file in RStudio or any R environment
3. Update the `read.csv()` path at the top to point to your local `ford.csv`
4. Run the script in full, or section by section

---

## Files

| File | Description |
|------|-------------|
| `data-analysis-on-ford-vehicles.R` | Full R source code for the analysis |
| `ford.csv` | Raw dataset from Kaggle (required at runtime) |
| `MH3511 Group Project (Group 37).pdf` | Full group project report with results and discussion |

---

## Authors

Group 37 — MH3511 Data Analysis with Computer, Nanyang Technological University

| Name | Matric No |
|------|-----------|
| Choo Yi Ken | U2240710B |
| Matthew Heng Yu Jie | U2223483D |
| Tong Hao Kit | U2240130E |
| Grand Tan Ze Ming | U2240872B |
| Hydee Qurniawan B Rosli | U2040911F |
