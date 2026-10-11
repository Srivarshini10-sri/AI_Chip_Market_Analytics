# AI Chip Market Analytics Dashboard Using R Shiny

## Project Overview
An interactive R Shiny dashboard that analyzes the AI chip market: vendors, chip performance, power consumption, selling price, shipments and revenue from 2020 to 2026.

## Problem Statement
The AI chip market data covers many vendors, chip models, performance and power ratings, prices, shipments and revenue estimates. As a large table it is hard to compare vendors, relate performance to power, or see how revenue changes over time. This project provides an interactive tool to filter the data and see results immediately.

## Objectives
- Read and clean the AI chip market dataset in R.
- Build an interactive dashboard with R Shiny.
- Filter the data by year, vendor and chip.
- Show KPI cards: total records, number of vendors, estimated revenue, average FP16 TFLOPS.
- Visualize revenue, shipments, selling price, performance and power, and study memory vs performance and revenue vs shipments.

## Dataset
`r project .csv` has 800 rows and 10 columns: YEAR, CHIP NAME, VENDOR, LAUNCH DATE, MEMORY GB, FP16_TFLOPS, TDP WATTS, ESTIMATED SHIPMENTS, ESTIMATED ASP USD, ESTIMATED REVENUE USD (revenue values are in USD millions). It covers 171 chips from 64 vendors.

## Tools and Technologies
- R and RStudio
- shiny (web application framework)
- ggplot2 (charts)
- DT (interactive tables)

## Dashboard Tabs
1. **Overview** – revenue by year and vendor, shipments by vendor, memory vs FP16
2. **Vendor Analysis** – vendor summary table, average selling price by vendor
3. **Chip Performance** – FP16 and power by chip, revenue vs shipments
4. **Dataset** – searchable, filterable data table

## How to Run
1. Open `chip analysis.Rproj` in RStudio.
2. Open `chip analysis.R` and click **Run App** (or run the whole script).
3. When prompted, select `r project .csv`.

## Files
- `chip analysis.R` – Shiny application code
- `chip analysis.Rproj` – RStudio project file
- `r project .csv` – dataset
- `AI_Chip_Market_Analytics_Report.pdf` – project report

## Conclusion
R Shiny makes it possible to share data analysis as an interactive web app. This dashboard can be extended with performance-per-watt metrics, price-performance ratios and yearly forecasting.
