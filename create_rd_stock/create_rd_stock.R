# ========================================================================================
# Project:  gtap_pub_ag_rd_stock
# Reference: Yin et al. (20xx), Harmonized Global Public Agricultural R&D Stocks for the GTAP Database, [DOI TO BE ADDED]
# Subject:  Script to compute public agricultural R&D stock
# Author:   Michiel van Dijk, Zuzana Smeets Kristkova & Yan Jin
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================

# Load pacman for p_load
if(!require(pacman)) install.packages("pacman")
library(pacman)

# Load key packages
p_load(here, tidyverse, readxl, stringr, scales, glue, dplyr, HARr)

# R options
options(scipen = 999)
options(digits = 4) 


# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# Load GRAPE dataset
grape_raw <- read_excel(here::here("input_data/grape_v1.0.0.xlsx"))

# Country classes
country_group <- read_csv(here::here("input_data/rd_stock_country_group.csv"))

# GTAP regional aggregation
iso3c_gtap <- read.csv(here::here("input_data/iso3c_gtap.csv"))


# ========================================================================================
# SOURCE FUNCIONS ------------------------------------------------------------------------
# ========================================================================================

source(here("create_rd_stock/functions.R"))


# ========================================================================================
# PROCESS --------------------------------------------------------------------------------
# ========================================================================================

rd_investment_db <- grape_raw |>
  filter(variable == "RD") |>
  dplyr::select(country, iso3c, year, unit, rd_investment = value) |>
  arrange(iso3c, year) 


# ========================================================================================
# CREATE R&D STOCK FOR ALL COUNTRIES -----------------------------------------------------
# ========================================================================================

# Use country group specific parameters (see paper for classification of countries).
# Users can manually change the group in rd_stock_country_group.csv.
# We add parameters for each country group.

rd_stock_parameters <- country_group |>
  mutate(
    lambda = case_when(
      group == "A" ~ 0.7,
      group == "B" ~ 0.7,
      group == "C" ~ 0.6,
      group == "D" ~ 0.4,
      group == "E" ~ 0.5,
      group == "F" ~ 0.5,
      TRUE ~ 0
    ),
    delta = case_when(
      group == "A" ~ 0.9,
      group == "B" ~ 0.8,
      group == "C" ~ 0.85,
      group == "D" ~ 0.8,
      group == "E" ~ 0.9,
      group == "F" ~ 0.8,
      TRUE ~ 0
    ),
    L = case_when(
      group == "A" ~ 50,
      group == "B" ~ 35,
      group == "C" ~ 25,
      group == "D" ~ 15,
      group == "E" ~ 25,
      group == "F" ~ 15,
      TRUE ~ 0
    ),
    g = case_when(
      group == "A" ~ 0,
      group == "B" ~ 0,
      group == "C" ~ 0,
      group == "D" ~ 0,
      group == "E" ~ 0,
      group == "F" ~ 0,
      TRUE ~ 0
      )
  )

# Join parameters and R&D investment data
rd_investment_db <- rd_investment_db |>
  left_join(rd_stock_parameters)

# Compute R&D stock
rd_stock_db <- rd_investment_db |>
  group_by(iso3c) |>
  group_modify(~ rd_stock_single(
    df = .x,
    lambda = .x$lambda[1],
    delta  = .x$delta[1],
    L      = .x$L[1],
    g      = .x$g[1]
  )) |>
  ungroup()


# ========================================================================================
# AGGREGATE TO GTAP ----------------------------------------------------------------------
# ========================================================================================

# We aggregate to GTAP12 with 145 regions. 
# Change gtap12 into gtap11 and gtap12_name into gtap11_name for the 141 GTAP11.
rd_stock_gtap_db <- rd_stock_db |>
  dplyr::select(-country) |>
  left_join(iso3c_gtap, by = "iso3c") |>
  group_by(year, gtap12, gtap12_name) |>
  summarize(rd_stock = sum(rd_stock, na.rm = TRUE),
            .groups = "drop") 

# Identify GTAP regions with no R&D stock data
setdiff(rd_stock_gtap_db$gtap12, iso3c_gtap$gtap12)
setdiff(iso3c_gtap$gtap12, rd_stock_gtap_db$gtap12)

# We focus on recent period from 2004, the first GTAP year.
# Note that because of the long lags (max 50 years), the R&D stock in 2004 might be already
# affected by the R&D investment in the 1950s for which data generally missing (except for a few countries).
# This means that the R&D stock from 2011 (1961, first year of R&D observation plus lag of 50 years) onwards,
# is 100% build using all investment in previous years. Depending on the assumed lag,
# R&D stock before 2011 might also be estimated using all relevant R&D investment data.

period <- c(2004:2022)

# Add zero for missing regions and years
rd_stock_gtap_db <- rd_stock_gtap_db |>
  filter(year %in% period) |>
  dplyr::select(-gtap12_name) |>
  complete(gtap12 = iso3c_gtap$gtap12, year = period, fill = list(rd_stock = 0))
n_distinct(rd_stock_gtap_db$gtap12)

# Create HAR object
m <- rd_stock_gtap_db |>
  dplyr::select(REG = gtap12, YEAR = year, VALUE = rd_stock) |>
  arrange(REG, YEAR) |>
  tidyr::pivot_wider(names_from = REG, values_from = VALUE) |>
  arrange(YEAR) |>
  tibble::column_to_rownames("YEAR") |>
  as.matrix()
names(dimnames(m)) <- c("YEAR", "REG")

rd_stock_gtap_har <- list(RDST = m)


# ========================================================================================
# R&D INVESTMENT -------------------------------------------------------------------------
# ========================================================================================

# For completeness, we also create a har file with R&D investment data aggregated to GTAP12.

# We aggregate to GTAP12 with 145 regions. 
# Change gtap12 into gtap11 and gtap12_name into gtap11_name for the 141 GTAP11.
rd_investment_gtap_db <- rd_investment_db |>
  dplyr::select(-country) |>
  left_join(iso3c_gtap, by = "iso3c") |>
  group_by(year, gtap12, gtap12_name) |>
  summarize(rd_investment = sum(rd_investment, na.rm = TRUE),
            .groups = "drop")

# Identify GTAP regions with no R&D data
setdiff(rd_investment_gtap_db$gtap12, iso3c_gtap$gtap12)
setdiff(iso3c_gtap$gtap12, rd_investment_gtap_db$gtap12)

# Add zero for missing regions and years
rd_investment_gtap_db <- rd_investment_gtap_db |>
  dplyr::select(-gtap12_name) |>
  complete(gtap12 = iso3c_gtap$gtap12,
           year = c(min(rd_investment_db$year):max(rd_investment_db$year)),
                    fill = list(rd_investment = 0))
n_distinct(rd_investment_gtap_db$gtap12)

# Create HAR object
m2 <- rd_investment_gtap_db |>
  dplyr::select(REG = gtap12, YEAR = year, VALUE = rd_investment) |>
  arrange(REG, YEAR) |>
  tidyr::pivot_wider(names_from = REG, values_from = VALUE) |>
  arrange(YEAR) |>
  tibble::column_to_rownames("YEAR") |>
  as.matrix()
names(dimnames(m2)) <- c("YEAR", "REG")

rd_investment_gtap_har <- list(RDIN = m2)


# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

# set path to save output
# Note: if you do not change this, the original files will be overwritten. 
temp_path <- here("output_data")

# csv file
write_csv(rd_stock_gtap_db, file.path(temp_path, "ag_rd_stock_gtap_db.csv"))
write_csv(rd_investment_gtap_db, file.path(temp_path, "ag_rd_investment_gtap_db.csv"))

# base year file to har for further use in GTAP
write_har(rd_stock_gtap_har, file.path(temp_path, "ag_rd_st_gtap.har"))
write_har(rd_investment_gtap_har, file.path(temp_path, "ag_rd_in_gtap.har"))
