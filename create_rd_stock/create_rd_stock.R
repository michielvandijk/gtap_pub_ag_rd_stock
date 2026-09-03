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
  left_join(iso3c_gtap) |>
  group_by(year, gtap12, gtap12_name) |>
  summarize(rd_stock = sum(rd_stock, na.rm = TRUE),
            .groups = "drop") 

# Identify GTAP regions with no R&D data
setdiff(rd_stock_gtap_db$gtap12, iso3c_gtap$gtap12)
setdiff(iso3c_gtap$gtap12, rd_stock_gtap_db$gtap12)

# Add zero for missing values. We focus on recent period afrom 2004, first GTAP year.
period <- c(2004:2022)

rd_stock_gtap_db <- rd_stock_gtap_db |>
  filter(year %in% period) |>
  dplyr::select(-gtap12_name) |>
  complete(gtap12 = iso3c_gtap$gtap12, year = period, fill = list(rd_stock = 0))
n_distinct(rd_stock_gtap_db$gtap12)

# GTAP base year
gtap_base_year <- 2017

rd_stock_gtap_by <- rd_stock_gtap_db |>
  filter(year == gtap_base_year) |>
  dplyr::select(REG = gtap12, VALUE = rd_stock) |>
  arrange(REG)

# Create HAR object
# Note. 0 values will be dropped.
rd_stock_gtap_by_har <- list(
  RD = array(
    rd_stock_gtap_by$VALUE,
    dim = c(nrow(rd_stock_gtap_by), 1),
    dimnames = list(
      REG = rd_stock_gtap_by$REG,
      COL = "AGRD"
    )
  )
)


# ========================================================================================
# SAVE -----------------------------------------------------------------------------------
# ========================================================================================

# set path to save output
# Note: if you do not change this, the original files will be overwritten. 
temp_path <- here("output_data")

# csv file
write_csv(rd_stock_gtap_db, file.path(temp_path, "rd_stock_gtap_db.csv"))

# base year file to har for further use in GTAP
write_har(rd_stock_gtap_by_har, file.path(temp_path, "rd_ag_gtap_base_year.har"))

