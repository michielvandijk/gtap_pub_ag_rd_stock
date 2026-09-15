# ========================================================================================
# Project:  gtap_pub_ag_rd_stock
# Reference: Yin et al. (20xx), Harmonized Global Public Agricultural R&D Stocks for the GTAP Database, [DOI TO BE ADDED]
# Subject:  Figures
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================

# Load pacman for p_load
if(!require(pacman)) install.packages("pacman")
library(pacman)

# Load key packages
p_load(here, tidyverse, readxl, stringr, scales, glue, dplyr, HARr, rnaturalearth, countrycode,
       slider)

# R options
options(scipen = 999)
options(digits = 4) 

# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# R&D stock data
rd_stock_gtap_db <- read_csv(here("output_data/ag_rd_stock_gtap_db.csv"))

# R&D investment data
rd_investment_gtap_db <- read_csv(here("output_data/ag_rd_investment_gtap_db.csv"))

# Agricultural value added in 2017 PPP$ data from GRAPE macro database
ag_va_db <- read_excel(here("input_data/macro_db_v1.0.0.xlsx")) |>
  dplyr::select(country, iso3c, year, ag_gdp_ppp) 

# GTAP 11 regional aggregation
iso3c_gtap <- read.csv(here::here("input_data/iso3c_gtap.csv"))


# ========================================================================================
# SOURCE FUNCIONS ------------------------------------------------------------------------
# ========================================================================================

source(here("create_rd_stock/functions.R"))


# ========================================================================================
# LAG STRUCTURES BY COUNTRY GROUP --------------------------------------------------------
# ========================================================================================

# Create data.frame with distributions
gamma_df <- bind_rows(
  # Group A
  data.frame(
    value = gamma_weights(g = 0, lambda = 0.7, delta = 0.9, L = 50)
  ) |>
    mutate(year = row_number() - 1,
           param = "\u03b4 = 0.90, \u03bb = 0.7, lag = 50 years (Group A)",
           group = "A"),
  
  # Group B
  data.frame(
    value = gamma_weights(g = 0, lambda = 0.7, delta = 0.8, L = 35)
  ) |>
    mutate(year = row_number() - 1,
           param = "\u03b4 = 0.80, \u03bb = 0.7, lag = 35 years (Group B)",
           group = "B"),
  
  # Group C
  data.frame(
    value = gamma_weights(g = 0, lambda = 0.6, delta = 0.85, L = 25)
  ) |>
    mutate(year =  row_number() - 1,
           param = "\u03b4 = 0.85, \u03bb = 0.6, lag = 25 years (Group C)",
           group = "C"),
  
  # Group D
  data.frame(
    value = gamma_weights(g = 0, lambda = 0.4, delta = 0.8, L = 15)
  ) |>
    mutate(year =  row_number() - 1,
           param = "\u03b4 = 0.80, \u03bb = 0.4, lag = 15 years (Group D)",
           group = "D"),
  
  # Group E
  data.frame(
    value = gamma_weights(g = 0, lambda = 0.5, delta = 0.9, L = 25)
  ) |>
    mutate(year =  row_number() - 1,
           param = "\u03b4 = 0.90, \u03bb = 0.5, lag = 25 years (Group E)",
           group = "E"),
  
  # Group F
  data.frame(
    value = gamma_weights(g = 0, lambda = 0.5, delta = 0.8, L = 15)
  ) |>
    mutate(year =  row_number() - 1,
           param = "\u03b4 = 0.80, \u03bb = 0.5, lag = 15 years (Group F)",
           group = "F")
) |>
  mutate(group = factor(group, c("A", "B", "C", "D", "E", "F")))

# Nice colors
cb_pal <- palette.colors(9, palette = "Okabe-Ito")

gamma_df |>
  arrange(group) |>
  mutate(param = factor(param, levels = unique(param))) |>
  ggplot(aes(x = year, y = value, color = param, linetype = param)) +
  geom_line(linewidth = 1) +
  labs(y = expression(beta), x = NULL, color = NULL, linetype = NULL) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_color_manual(values = cb_pal[c(2,3,4,6,7,8)]) +
  theme_classic() +
  theme(legend.position = "bottom")


# ========================================================================================
# PROCESS AGRICULTURAL VALUE ADDED DATA --------------------------------------------------
# ======================================================================================== 

# We aggregate to GTAP12 with 145 regions and express in million 2017 PPP$.
ag_va_gtap_db <- ag_va_db |>
  dplyr::select(-country) |>
  left_join(iso3c_gtap, by = "iso3c") |>
  group_by(year, gtap12, gtap12_name) |>
  summarize(ag_gdp_ppp = sum(ag_gdp_ppp, na.rm = TRUE)/1000,
            .groups = "drop")

# Identify GTAP regions with no R&D data
setdiff(ag_va_gtap_db$gtap12, iso3c_gtap$gtap12)
setdiff(iso3c_gtap$gtap12, ag_va_gtap_db$gtap12)


# ========================================================================================
# PUBLIC AGRICULTURAL R&D INVESTMENT -----------------------------------------------------
# ========================================================================================

# 3 year rolling mean
rd_investment_gtap_3y_db <- rd_investment_gtap_db |>
  arrange(gtap12, year) |>
  group_by(gtap12) |>
  mutate(rd_investment = slide_dbl(rd_investment, mean, .before = 3, .complete = TRUE)) |>
  na.omit()

# Select top 10
top_rd_investment <- rd_investment_gtap_3y_db |>
  filter(year %in% c(2022)) |>
  group_by(year) |>
  slice_max(rd_investment, n = 8)

rd_investment_gtap_3y_db |>
  filter(gtap12 %in% top_rd_investment$gtap12) |>
  ggplot(aes(x = year, y = rd_investment,
             color = gtap12, group = gtap12, shape = gtap12)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) +
  scale_shape_manual(values = 1:12) +
  scale_color_manual(values = unname(cb_pal)) +
  scale_x_continuous(limits = c(1973, 2022)) +  
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "Million 2017 USD PPP", 
       color = NULL, shape = NULL) +
  theme_classic() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1),
         shape = guide_legend(nrow = 1))



# ========================================================================================
# PUBLIC AGRICULTURAL R&D STOCK ----------------------------------------------------------
# ========================================================================================

# MAP ------------------------------------------------------------------------------------

# Obtain world map and merge polygons to so they reflect the GTAP aggregation
world_map <- ne_countries(returnclass = 'sf') |>
  dplyr::select(iso3c = adm0_a3, admin) |>
  mutate(country = countrycode(iso3c, "iso3c", "country.name")) 

# Note that not all countries in the GTAP region list are included in the world map.
# Also note that some countries exist of multiple polygons (e.g. overseas territories)
# GTAP countries that do not have a polygon, mostly tiny islands
countrycode(setdiff(iso3c_gtap$iso3c, world_map$iso3c), "iso3c", "country.name")

# World map regions that do not feature in GTAP
countrycode(setdiff(world_map$iso3c, iso3c_gtap$iso3c), "iso3c", "country.name")
setdiff(world_map$iso3c, iso3c_gtap$iso3c)

# Create figure labels
breaks <- c(0, 2, 5, 25, Inf)
labels <- paste0(
  format(head(breaks, -1), big.mark = ",", trim = TRUE),
  " – ",
  format(tail(breaks, -1), big.mark = ",", trim = TRUE)
)
labels[4] <- "> 25"

# Link agricultural value added data to GTAP regions
# Only select base year data and countries with data
# Express R&D stock as percentage of value added
gtap_base_year <- 2017
rd_stock_gtap_by <- rd_stock_gtap_db |> 
  left_join(ag_va_gtap_db, by = c("gtap12", "year")) |>
  filter(year == gtap_base_year) |>
  filter(!(is.na(ag_gdp_ppp) | ag_gdp_ppp == 0)) |>
  mutate(rd_stock_norm = (rd_stock / (ag_gdp_ppp)*100),
         rd_stock_norm_bin = cut(rd_stock_norm, 
                                 breaks = breaks, 
                                 labels = labels,
                                 include.lowest = TRUE))
summary(rd_stock_gtap_by$rd_stock_norm)

# Plot
world_map |>
  filter(iso3c != "ATA") |>
  left_join(iso3c_gtap, by = c("iso3c")) |>
  left_join(rd_stock_gtap_by) |>
  mutate(rd_stock_norm_bin = fct_na_value_to_level(rd_stock_norm_bin, "No data")) |>
  ggplot() +
  geom_sf(aes(fill = rd_stock_norm_bin), colour = "black") +
  scale_fill_manual(
    values = cb_pal[c(2,3,4,7,9)],
    guide = guide_legend(title.position = "top", title.hjust = 0.5)) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(fill = "Public agricultural R&D capital stock\n as a share of agricultural value added (%)")


# BAR CHART ------------------------------------------------------------------------------

# Select top 10 in terms of stock
rd_stock_gtap_by |>
  arrange(desc(rd_stock_norm)) |>
  filter(!gtap12 %in% c("xef")) |>
  slice_head(n = 10)  |>
  ggplot(aes(x = reorder(gtap12, -rd_stock_norm), y = rd_stock_norm)) +
  geom_bar(stat = "identity", fill = cb_pal[6]) +
  geom_text(aes(label = comma(round(rd_stock_norm, 0))), vjust = -0.5, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     labels = comma) +
  labs(x = "", y = "Public agricultural R&D capital stock\nas a share of agricultural value added (%)") +
  theme_classic() 

