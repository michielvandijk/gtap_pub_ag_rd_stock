# ========================================================================================
# Project:  gtap_pub_ag_rd_stock
# Subject:  Figures
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================

# Load pacman for p_load
if(!require(pacman)) install.packages("pacman")
library(pacman)

# Load key packages
p_load(here, tidyverse, readxl, stringr, scales, glue, dplyr, HARr, rnaturalearth,countrycode,
       classInt)

# R options
options(scipen = 999)
options(digits = 4) 

# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# R&D stock data
rd_stock_gtap_db <- read_csv(here("output_data/rd_stock_gtap_db.csv"))

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
  theme(legend.position = c(0.7,0.8))


# ========================================================================================
# MAP ------------------------------------------------------------------------------------
# ========================================================================================

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

# Jenks natural breaks, which minimizes the variance within bins and maximizes the variance between bins. 
breaks <- classIntervals(rd_stock_gtap_db$rd_stock, n = 5, style = "jenks")$brks

# Proper break format
b_int <- round(breaks)
labels <- paste0(
  format(head(b_int, -1), big.mark = ",", trim = TRUE),
  " – ",
  format(tail(b_int, -1), big.mark = ",", trim = TRUE)
)

# Only select base year data
gtap_base_year <- 2017
rd_stock_gtap_by <- rd_stock_gtap_db |> 
  filter(year == gtap_base_year) |>
  mutate(bin = cut(rd_stock, breaks = breaks, include.lowest = TRUE, labels = labels)) |>
  select(gtap12, bin, rd_stock) 

# Plot
world_map |>
  filter(iso3c != "ATA") |>
  left_join(iso3c_gtap, by = c("iso3c")) |>
  left_join(rd_stock_gtap_by) |>
  mutate(bin = fct_na_value_to_level(bin, "No data")) |>
  ggplot() +
  geom_sf(aes(fill = bin), colour = "black") +
  scale_fill_manual(values = cb_pal[c(2,3,4,7,8,9)]) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(fill = "Public agricultural\nR&D stock (million 2017 PPP$)")


# ========================================================================================
# BAR CHART ------------------------------------------------------------------------------
# ========================================================================================

# Select top 10 in terms of stock
rd_stock_gtap_by |>
  arrange(desc(rd_stock)) |>
  slice_head(n = 10)  |>
  ggplot(aes(x = reorder(gtap12, -rd_stock), y = rd_stock)) +
  geom_bar(stat = "identity", fill = cb_pal[6]) +
  geom_text(aes(label = comma(round(rd_stock, 0))), vjust = -0.5, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     labels = comma) +
  labs(x = "", y = "Public agricultural R&D stock (million 2017 PPP$)") +
  theme_classic() 
