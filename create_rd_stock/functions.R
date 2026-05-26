# ========================================================================================
# Project:  ag_rd_stock
# Subject:  Prepare data
# Author:   Michiel van Dijk
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
# FUNCTIONS ------------------------------------------------------------------------------
# ========================================================================================

# Function to create gammy weights
gamma_weights <- function(lambda, delta, L, g = 0) {
  k <- 0:L 
  w <- ifelse(
    k >= g,
    (k - g + 1)^(delta / (1 - delta)) * lambda^(k - g),
    0
  )
  w <- w / sum(w)
  return(w)
}

# Function to create R&D stock of a single country
rd_stock_single <- function(df, lambda, delta, L, g = 0) {
  df <- df[order(df$year), ]
  rd_invest <- df$rd_investment
  n <- length(rd_invest)
  w <- gamma_weights(lambda, delta, L, g)
  
  rd_stock <- rep(NA_real_, n)
  
  for (t in seq_len(n)) {
    k_max <- min(L, t - 1)
    
    rd_invest_slice <- rd_invest[(t - k_max):t]
    w_slice <- w[1:(k_max + 1)]
    
    rd_stock[t] <- sum(w_slice * rev(rd_invest_slice))
  }
  
  df$rd_stock <- rd_stock
  return(df)
}
