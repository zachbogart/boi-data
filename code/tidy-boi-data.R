#' Tidying data for BoI data
#' Version: 2022-11-07
#' 
#' Notes:
#'

# Imports 
library(tidyverse)
library(here)

raw <- read_csv(
    here("data/BoI Achievements - raw_unlocks.csv"), 
    col_names = F
) %>% rename(raw = X1)

colnames_unlocks <- c("item", "description", "timestamp")
tidy <- raw %>% 
    mutate(
        col = ((row_number() - 1) %% 3) + 1,
        name = colnames_unlocks[col]
    ) %>% 
    mutate(tmp = col == 1, num = cumsum(tmp)) %>% 
    select(-c(col, tmp)) %>% 
    pivot_wider(names_from = name, values_from = raw)
    
