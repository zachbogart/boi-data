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
    mutate(num = cumsum(col == 1)) %>% 
    select(-c(col)) %>% 
    pivot_wider(names_from = name, values_from = raw) %>% 
    mutate(timestamp = str_replace(timestamp, "Unlocked ", ""))

export <- tidy %>% 
    # add in the year for recent achievements
    mutate(timestamp = case_when(
        !str_detect(timestamp, ",") ~ paste0(str_extract(timestamp, "[A-Za-z]+ [0-9]+"), ", 2022", str_extract(timestamp, " @.*$")),
        TRUE ~ timestamp
    )) %>% 
    # parse timestamp as datetime
    mutate(datetime = parse_datetime(timestamp, "%b %d, %Y @ %I:%M%p")) %>% 
    relocate(datetime, .before = description) %>% 
    arrange(datetime)
    
# save off as a CSV
export %>% 
    write_csv(here("outputs/boi-achievements.csv"))
