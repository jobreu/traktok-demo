# Accounts from the dataset "Social Media Accounts (TikTok, YouTube, X/Twitter) of the Candidates in the 2025 German Federal Election"
# https://search.gesis.org/research_data/SDN-10.7802-2862

library(readr)
library(dplyr)

btw25_accounts <- read_csv2("INSERT_PATH_HERE")

names(btw25_accounts)

btw_25_tiktok <- btw25_accounts |>
  filter(!is.na(handle_TikTok)) |>
  arrange(list_position) |>
  select(name, first_names, gender, party, list_position,
         handle_TikTok) |>
  head(5)
