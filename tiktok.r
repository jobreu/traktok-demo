# Import data collected with zeeschuimer and parsed as .csv with 4 CAT ####

library(readr)

tiktok1 <- read_csv("INSERT_FILE_NAME_HERE") # Namen der entsprechenden Datei (inkl. Dateiendung) einfügen

names(tiktok1)

library(dplyr)

glimpse(tiktok1)

# Import and parse data from an .ndjson file collected with zeeschuimer ####

source("parse_tiktok.R")

tiktok2 <- parse_4cat_ndjson_to_tibble("INSERT_FILE_NAME_HERE") # Namen der entsprechenden Datei (inkl. Dateiendung) einfügen

names(tiktok2)

glimpse(tiktok2)

write_csv(tiktok2, "./tiktok_parsed.csv")

# Collect data with traktok ####

library(cookiemonster)
library(traktok)

add_cookies("cookies.txt")

user_info <- tt_user_info_hidden("franziska.brantner")
user_info

following <- tt_get_following_hidden(secuid = user_info$secUid,
                                     verbose = TRUE)

following

videos <- tt_user_videos_hidden("franziska.brantner",
                                save_video = FALSE)

videos

library(purrr)

accounts <- c("franziska.brantner", "judith.skudelny", "jamila.schaefer", "martin.hagen")

users <- map_df(
  accounts,
  ~ tt_user_info_hidden(.x)
)

users
