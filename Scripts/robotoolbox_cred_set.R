
library(tidyverse)
library(robotoolbox)


# use the below with username and password to obtain token
kobo_token(username = "",
           password = "",
           url = "https://eu.kobotoolbox.org")

# use the below to set token
kobo_setup(url = "https://eu.kobotoolbox.org",
           token = "")

# use the below to verify token settings
kobo_settings()

# use the below to generate asset list
assets <- kobo_asset_list()

uid <- assets |>
  filter(name == "PESOE DNSP") |>
  pull(uid) |>
  first()

asset <- kobo_asset(uid)


df <- kobo_submissions(asset)
