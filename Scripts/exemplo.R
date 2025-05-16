
library(tidyverse)
library(robotoolbox)
library(glamr)
library(dm)

# SET CREDENTIALS ---------------------------------------------------------

acct_kobo <- "kobo-jlara"
acct_kobo_con <- get_account(name = acct_kobo)

kobo_token(username = acct_kobo_con$username,
           password = acct_kobo_con$password,
           url = acct_kobo_con$url)

# FETCH ASSETS ------------------------------------------------------------

assets <- kobo_asset_list()

uid <- assets |>
  filter(name == "DNSP PESOE Rescunho v2") |>
  pull(uid) |>
  first()

asset <- kobo_asset(uid)

df <- kobo_submissions(asset)

# EXPLORE DATA ------------------------------------------------------------

rm(acct_kobo_con, asset, assets)

df <- df$main |> 
  as_tibble()


