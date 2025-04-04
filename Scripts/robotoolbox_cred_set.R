
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
  filter(name == "PESOE DNSP") |>
  pull(uid) |>
  first()

asset <- kobo_asset(uid)

df <- kobo_submissions(asset)

# EXPLORE DATA ------------------------------------------------------------

dm_draw(df)

df_tbl <- df$main |> 
  as_tibble()

df_tbl_cron <- df$group_data |> 
  as_tibble()

df_flat <- df |>
  dm_flatten_to_tbl(.start = group_data,
                    .join = left_join)

