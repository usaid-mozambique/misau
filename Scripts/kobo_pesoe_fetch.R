
library(tidyverse)
library(robotoolbox)
library(glamr)
library(dm)
library(haven)



# SET CREDENTIALS ---------------------------------------------------------

acct_kobo <- "kobo-jlara"
acct_kobo_con <- get_account(name = acct_kobo)

kobo_token(username = acct_kobo_con$username,
           password = acct_kobo_con$password,
           url = acct_kobo_con$url)

# FETCH ASSETS ------------------------------------------------------------

assets <- kobo_asset_list()
asset_name <- "DNSP PESOE Rescunho v2"

uid <- assets |>
  filter(name == asset_name) |>
  pull(uid) |>
  first()

asset_list <- kobo_asset(uid)

asset_df <- kobo_submissions(asset_list)

# EXPLORE DATA ------------------------------------------------------------

# dm_draw(asset_df)

df_tbl <- asset_df$main |> 
  as_tibble()

df_tbl_cron <- asset_df$grupo_data |>
  as_tibble()

df_flat <- asset_df |>
  dm_flatten_to_tbl(.start = grupo_data,
                    .join = left_join)

rm(asset_df, asset_list, assets, df_tbl, df_tbl_cron)

# MODS
# remove unnecessary vars ok
# reorder ok
# rename
# recode for labels ok
# recode actividade_principal



# RECODE ------------------------------------------------------------------

map_label <- read_csv("Documents/map_label.csv")

meta <- c("username",
          "_uuid",
          "_submission_time",
          "subactividade_data_inicio",
          "subactividade_data_fim")

df_flat_1 <- df_flat |> 
  select(!c(`__version__`,
            instanceID,
            `_xform_id_string`,
            rootUuid,
            `_status`,
            `_validation_status.main`,
            `_submitted_by`,
            `_attachments`,
            `_index`,
            `_parent_index`,
            `_parent_table_name`,
            `_validation_status.grupo_data`,
            `_id`,
            `uuid`,
            subactividade_local_prov_detal,
            actividade_principal)
  ) |> 
  relocate(c(username,`_uuid`, `_submission_time`, everything())) |>
  mutate(across(where(~inherits(., "haven_labelled")), haven::as_factor)) |> 
  mutate(across(.cols = !any_of(meta), .fns = as.character)) |> 
  pivot_longer(cols = !c(username,
                         `_uuid`,
                         `_submission_time`,
                         subactividade_data_inicio,
                         subactividade_data_fim),
               names_to = "variable",
               values_to = "value")


