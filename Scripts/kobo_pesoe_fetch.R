
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
            observacao_label,
            subactividade_meta_label,
            starts_with("actividade_principal_objective_esp"),
            starts_with("subactividade_local_prov_detal_"))
  ) |> 
  relocate(c(username,`_uuid`, `_submission_time`, everything())) |>
  mutate(across(where(~inherits(., "haven_labelled")), haven::as_factor)) |> 
  mutate(across(.cols = !any_of(meta), .fns = as.character)) |> 
  separate(subactividade_local_prov_detal, into = paste0("local_", 1:12), sep = " ") |> 
  separate(actividade_principal, into = paste0("activity_principal_", 1:21), sep = " ") |> 
  pivot_longer(cols = !c(username,
                         `_uuid`,
                         `_submission_time`,
                         subactividade_data_inicio,
                         subactividade_data_fim),
               names_to = "indicator",
               values_to = "value") |> 
  filter(!is.na(value)) |> 
  mutate(
    indicator = case_when(
      
      indicator == "responsavel_programa" ~ "Programa Responsável",
      indicator == "responsavel_pf" ~ "Ponto Focal Responsável",
      
      indicator == "subactividade_descricao" ~ "Descrição da Subactividade",
      indicator == "subactividade_tipo" ~ "Tipo de Subactividade",
      indicator == "subactividade_beneficiario" ~ "Beneficiário da Subactividade",
      indicator == "subactividade_local" ~ "Local da Subactividade",
      
      indicator == "objectivo_pess" ~ "Objetivo PESS",
      indicator == "actividade_principal_001" ~ "Actividade Principal",
      
      indicator == "actividade_principal_indicador_001" ~ "Indicador de Producto da Actividade",
      indicator == "subactividade_indicador" ~ "Indicador da Subactividade",
      indicator == "actividade_meta" ~ "Meta da Actividade",
      indicator == "subactividade_meta" ~ "Meta da Subactividade",
      indicator == "meta_geografia_repeat_count" ~ "Meta Disaggreagada da Subactividade",
      
      indicator == "dias_central" ~ "Dias Central",
      indicator == "dias_provincial" ~ "Dias Provincial",
      
      indicator == "orcamento_oe" ~ "Orçamento do Estado",
      indicator == "orcamento_prosaude" ~ "Orçamento da ProSaude",
      indicator == "orcamento_outro_total" ~ "Orçamento Outro Total",
      
      indicator == "participantes_central" ~ "Participantes Central",
      indicator == "participantes_provincial" ~ "Participantes Provincial",
      indicator == "participantes_distrital" ~ "Participantes Distrital",
      
      indicator == "observacao" ~ "Observação",
      .default = indicator),
    
    indicator = str_replace(indicator, pattern = "local_.*", replacement = "Local"),
    indicator = str_replace(indicator, pattern = "activity_principal_.*", replacement = "Objectivo Especifico Ligado ao PESS"),
    
    value = case_when(
      value == "niassa" ~ "Niassa",
      value == "cabo_delgado" ~ "Cabo Delgado",
      value == "nampula" ~ "Nampula",
      value == "zambezia" ~ "Zambezia",
      value == "tete" ~ "Tete",
      value == "manica" ~ "Manica",
      value == "sofala" ~ "Sofala",
      value == "inhambane" ~ "Inhambane",
      value == "gaza" ~ "Gaza",
      value == "maputo_provincia" ~ "Maputo Provincia",
      value == "maputo_cidade" ~ "Cidade de Maputo",
      
      value == "norte" ~ "Norte",
      value == "centro" ~ "Centro",
      value == "sul" ~ "Sul",
      
      .default = value)
  )
  
  


csv <- df_flat_1 |> 
  distinct(indicator)

write_csv(csv,
          "Documents/distinct_ind.csv",
          na = "")
