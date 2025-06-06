
# LOAD DEPENDENCIES ---------------------------------------------------------


library(tidyverse)
library(robotoolbox)
library(glamr)
library(dm)
library(haven)

acct_kobo <- "kobo-jlara"
acct_kobo_con <- get_account(name = acct_kobo)

kobo_token(username = acct_kobo_con$username,
           password = acct_kobo_con$password,
           url = acct_kobo_con$url)


# FETCH KOBO ASSETS ------------------------------------------------------------


assets <- kobo_asset_list()

uid <- assets |>
  filter(name == "DNSP PES Piloto") |>
  pull(uid) |>
  first()

asset_list <- kobo_asset(uid)

asset_df <- kobo_submissions(asset_list)


# IMPORT LOCAL DATA -------------------------------------------------------


map_label <- read_csv("Documents/map_label.csv") |> 
  select(indicator = name, label)

main_vars <- c(
  "_index",
  "responsavel_programa",
  "objectivo_pess", 
  "actividade_principal",
  "actividade_principal_indicador_001",
  "actividade_meta",
  "subactividade_tipo",
  "subactividade_tipo_outro_espec",
  "subactividade_descricao",
  "subactividade_beneficiario",
  "subactividade_indicador",
  "subactividade_meta",
  "subactividade_local",
  "subactividade_local_inter_deta",
  "subactividade_local_reg_detalh",
  "subactividade_local_prov_detal",
  "subactividade_meta_label",
  "orcamento_oe",
  "orcamento_prosaude",
  "orcamento_outro_total",
  "calc_financiamento_lacuna"
)


# DRAW SCHEMA ------------------------------------------------------------


dm_draw(asset_df)


# CREATE MAIN DF --------------------------------------------------------------


df_tbl_main_pesoe <- asset_df$main |> 
  select(all_of(main_vars)) |> 
  pivot_longer(cols = c(responsavel_programa, 
                        objectivo_pess,
                        subactividade_tipo,
                        subactividade_local),
               names_to = "indicator",
               values_to = "value") |> 
  left_join(map_label,
            by = c("value" = "indicator")
  ) |> 
  select(!c(value,
            actividade_principal)) |> 
  pivot_wider(names_from = indicator,
              values_from = label) %>%
  mutate(subactividade_meta_label = as.character(subactividade_meta_label))


# CREATE OBJECTIVES DF ----------------------------------------------------

df_tbl_obj_esp <- asset_df$main |> 
  select(`_index`,
         responsavel_programa,
         starts_with("actividade_principal_objective_esp_")) |> 
  pivot_longer(cols = !c(`_index`,
                         responsavel_programa),
               names_to = "indicator",
               values_to = "value") |> 
  mutate(indicator = str_replace_all(indicator, "actividade_principal_", "")) |> 
  filter(value == 1)

objective_codes <- map_label %>%
  filter(str_starts(indicator, "objective_esp_")) %>%
  distinct(indicator) %>%
  mutate(objective_esp_code = paste0("A", row_number()))

# Join back to the full dataframe
df_tbl_obj_esp <- df_tbl_obj_esp %>%
  left_join(map_label, by = c("responsavel_programa" = "indicator")) |>
  left_join(objective_codes, by = "indicator") |>
  left_join(map_label, by = "indicator") |> 
  select('_index',
         responsavel_programa = label.x,
         principal = label.y,
         obj_esp_cod = objective_esp_code) |> 
  mutate(codigo = str_c("DNSP", responsavel_programa, obj_esp_cod, sep = "-")) |> 
  select(`_index`,
         codigo) %>% 
  group_by(`_index`) %>%
  summarise(
    codigo = paste(unique(codigo), collapse = ", "),
    .groups = "drop"
  )


# CREATE GEO TARGET DF -------------------------------------------------------


df_tbl_target_geo <- asset_df$meta_geografia_repeat |> 
  select(`_parent_index`, geo_nome_geo_label, meta_geo) |> 
  mutate(
    localizacao = str_c(geo_nome_geo_label, " (", meta_geo, ")")
  ) |> 
  pivot_wider(
    names_from = geo_nome_geo_label,
    values_from = localizacao,
    values_fn = list(localizacao = ~ paste(.x, collapse = ", "))
  ) |> 
  select(!meta_geo)

# Make sure `_parent_index` is still present
df_tbl_target_geo <- df_tbl_target_geo %>% relocate(`_parent_index`, .before = everything())

# Collapse values across each `_parent_index`
df_tbl_target_geo <- df_tbl_target_geo %>%
  group_by(`_parent_index`) %>%
  summarise(
    across(
      .cols = everything(), 
      .fns = ~ {
        vals <- na.omit(unique(.x))
        if (length(vals) == 0) NA_character_ else paste(vals, collapse = ", ")
      }
    ),
    .groups = "drop"
  ) %>% 
  mutate(
    localizacao = pmap_chr(
      select(., -`_parent_index`),
      ~ c(...) %>%
        discard(is.na) %>%
        paste(collapse = ", ")
    )
  ) %>% 
  select(`_parent_index`,
         localizacao)


# JOIN DFS & CLEAN -------------------------------------------------------------


df_tbl_final <- df_tbl_main_pesoe %>% 
  left_join(df_tbl_obj_esp, by = "_index") %>% 
  left_join(df_tbl_target_geo, by = c("_index" = "_parent_index")) %>% 
  mutate(localizacao = case_when(
    subactividade_local == "Nível Central" ~ str_c("Central (", subactividade_meta_label, ")"),
    subactividade_local == "Nível Internacional" ~ str_c("Central (", subactividade_meta_label, ")"),
    .default = localizacao
  )) %>% 
  select(
    codigo,
    principal = objectivo_pess,
    indicador_producto = actividade_principal_indicador_001,
    meta_global = actividade_meta,
    responsavel = responsavel_programa,
    subactividade_descricao,
    subactividade_meta, 
    indicador_process = subactividade_indicador,
    localizacao,
    subactividade_beneficiario,
    orcamento_oe,
    orcamento_prosaude,
    orcamento_outro = orcamento_outro_total,
    financiamento_lacuna = calc_financiamento_lacuna
  ) %>% 
  mutate(financiamento_lacuna = 0)


# WRITE FINAL DF TO DISK -----------------------------------------------------------


write_csv(df_tbl_final, 
          "Documents/pesoe_export.csv",
          na = "")
