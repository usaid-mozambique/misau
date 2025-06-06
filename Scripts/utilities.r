## NEEDS
# data for looking up and changing variable coding (M&A staff, activity type, etc.)
# other quality checks

#' Verificar a coerência entre os metas agregadas e geo-disaggregadas
#'
#' @param asset_df 
#'
#' @returns df
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- verificar_metas(asset_df = asset_df)}

verificar_metas <- function(asset_df) {
  
  # Generate df for geo-disaggregated targets
  df_geo <- asset_df$tbl_geografia_impl %>% 
    select(`_parent_index`, meta_geo) %>% 
    group_by(`_parent_index`) %>% 
    summarize(subactividade_meta_geo = sum(meta_geo, na.rm = TRUE), .groups = 'drop')
  
  # Generate df for aggregate target and join with geo targets
  df <- asset_df$main %>% 
    select(responsavel_programa,
           subactividade_descricao_curta,
           `_index`,
           subactividade_meta) %>% 
    left_join(df_geo, by = c("_index" = "_parent_index")) %>% 
    mutate(erro = case_when(
      subactividade_meta == subactividade_meta_geo ~ NA_character_,
      .default = "por verificar"
    ))
  
  return(df)
  
}



#' Gerar df das subactividade PES para googlesheets
#'
#' @param asset_df 
#'
#' @returns df
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- gerar_ggsheet(asset_df = asset_df)}

gerar_ggsheet <- function(asset_df) {
  
  df <- asset_df %>%
    dm_flatten_to_tbl(.start = tbl_datas_impl,
                      .join = left_join) %>% 
    select(
      `_parent_index`,
      responsavel_programa,
      responsavel_pf_ma,
      responsavel_pf_prog,
      subactividade_tipo,
      subactividade_descricao_curta,
      subactividade_data_inicio,
      subactividade_data_fim,
      subactividade_meta,
      subactividade_indicador
    ) %>% 
    mutate(
      responsavel_programa = str_to_upper(responsavel_programa),
      estado = "Pendente"
    ) %>% 
    group_by(`_parent_index`) %>%
    filter(subactividade_data_inicio == min(subactividade_data_inicio, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(responsavel_programa, responsavel_pf_ma)
  
  return(df)
  
}