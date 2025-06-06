
# LOAD DEPENDENCIES ---------------------------------------------------------


library(tidyverse)
library(ggthemes)
library(robotoolbox)
library(glamr)
library(dm)
library(haven)
library(glue)
library(googlesheets4)
source("Scripts/utilities.r")

acct_kobo <- "kobo-jlara"
acct_kobo_con <- get_account(name = acct_kobo)

kobo_token(username = acct_kobo_con$username,
           password = acct_kobo_con$password,
           url = acct_kobo_con$url)


# FETCH ASSETS ------------------------------------------------------------


assets <- kobo_asset_list()

uid <- assets %>%
  filter(name == "2026 DNSP PES") %>%
  pull(uid) %>%
  first()

asset_list <- kobo_asset(uid)

asset_df <- kobo_submissions(asset_list)


# DRAW SCHEMA ------------------------------------------------------------


dm_draw(asset_df)


# GANTT PLOT -------------------------------------------------------------------


df_tbl_dates <- asset_df$tbl_datas_impl

df_gantt <- asset_df %>%
  dm_flatten_to_tbl(.start = tbl_datas_impl,
                    .join = left_join) %>% 
  select(
    responsavel_programa,
    subactividade_tipo,
    subactividade_descricao,
    subactividade_data_inicio,
    subactividade_data_fim
  ) %>% 
  mutate(
    responsavel_programa = str_to_upper(responsavel_programa),
    data_inicio = subactividade_data_inicio,
    subactividade_descricao_wrapped = str_wrap(subactividade_descricao, width = 125),
    subactividade_descricao_wrapped = factor(subactividade_descricao_wrapped, levels = unique(subactividade_descricao_wrapped)),
    subactividade_descricao_short = str_trunc(subactividade_descricao, width = 100, ellipsis = "...")
  ) %>% 
  pivot_longer(
    cols = contains("_data_"),
    names_to = "data_tipo",
    values_to = "data"
  ) %>% 
  arrange(responsavel_programa, desc(data_inicio)) %>%
  mutate(
    subactividade_descricao_short = factor(
      subactividade_descricao_short,
      levels = unique(subactividade_descricao_short)
    )
  )


# plot activity gantt chart
df_gantt_plot <- ggplot(df_gantt) +
  geom_line(
    aes(x = subactividade_descricao_short, y = data, color = responsavel_programa),
    linewidth = 4,
    lineend = "butt",
    linejoin = "mitre"
  ) +
  scale_y_date(
    date_breaks = "1 month",
    date_labels = "%m-%Y"
  ) +
  coord_flip() +
  facet_wrap(~ responsavel_programa, ncol = 1, scales = "free_y") +
  theme_fivethirtyeight() +
  labs(
    title = "Monitoria das Actividades PES",
    subtitle = "Abaixo, visualiza-se o calendário de execução previsto para as subactividades da DNSP previstas no âmbito do PES.\n  As entradas estão organizadas primeiro por Programa e depois por data de início.",
    caption = "KoboToolbox, DNSP PES Piloto",
    x = NULL,
    y = "Date"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1, face = "italic"),
    legend.position = "none",
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    panel.spacing = unit(0.5, "lines"),
    panel.grid.minor = element_line(color = "white", linewidth = 0.5),
    strip.background = element_rect(fill = "grey80", color = "grey10"),
    strip.text = element_text(color = "grey10", 
                              face = "bold", 
                              size = 11,
                              margin = margin(t = 3, b = 3)
    )
  )

df_gantt_plot

ggsave("Images/pesoe_gantt.pdf",
       df_gantt_plot,
       width = 40,
       height = 40,
       units = "cm")


# DIMENSION VIZ PLOT --------------------------------------------------


df_orc <- asset_df$main %>% 
  select(
    `_index`,
    responsavel_programa,
    subactividade_tipo_label,
    subactividade_descricao,
    calc_consultor_total,
    calc_reproducao_total_custo,
    calc_estudo_investigador_total,
    calc_deslocacao_total,
    calc_contratacao_custo,
    orcamento_oe,
    orcamento_prosaude,
    orcamento_outro_total,
    calc_financiamento,
    calc_financiamento_lacuna
  ) %>% 
  mutate(across(
    !c(`_index`, 
       responsavel_programa, 
       subactividade_descricao, 
       subactividade_tipo_label),
    ~ ifelse(is.nan(.), 0, .)
  ),
  across(starts_with("calc_"), as.numeric),
  responsavel_programa = str_to_upper(responsavel_programa)) %>% 
  group_by(responsavel_programa) %>% 
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>% 
  mutate(
    orcamento_total = rowSums(across(c(calc_consultor_total, 
                                       calc_reproducao_total_custo,
                                       calc_estudo_investigador_total,
                                       calc_deslocacao_total,
                                       calc_contratacao_custo)))
  )


df_viz <- asset_df$main %>% 
  select(
    `_index`,
    responsavel_programa,
    subactividade_tipo_label,
    subactividade_descricao,
    subactividade_beneficiario,
    objectivo_pess,
    calc_consultor_total,
    calc_reproducao_total_custo,
    calc_estudo_investigador_total,
    calc_deslocacao_total,
    calc_contratacao_custo,
    orcamento_oe,
    orcamento_prosaude,
    orcamento_outro_total,
    calc_financiamento,
    calc_financiamento_lacuna
  ) %>% 
  mutate(across(
    !c(`_index`, 
       responsavel_programa, 
       subactividade_tipo_label,
       subactividade_descricao,
       subactividade_beneficiario,
       objectivo_pess
       ),
    ~ ifelse(is.nan(.), 0, .)),
    objectivo_pess = case_when(
      objectivo_pess == 'objective_pess_1' ~ 'Redução da mortalidade materna e neonatal',
      objectivo_pess == 'objective_pess_2' ~ 'Melhoria da saúde infantil incluindo a Nutrição',
      objectivo_pess == 'objective_pess_3' ~ 'Redução das grandes endemias',
      objectivo_pess == 'objective_pess_4' ~ 'Prevenção e tratamento das doenças crónicas',
      objectivo_pess == 'objective_pess_5' ~ 'Domínios e sistemas transversais'
    ),
    across(starts_with("calc_"), as.numeric),
    responsavel_programa = str_to_upper(responsavel_programa),
    orcamento_total = rowSums(across(c(calc_consultor_total, 
                                       calc_reproducao_total_custo,
                                       calc_estudo_investigador_total,
                                       calc_deslocacao_total,
                                       calc_contratacao_custo)),
                              na.rm = TRUE)
  )



plot_col <- function(df, dimension, metric) {
  
  # Lookup for user-friendly dimension names
  dim_mapper <- c(
    responsavel_programa = "Programa",
    subactividade_tipo_label = "Tipo de Subactividade",
    subactividade_descricao = "Descrição da Subactividade",
    subactividade_beneficiario = "Beneficiário",
    objectivo_pess = "Objectivo"
  )
  
  # Lookup for user-friendly metric names
  val_mapper <- c(
    orcamento_total = "Total Orçamento",
    orcamento_oe = "Orçamento do Estado",
    orcamento_prosaude = "Orçamento da ProSaude",
    orcamento_outro_total = "Outro Orçamento",
    calc_financiamento = "Financiamento previsto",
    calc_financiamento_lacuna = "Lacuna Financeira"
  )
  
  # Get the label for the selected dimension and metric
  dim_label <- dim_mapper[as.character(substitute(dimension))]
  val_label <- val_mapper[as.character(substitute(metric))]
  
  # Group & summarize dataframe
  df_summary <- df %>%
    group_by({{ dimension }}) %>%
    summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop")
  
  # Plot!
  ggplot(df_summary, aes(x = fct_reorder(as.factor({{ dimension }}), {{ metric }}),
                         y = {{ metric }},
                         fill = {{ dimension }})) +
    geom_col() +
    geom_text(aes(label = paste0(round({{ metric }} / 1e6, 1), "M")),
              hjust = -0.1, size = 3.2) +
    coord_flip() +
    theme_fivethirtyeight() +
    scale_y_continuous(
      labels = scales::label_comma(),
      expand = expansion(mult = c(0, 0.1))  # or try c(0, 0.15) if it's still too tight
    ) +
    scale_fill_viridis_d(option = "D") +  # <- added viridis color scale
    labs(
      title = glue::glue("{val_label} por {dim_label}"),
      subtitle = glue::glue("{val_label} desagregado por {dim_label} segundo as\n submissões feitas através doKoboToolbox no âmbito do projeto-piloto DNSP"),
      caption = "KoboToolbox, DNSP PES Piloto"
    ) +
    theme(
      plot.title = element_text(hjust = 0),
      plot.caption = element_text(hjust = 1, face = "italic"),
      legend.position = "none",
      axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 8)
    )
}



plot_col(df = df_viz, 
         dimension = subactividade_tipo_label, 
         metric = orcamento_outro_total)



# CHECK ALIGNMENT OF AGGREGATE AND GEO-DISAGGREGATED TARGETS -----------------------------------------------------------


c <- verificar_metas(asset_df = asset_df)
s <- gerar_ggsheet(asset_df = asset_df)


# WRITE TO DISK -----------------------------------------------------------


write_csv(
  s,
  file = "Dataout/ggsheet.csv",
  na = ""
)
