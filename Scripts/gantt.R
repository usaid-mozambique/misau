
library(tidyverse)
library(googlesheets4)
library(glamr)
library(janitor)
library(pins)
library(ggthemes)
load_secrets()


path_gsheet <- googlesheets4::as_sheets_id("1grY373VmU8fdJvX4IRRwa1PKdEzMxt6ph2zT2ucL8mg")

df <- googlesheets4::read_sheet(path_gsheet, sheet = "estado")

df_tidy <- df |> 
  clean_names() |> 
  select(!c(nota, indicador_de_actividades_processo)) |> 
  mutate(start = as.Date(data_do_inicio), end = as.Date(data_do_fim)) |> 
  pivot_longer(cols = starts_with("data_do_"),
               names_to = "date_type",
               values_to = "date")




plot_gantt <- function(df, responsavel_value) {
  df_filtered <- df %>%
    filter(responsavel == responsavel_value) %>%
    mutate(
      descricao_das_sub_actividades = factor(descricao_das_sub_actividades, levels = unique(descricao_das_sub_actividades))
    )
  
  ggplot() +
    geom_line(
      data = df_filtered,
      mapping = aes(x = descricao_das_sub_actividades, y = date, color = estado),
      size = 9
    ) +
    theme_fivethirtyeight() +
    coord_flip() +
    scale_color_manual(
      values = c(
        "Atrasado" = "#c0392b",
        "Cancelado" = "#d5d8dc",
        "Finalizado" = "#17a589",
        "Pendente" = "#f5b041"
      )
    ) +
    labs(
      title = "Monitoria das Actividades PESOE",
      x = NULL,
      y = "Date",
      color = "Estado"
    ) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      axis.text.y = element_text(hjust = 0.5),
      panel.grid.minor = element_line(color = "white", linewidth = 0.5),
      legend.position = "right",
      legend.direction = "vertical",
      plot.title = element_text(hjust = 0.5)
    )
}



plot_gantt(df_tidy, "DSF")
