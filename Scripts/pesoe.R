# PROJECT:  
# AUTHOR:   J. Lara | USAID
# PURPOSE:  
# REF ID:   12810409 
# LICENSE:  MIT
# DATE:     2025-03-12
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
library(tidyverse)
library(janitor)
library(glue)
library(readxl)
library(tidyxl)
library(stringi)

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "12810409"
  path_pesoi <- "Data/PESOSE 2025 DNSP Actualizado 18.03.2025.xlsx"

# LOAD DATA ------------------------------------------------------------------
  
  df <- read_excel(path_pesoi, 
                   sheet = "Sheet1", 
                   skip = 8) |> 
    clean_names() |> 
    slice(-(1:4)) |> 
    mutate(subactividade_tipo = stri_trans_general(subactividade_tipo, "latin-ascii"))


# TIDY XL FORMATTING ------------------------------------------------------
  
  # xlsx_data <- xlsx_cells(path_pesoi, sheets = "Sheet1")
  # xlsx_formats <- xlsx_formats(path_pesoi)
  # 
  # xlsx_formats <- xlsx_formats(path_pesoi)
  # 
  # # Convert formatting list to a data frame
  # xlsx_formats_df <- as_tibble(xlsx_formats$local)  # Ensures it's a tibble
  # 
# MUNGE -------------------------------------------------------------------
  
  
  df1 <- df |> 
    mutate(row_num = row_number()) |>  # Create a row index
    filter(!(row_num %in% unlist(map(which(no == "Sub-total"), ~ seq(.x, .x + 4))))) |> 
    select(-c(row_num, valor)) |>  
    fill(no, codigo, principal, indicador_produto, meta_global, responsavel, .direction = "down") |>
    mutate(across(starts_with("cal_"), ~as.character(.))) |> 
    pivot_longer(cols = starts_with("orcamento"), 
                 names_to = "valor_fonte",
                 values_to = "valor") |> 
    filter(!is.na(valor)) |> 
    mutate(valor_fonte = str_remove(valor_fonte, "orcamento_")) |> 
    pivot_longer(cols = starts_with("cal_"),
                 names_to = "periodo",
                 values_to = "implementacao") |> 
    mutate(periodo = str_remove(periodo, "cal_"),
           valor_fonte = case_when(valor_fonte == "oe" ~ "OE",
                                   valor_fonte == "prosaude" ~ "Prosaude",
                                   valor_fonte == "outro" ~ "Outro",
                                   .default = valor_fonte)) |> 
    separate_wider_delim(periodo, delim = "_", names = c("mes", "semana")) |> 
    mutate(implementacao = case_when(implementacao == "x" ~ 1,
                                     .default = 0)) |> 
    # select(!test) |> 
    mutate(mes = case_when(mes == "jan" ~ "jan-2025",
                           mes == "fev" ~ "feb-2025",
                           mes == "mar" ~ "mar-2025",
                           mes == "abr" ~ "apr-2025",
                           mes == "mai" ~ "may-2025",
                           mes == "jun" ~ "jun-2025",
                           mes == "jul" ~ "jul-2025",
                           mes == "ago" ~ "aug-2025",
                           mes == "set" ~ "sep-2025",
                           mes == "out" ~ "oct-2025",
                           mes == "nov" ~ "nov-2025",
                           mes == "dez" ~ "dec-2025")
           ) |>
    glimpse()

  
  princial <- df1 |> 
    distinct(principal) |> 
    as_tibble() |> 
    write_tsv("Documents/principal.txt", na = "")
  
  
# WRITE TO DISK -----------------------------------------------------------

  write_csv(df1, "Dataout/pesoi_clean.csv", na = "")  
  write_tsv(df1, "Dataout/pesoi_clean.tsv", na = "")
  
  unique(df1$responsavel)
  