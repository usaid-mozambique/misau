# PROJECT:  
# AUTHOR:   J. Lara | USAID
# PURPOSE:  
# REF ID:   12810409 
# LICENSE:  MIT
# DATE:     2025-03-12
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
library(tidyverse)
library(gagglr)
library(janitor)
library(glue)
library(readxl)
library(openxlsx)
library(tidyxl)

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "12810409"
  path_pesoi <- "Data/PESOSE 2025 DNSP Actualizado 05.03.2025.xlsx"

# LOAD DATA ------------------------------------------------------------------
  
  df <- read_excel(path_pesoi, 
                   sheet = "Sheet1", 
                   skip =8) |> 
    clean_names()

# MUNGE -------------------------------------------------------------------
  