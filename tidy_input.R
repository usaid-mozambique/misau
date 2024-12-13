
library(tidyverse)
library(readxl)

path_usg <- "Data/PPI USG.xlsx"

df <- read_excel(
  path = path_usg,
  skip = 4)

df1 <- df |> 
  fill(pilar, programa, intervencoes, .direction = "down") |> 
  pivot_longer(
    cols = !c(pilar, programa, intervencoes),
    names_to = c("tipo", "disag", "financiador"), 
    names_sep = "_", 
    values_to = "value", 
    values_transform = list(value = as.character)
  ) |>  
  
  
  mutate(value2 = value, 
         value2 = case_match(
           value2,
           "x" ~ "1",
           "X" ~ "1",
           "XX" ~ "2",
           "XXX" ~ "3",
           NA ~ "0",
           .default = "1"),
         pilar = str_to_title(pilar),
         programa = str_to_title(programa),
         intervencoes = str_to_title(intervencoes)
  ) |> 
  
  mutate(tipo = case_match(
    tipo,
    "local" ~ "Localizacao",
    "financ" ~ "Financiamento")
    ) |> 
  
  mutate(financiador = case_match(
    financiador,
    "grm" ~ "GRM",
    "cdc" ~ "CDC",
    "usaid" ~ "USAID",
    "unicef" ~ "Unicef",
    "canada" ~ "Canada",
    "gf" ~ "Fundo Global",
    "gavi" ~ "Gavi",
    "wb" ~ "Banco Mundial")
    ) |> 
  
  mutate(disag = case_match(
    disag,
    "grm" ~ "GRM",
    "cdc" ~ "CDC",
    "usaid" ~ "USAID",
    "unicef" ~ "Unicef",
    "canada" ~ "Canada",
    "gf" ~ "Fundo Global",
    "gavi" ~ "Gavi",
    "wb" ~ "Banco Mundial",
    "sps" ~ "SPS",
    "dps" ~ "DPS",
    "jhpiego" ~ "Jhpiego",
    "icap" ~ "ICAP",
    "adpp" ~ "ADPP",
    "alcancar" ~ "Alcancar",
    "mssfpo" ~ "MSSFPO",
    "ifpi" ~ "IFPI",
    "g2g" ~ "G2G",
    "amasi" ~ "AMASI",
    "ip1" ~ "IP1",
    "ip2" ~ "IP2",
    "ip3" ~ "IP3",
    "ip4" ~ "IP4",
    "fdc" ~ "FDC",
    "visao.mundial" ~ "Visao Mundial",
    "hum" ~ "HUM" )
    )

df1 |> 
  distinct(pilar) # ok

df1 |> 
  distinct(programa) |> print(n=100) # ok

df1 |> 
  distinct(intervencoes) |> print(n=100)

df1 |> 
  distinct(tipo) |> print(n=100) # ok

df1 |> 
  distinct(disag) |> print(n=100) # recoding needed on local and beneficiary

df1 |> 
  distinct(financiador) |> print(n=100) # recoding needed on financer

df1 |> 
  filter(tipo == "financ") |> 
  distinct(tipo, disag, financiador) |> print(n=100)


df1 |> 
  distinct(value,
           value2) |> 
  print(n=100)

ts <- df1 |> 
  filter(value2 == "1")



