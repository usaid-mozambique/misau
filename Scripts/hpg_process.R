
library(tidyverse)
library(readxl)

# DEFINE GLOBAL VARIABLES -------------------------------------------------

path_usg <- "Data/Planilha_Planificação Integrada Todos parceiros_20250122.xlsx"

df <- read_excel(
  path = path_usg,
  skip = 4)

# LOAD DATA & PIVOT -------------------------------------------------------

df_import <- df |>
  fill(pilar, programa, intervencoes, .direction = "down") |>
  pivot_longer(
    cols = !c(pilar, programa, intervencoes),
    names_to = c("disag", "beneficiario", "doador", "mecanismo"),
    names_sep = "_",
    values_to = "value",
    values_transform = list(value = as.character)
  )

# MUNGE -------------------------------------------------------------------

df_clean <- df_import |>
  
# relocate & convert string values to title
  relocate(doador, .after = disag) |> 
  mutate(pilar = str_to_title(pilar),
         programa = str_to_title(programa),
         intervencoes = str_to_title(intervencoes)
  ) |>
  
# recode disag value
  mutate(disag = case_match(
    disag,
    "local" ~ "Por Beneficiario",
    "financ" ~ "Por Mecanismo")
  ) |>
  # recode doador value
  mutate(doador = case_match(
    doador,
    "grm" ~ "GRM",
    "cdc" ~ "CDC",
    "usaid" ~ "USAID",
    "unicef" ~ "Unicef",
    "prosaude" ~ "ProSaude",
    "canada" ~ "Canada",
    "gf" ~ "Fundo Global",
    "gavi" ~ "Gavi",
    "wb" ~ "Banco Mundial",
    .default = doador)
  ) |>
  mutate(psnuuid = case_match(
    beneficiario,
    "sps" ~ NA,
    "dps" ~ NA,
    "angoche" ~ "xIowtHPEVqN",
    "c.d.nampula" ~ "jwLNNIw1MjY",
    "erati" ~ "CotlFJ64kUi",
    "ilha.d.moc" ~ "zvN1CKVc0o5",
    "malema" ~ "RthlF0LdPnR",
    "mecuburi" ~ "YLkTM4jAvvq",
    "memba" ~ "dOIfGVyGIpn",
    "mogovolas" ~ "EBH5pfGP97m",
    "monapo" ~ "ztnYInN25Ev",
    "mossuril" ~ "twsAundvOb3",
    "muecate" ~ "TC4V2Gtm5am",
    "murrupula" ~ "vbDb0zzpQej",
    "nacala.porto" ~ "rzyQVCV9LjG",
    "mongincual" ~ "rdJibBp1ZPV",
    "lalaua" ~ "TpXEdX36fEz",
    "moma" ~ "j5uYmqh5tq6",
    "nacaroa" ~ "i25wGrBbMs1",
    "rapale" ~ "dhGsWCy06oC",
    "liupo" ~ "bEwMiJVVrEJ",
    "nacala.velha" ~ "Un8VIqAxgEx",
    "ribaue" ~ "DkiQIZm6OXK",
    "larde" ~ "Bpj2lcQ3uL4",
    "meconta" ~ "tXKwBtQqb2o",
    .default = beneficiario)
  ) |>
# recode beneficiario value
  mutate(beneficiario = case_match(
    beneficiario,
    "sps" ~ "SPS",
    "dps" ~ "DPS",
    "angoche" ~ "Angoche",
    "c.d.nampula" ~ "Cidade de Nampula",
    "erati" ~ "Erati",
    "ilha.d.moc" ~ "Ilha de Mocambique",
    "malema" ~ "Malema",
    "mecuburi" ~ "Mecuburi",
    "memba" ~ "Memba",
    "mogovolas" ~ "Mogovolas",
    "monapo" ~ "Monapo",
    "mossuril" ~ "Mossuril",
    "muecate" ~ "Muecate",
    "murrupula" ~ "Murrupula",
    "nacala.a.velha" ~ "Nacala-a-Velha",
    "nacala.porto" ~ "Nacala Porto",
    "mongincual" ~ "Mongincual",
    "lalaua" ~ "Lalaua",
    "moma" ~ "Moma",
    "nacaroa" ~ "Nacaroa",
    "rapale" ~ "Rapale",
    "liupo" ~ "Liupo",
    "nacala.velha" ~ "Nacala Velha",
    "murrupla" ~ "Murrupa",
    "ribaue" ~ "Ribaua",
    "larde" ~ "Larde",
    "meconta" ~ "Meconta",
    .default = beneficiario)
  ) |>
  mutate(mecanismo = case_match(
    mecanismo,
    "adpp" ~ "ADPP",
    "alcancar" ~ "Alcancar",
    "amasi" ~ "AMASI",
    "forte" ~ "FORTE",
    "amostra" ~ "Amostra",
    "passos" ~ "Passos",
    "fao" ~ "FAO",
    "ccs" ~ "CCS",
    "dps" ~ "DPS",
    "fdc" ~ "FDC",
    "fmsbc" ~ "FMSBC",
    "g2g" ~ "G2G",
    "icap" ~ "ICAP",
    "ifpi" ~ "IFPI",
    "jhpiego" ~ "Jhpiego",
    "mrite" ~ "M-RITE",
    "mssfpo" ~ "MSSFPO",
    "psm" ~ "PSM",
    "sps" ~ "SPS",
    "tn" ~ "Transform Nutrition",
    "visao.mundial" ~ "Visão Mundial",
    "pircom" ~ "PIRCOM",
    "chegar" ~ "Chegar",
    "mcaps" ~ "MCAPS",
    "nweti" ~ "N'weti",
    "sdsmas" ~ "SDSMAS",
    "hdcs1" ~ "HDCS1",
    .default = mecanismo)
  ) |>
  # recode values
  mutate(
    value2 = case_match(
      value,
      "x" ~ "1",
      "X" ~ "1",
      "XX" ~ "2",
      "XXX" ~ "3",
      "XXXX" ~ "4",
      "XXXXX" ~ "5",
      NA ~ "0",
      .default = "1"),
    value2 = as.numeric(value2),
    value3 = if_else(value2 > 0, 1, 0)
  ) |> 
  relocate(psnuuid, .after = beneficiario) |>
  
  rename(val_raw = value,
         val_count = value2,
         val_binary = value3) |> 
  drop_na(val_raw)


# QUALITY CONTROL ---------------------------------------------------------

df_clean |>
  
  distinct(disag) |> print(n=100)

df_clean |>
  distinct(disag, beneficiario, psnuuid) |> print(n=100)

df_clean |>
  distinct(doador) |> print(n=100)


df_clean |>
  arrange(disag, beneficiario) |>
  distinct(disag, beneficiario) |> 
  print(n=100)


df_clean |>
  arrange(disag, mecanismo) |>
  distinct(disag, mecanismo) |> 
  print(n=100)


check <- df_clean |> 
  filter(val_binary == 0)


# WRITE TO DISK ---------------------------------------------------------------

write_csv(df_clean, "Dataout/df_clean.csv")

