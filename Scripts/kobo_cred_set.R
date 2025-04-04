library(glamr)
library(robotoolbox)
library(rstudioapi)

# Se necessário, instale o `glamr` a partir do código abaixo
# install.packages('glamr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))

# Definir um valor de usuário
acct_kobo <- "kobo-jlara"

# url = "https://eu.kobotoolbox.org"
set_account(name = acct_kobo, 
            keys = c("url", 
                     "username", 
                     "password"), 
            update = TRUE)

acct_kobo_con <- get_account(name = acct_kobo)

kobo_token(username = acct_kobo_con$username,
           password = acct_kobo_con$password,
           url = acct_kobo_con$url)
