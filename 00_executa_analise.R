#---- renv & RSPM para manter a reproducibilidade

# https://github.com/grantmcdermott/renv-rspm
#renv::init()    ## Automatically run if you cloned/opened the repo as an RStudio project
renv::restore()  ## Enter "y" when prompted
options(encoding = "UTF-8",
        repos = c(RSPM = "https://packagemanager.rstudio.com/all/latest"))

source("01_gera_dados.R")
source("02_grafico_federais.R")
source("03_grafico_nacional.R")
