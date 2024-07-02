# Bibliotecas ==================================================================

if(!require(pacman)) install.packages("pacman")
library(pacman)

p_load(
  geobr,        # Geospatial data related to Brazil
  tidyverse,    # Data manipulation and visualization
  readxl,       # Reading Excel files
  paletteer,    # Color palettes for data visualization
  ggplot2,      # Advanced data visualizations using the Grammar of Graphics
  dplyr,        # Data manipulation tasks
  classInt,     # Classification intervals for mapping and visualization
  raster,       # Working with raster data
  # rgdal,      # Deprecated, used for reading and writing geospatial data files
  dismo,        # Species distribution modeling and ecological niche modeling
  XML,          # Reading and parsing XML files
  maps,         # Accessing geographical maps and creating static maps
  sp,           # Spatial data representation and manipulation
  bivariatemaps,# Creating bivariate choropleth maps
  colorspace,   # Manipulating and selecting color palettes
  biscale,      # Creating bivariate choropleth maps with two diverging color scales
  cowplot,      # Simplifying the creation of complex, multi-panel plots in ggplot2
  sf,           # Working with simple features (geospatial vector data)
  ggnewscale,   # Creating multiple gradient scales in a single ggplot
  gridExtra,    # Combining multiple plots
  stringi,      # String manipulation
  patchwork,
  janitor,
  mice,
  VIM, 
  expss)


#microdatasus: This package seems to be a custom package hosted on GitHub, likely designed for working with microdata from the Brazilian Unified Health System (SUS).
remotes::install_github("rfsaldanha/microdatasus")
library(microdatasus)

# Antes de baixar o read.dbc abaixo, tenha certeza de ter instalado no PC o rtools versão 4.4
remotes::install_github("danicat/read.dbc", force = TRUE)
library(base)
library(read.dbc)


# Define a base URL and years
base_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/"
years <- c("07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22")

# Create a function to download and read DBC files
download_and_read_dbc <- function(year) {
  dbc_url <- paste0(base_url,"LEPTBR",year,".dbc")
  file_name <- paste0("LEPTBR",year,".dbc")
  
  # Attempt to download and read the file, with error handling
  tryCatch(
    {
      download.file(dbc_url, file_name, mode = "wb")
      data <- read.dbc(file_name)
      data$Year <- paste0("20", year)
      return(data)
    },
    error = function(e) {
      cat("Error for year", year, ":", e$message, "\n")
      return(NULL)
    }
  )
}


# Use lapply to download and read all DBC files and combine them into a dataframe
lept_data <- do.call(rbind, lapply(years, download_and_read_dbc))

clean_names(lept_data)

colnames(lept_data)


#renomeia as variáveis: OS EM LETRA MAIÚSCULA EU NÃO ACHEI NO DICDADOS 
new_names <- c ("tiponot",
                "IDagravo",
                "datanot",
                "semnot",
                "anonot",
                "UFnot",
                "muninot",
                "IDregi",
                "IDunidade",
                "dtsint",
                "sempri",
                "nasci",
                "idade",
                "sexo",
                "gestante",
                "raça",
                "escolaridade",
                "UFresid",
                "muniresid",
                "regisaude",
                "paisresid",
                "notdupli", #campo interno
                "dtdigit",#campo interno
                "flxret", #campo interno
                "recebflxret",  #campo interno
                "migrado",  #campo interno
                "dtinvest",
                "ocupacao",
                "antlama",
                "antanimais",
                "antcaixaagua",
                "antfossa",
                "antlocroedor",
                "antlavoura",
                "antrio",
                "antroedor",
                "antgrao",
                "antterbald",
                "antlixo",
                "antout",
                "antespecif",
                "anthum",
                "antanimal",
                "dataatend",
                "sintfebre",
                "sintmialg",
                "sintcefaleia",
                "sintprost",
                "sintcongest",
                "sintpanturr",
                "sintvomit",
                "sintdiarre",
                "sinticteri",
                "sintrenal",
                "sintresp",
                "sintcardio",
                "sinthemorrpulm",
                "sinthemorr",
                "sintmening",
                "sintout",
                "sintespecif",
                "hosp",
                "dthosp",
                "dtalta",
                "UFhosp",
                "munihosp",
                "dtelisa1",
                "reselisa1",
                "dtelisa2",
                "reselisa2",
                "dtmicro1",
                "micro1sor1",
                "micro1tit1",
                "micro1sor2",
                "micro1tit2",
                "resmicro1",
                "dtmicro2",
                "micro2sor1",
                "micro2tit1",
                "micro2sor2",
                "micro2tit2",
                "resmicro2",
                "dtisola",
                "resisola",
                "dtimuno",
                "resimuno",
                "dtpcr",
                "respcr",
                "classfin",
                "critclass",
                "autoc",
                "UFinf",
                "paisinf",
                "muniinf",
                "caractareainf",
                "caractambinf",
                "doencatrab",
                "evolução",
                "dtobito",
                "dtencerramento",
                "dtrisco1",
                "dtrisco2",
                "dtrisco3",
                "dtrisco4",
                "munirisco1",
                "munirisco2",
                "munirisco3",
                "munirisco4",
                "UFrisco1",
                "UFrisco2",
                "UFrisco3",
                "UFrisco4",
                "ano")


# aplica o value "new_names" ao dataset usando a função colnames
lept_data2 <- lept_data
colnames(lept_data2) <- new_names


# cria uma lista de labels correspondente à cada variável
lept_data2 <- apply_labels(lept_data2,
                           "tiponot" = "tipo de notificação",
                           "IDagravo" = "CID agravo",
                           "datanot" = "data da notificação",
                           "semnot" = "semana epidemiológica da notificação",
                           "anonot" = "ano da notificação",
                           "UFnot" = "UF da notificação",
                           "muninot" = "município da notificação",
                           "IDregi" = "ID da região",
                           "IDunidade" = "unidade de saúde",
                           "dtsint" = "data do primeiro sintoma",
                           "sempri" = "semana epidemiológica do primeiro sintoma",
                           "nasci" = "ano de nascimento",
                           "idade" = "idade",
                           "sexo" = "sexo",
                           "gestante" = "gestante",
                           "raça" = "raça/cor",
                           "escolaridade" = "escolaridade",
                           "UFresid" = "UF de residência",
                           "muniresid" = "município de residência",
                           "regisaude" = "Região de saúde do município",
                           "paisresid" = "país de residência",
                           "notdupli" = "indica duplicidade",
                           "dtdigit" = "Data de digitação",
                           "flxret" = "Fluxo de retorno",
                           "recebflxret" = "Recebida por fluxo de retorno",
                           "migrado" = "identifica migração",
                           "dtinvest" = "Data da Investigação",
                           "ocupacao" = ". Ocupação/Ramo de Atividade Econômica",
                           "antlama" = "Situação de risco: Água ou lama de enchente",
                           "antanimais" = "Situação de risco: criação de animais",
                           "antcaixaagua" = "Situação de risco: caixa dágua",
                           "antfossa" = "Situação de risco: fossa, caixa de gordura ou esgoto",
                           "antlocroedor" = "Situação de risco: locais com sinais de roedores",
                           "antlavoura" = "Situação de risco: plantio/colheita",
                           "antrio" = "Situação de risco: rio, córrego, lagoa ou represa",
                           "antroedor" = "Situação de risco: roedores diretamente",
                           "antgrao" = "Situação de risco: armazenamento de grãos/alimento",
                           "antterbald" = "Situação de risco: terreno baldio",
                           "antlixo" = "Situação de risco: lixo/entulho",
                           "antout" = "Situação de risco: outra",
                           "antespecif" = "especificação da outra situação de risco",
                           "anthum" = "Casos humanos anteriores no local",
                           "antanimal" = "Casos animais anteriores no local",
                           "dataatend" = "data de atendimento",
                           "sintfebre" = "Febre",
                           "sintmialg" = "Mialgia",
                           "sintcefaleia" = "Cefaléia",
                           "sintprost" = "Prostração",
                           "sintcongest" = "Congestão conjuntival",
                           "sintpanturr" = "Dor na panturrilha",
                           "sintvomit" = "Vômito",
                           "sintdiarre" = "Diarréia",
                           "sinticteri" = "Icterícia",
                           "sintrenal" = "Insuficiência renal",
                           "sintresp" = "Alterações respiratórias",
                           "sintcardio" = "Alterações cardíacas",
                           "sinthemorrpulm" = "Hemorragia Pulmonar",
                           "sinthemorr" = "Outras Hemorragias",
                           "sintmening" = "Meningismo",
                           "sintout" = "Outros sintomas",
                           "sintespecif" = "Especificação de outros sintomas",
                           "hosp" = "Ocorreu Hospitalização",
                           "dthosp" = "Data da Internação",
                           "dtalta" = "Data de Alta",
                           "UFhosp" = "UF do hospital",
                           "munihosp" = "Município do Hospital",
                           "dtelisa1" = "Data da Coleta: Sorologia IgM Elisa 1ª amostra",
                           "reselisa1" = "Resultado Elisa 1ª amostra",
                           "dtelisa2" = "Data da Coleta: Sorologia IgM Elisa 2ª amostra",
                           "reselisa2" = "Resultado Elisa 1ª amostra",
                           "dtmicro1" = "Data da Coleta: micro 1ª amostra",
                           "micro1sor1" = "Micro 1ª amostra 1º Sorovar",
                           "micro1tit1" = "Micro 1ª Amostra 1º título",
                           "micro1sor2" = "Micro 1ª amostra 2º Sorovar",
                           "micro1tit2" = "Micro 1ª Amostra 2º título",
                           "resmicro1" = "Resultado micro aglutinação 1ª amostra",
                           "dtmicro2" = "Data da Coleta: micro 2ª amostra",
                           "micro2sor1" = "Micro 2ª amostra 1º Sorovar",
                           "micro2tit1" = "Micro 2ª Amostra 1º título",
                           "micro2sor2" = "Micro 2ª amostra 2º Sorovar",
                           "micro2tit2" = "Micro 2ª Amostra 2º título",
                           "resmicro2" = "Resultado micro aglutinação 2ª amostra",
                           "dtisola" = "Data da Coleta: Isolamento",
                           "resisola" = "Resultado isolamento",
                           "dtimuno" = "Data de Coleta: Imunohistoquímica",
                           "resimuno" = "Resultado Imunohistoquímica",
                           "dtpcr" = "Data de Coleta: RT-PCR",
                           "respcr" = "Resultado RT-PCR",
                           "classfin" = "Classificação Final",
                           "critclass" = "critério de classificação",
                           "autoc" = "Caso autóctone de residência",
                           "UFinf" = "UF (provável da fonte de infecção)",
                           "paisinf" = "País (provável da fonte de infecção)",
                           "muniinf" = "Município (provável da fonte de infecção)",
                           "caractareainf" = "Característica da área de infecção",
                           "caractambinf" = "Característica do ambiente de infecção",
                           "doencatrab" = "Doença relacionada ao trabalho",
                           "evolução" = "Evolução do caso",
                           "dtobito" = "Data do Óbito",
                           "dtencerramento" = "Data do Encerramento",
                           "dtrisco1" = "dtrisco1",
                           "dtrisco2" = "dtrisco2",
                           "dtrisco3" = "dtrisco3",
                           "dtrisco4" = "dtrisco4",
                           "munirisco1" = "munirisco1",
                           "munirisco2" = "munirisco2",
                           "munirisco3" = "munirisco3",
                           "munirisco4" = "munirisco4",
                           "UFrisco1" = "UFrisco1",
                           "UFrisco2" = "UFrisco2",
                           "UFrisco3" = "UFrisco3",
                           "UFrisco4" = "UFrisco4",
                           "ano" = "ano")

#quantos dados faltantes existem em cada variável
sapply(lept_data, function(x) sum(is.na(x)))

#visualização dos dados faltantes
visdat::vis_miss(lept_data,
                 warn_large_data = FALSE)
md.pattern(lept_data)


