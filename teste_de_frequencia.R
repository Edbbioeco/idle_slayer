# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

# Dados ----

## Importar ----

dados <- readxl::read_xlsx("./dados.xlsx")

## Visualizar ----

dados

dados |> dplyr::glimpse()

## Tratar ----

dados %<>%
  dplyr::mutate(dplyr::across(.cols = c(1, 3),
                              .fns = ~. |> as.character()))

dados

dados |> dplyr::glimpse()
