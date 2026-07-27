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

# Teste Qui-Quadrado ----

## Calcular frequência ----

freq <- dados |>
  dplyr::count(bau, conteudo) |>
  tidyr::pivot_wider(names_from = conteudo,
                     values_from = n,
                     values_fill = 0) |>
  dplyr::arrange(bau) |>
  dplyr::pull(mimico)

freq
