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
  dplyr::arrange(bau)

freq

## Teste Qui-Quadrado ----

df_x2 <- purrr::map2_dfr(
  freq[2:3] |>
    names(),
  c(6, 20),
  \(conteudo, frequencia){

    qq <- chisq.test(freq[[conteudo]])

    tibble::tibble(Conteúdo = conteudo,
                   sts = paste0("X² = ",
                                qq$statistic |> round(2),
                                ", df = ",
                                qq$parameter,
                                ", p = ",
                                qq$p.value |> round(2)),
                   Frequência = frequencia,
                   Bau = 15)

    })

df_x2
