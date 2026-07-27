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
  c(7, 22),
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
                   Baú = 15)

    })

df_x2

## Gráfico ----

freq |>
  tidyr::pivot_longer(cols = 2:3,
                      names_to = "Conteúdo",
                      values_to = "Frequência") |>
  dplyr::rename("Baú" = 1) |>
  dplyr::mutate(Baú = Baú |>
                  forcats::fct_relevel(1:30 |> as.character())) |>
  ggplot(aes(Baú, Frequência)) +
  geom_col() +
  geom_text(data = df_x2,
            aes(Baú, Frequência, label = sts),
            size = 7,
            color = "black") +
  facet_wrap(~Conteúdo, ncol = 1, scales = "free_y") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        strip.text = element_text(color = "black", size = 20)) +
  ggview::canvas(height = 10, width = 12)
