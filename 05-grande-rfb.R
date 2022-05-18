library(tidyverse)

path <- "dados_rfb_small.csv"

dados <- readr::read_csv(path)
# vamos fingir que essa é a base completa
# que eu não consigo abrir

# dados |>
#   write_csv("dados_rfb_small.csv")

# o resultado que eu gostaria de prozudir é
# algo como

dados |>
  count(situacao_cadastral, uf) |>
  arrange(desc(n))

# ideia: quebrar em pedaços

primeiro_pedaco <- read_csv(path, n_max = 1000) |>
  count(situacao_cadastral, uf)

segundo_pedaco <- read_csv(path, n_max = 2000, skip = 1000) |>
  count(situacao_cadastral, uf)

primeiro_pedaco |>
  bind_rows(
    segundo_pedaco
  ) |>
  group_by(situacao_cadastral, uf) |>
  summarise(
    n = sum(n)
  )

# tem no próprio pacote readr algumas funcionalidades

sumarizar <- function(dados){

  dados |>
    count(situacao_cadastral, uf)

}

sumarizar_2args <- function(dados, index){

  dados |>
    count(situacao_cadastral, uf) |>
    mutate(numero_do_chunk = index)

}

filtro_am <- function(dados, index){
  dados |>
    filter(
      uf == "AM"
    )
}

callback <- DataFrameCallback$new(sumarizar)
callback2 <- DataFrameCallback$new(sumarizar_2args)
# precisa ser esse aqui!

callback_filtro <- DataFrameCallback$new(filtro_am)

dados_chunked <- read_csv_chunked(
  # ler em "chunks" ou "pedaços"
  "dados_rfb_small.csv",
  # ler desse arquivo
  callback_filtro,
  # o que eu faço com cada pedaço? é um callback
  chunk_size = 1000
  # qual é o tamanho do pedaço
)

dados_chunked |>
  group_by(situacao_cadastral, uf) |>
  summarise(
    n = sum(n)
  ) |>
  ungroup() |>
  arrange(desc(n))

# final da analise ^

## fazer usando o pacote vroom

#
tamanho_do_chunk <- 1000

n_rows <- length(vroom::vroom_lines(path))-1

numero_de_chunks <- ceiling(n_rows/tamanho_do_chunk)

vetor_de_skips <- tamanho_do_chunk*seq(0, numero_de_chunks-1)

vetor_de_nomes <- names(vroom::vroom(
  path,
  n_max =1
))

ler_e_sumarizar <- function(skip, path, chunk_size){

  message(skip)

  dados <- vroom::vroom(
    file = path,
    delim = ",",
    skip = skip,
    col_names = vetor_de_nomes,
    col_type = rep("c", length(vetor_de_nomes)),
    n_max = chunk_size,
    progress = FALSE
  )

  dados |>
    count(situacao_cadastral, uf)
}

purrr::map_dfr(
  vetor_de_skips,
  ler_e_sumarizar,
  path = path,
  chunk_size = tamanho_do_chunk
)

####
