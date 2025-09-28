# tests/testthat/test-prepare_data.R

library(testthat)
library(wordstreamr) # Assumindo que o pacote é carregado
library(tibble)

test_that("prepare_wordstream_data funciona corretamente", {

  # 1. Criar um data frame de teste simples e previsível
  test_df <- tibble(
    Dia = c("Dia 1", "Dia 1", "Dia 1", "Dia 1", "Dia 2", "Dia 2"),
    Tipo = c("A", "A", "B", "B", "A", "A"),
    Palavra = c("R", "R", "azul", "verde", "R", "R")
  )

  # 2. Executar a função
  result <- prepare_wordstream_data(
    data = test_df,
    time_col = "Dia",
    group_col = "Tipo",
    word_col = "Palavra",
    top_n = 1 # Manter apenas a palavra mais frequente de cada grupo
  )

  # 3. Verificar a estrutura geral
  expect_type(result, "list")
  expect_length(result, 2) # Dois dias
  expect_equal(names(result[[1]]), c("date", "words"))

  # 4. Verificar o conteúdo do primeiro período de tempo ("Dia 1")
  dia1_data <- result[[1]]
  expect_equal(dia1_data$date, "Dia 1")
  expect_true("A" %in% names(dia1_data$words))
  expect_true("B" %in% names(dia1_data$words))

  # No "Dia 1", a palavra mais comum do Tipo "A" é "R" com frequência 2
  expect_equal(dia1_data$words$A[[1]]$text, "R")
  expect_equal(dia1_data$words$A[[1]]$frequency, 2)

  # No "Dia 1", para o Tipo "B", há um empate. slice_max mantém ambos.
  # Vamos testar se "azul" está lá.
  expect_true("azul" %in% sapply(dia1_data$words$B, `[[`, "text"))

  # 5. Verificar o conteúdo do segundo período de tempo ("Dia 2")
  dia2_data <- result[[2]]
  expect_equal(dia2_data$date, "Dia 2")
  # No "Dia 2", o top_n=1 deve retornar apenas "R" para o Tipo "A"
  expect_length(dia2_data$words$A, 1)
  expect_equal(dia2_data$words$A[[1]]$text, "R")
  expect_equal(dia2_data$words$A[[1]]$frequency, 2)

})
