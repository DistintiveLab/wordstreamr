# tests/testthat/test-wordstream.R

library(testthat)
library(wordstreamr)
library(tibble)

test_that("wordstream() cria um widget com dados pré-formatados", {

  # 1. Criar dados de teste
  test_df <- tibble(
    Semana = c("Semana 1", "Semana 1"),
    Categoria = c("NOUN", "NOUN"),
    Termo = c("dados", "dados")
  )

  # 2. Preparar os dados usando a função auxiliar
  prepared_data <- prepare_wordstream_data(
    data = test_df,
    time_col = "Semana",
    group_col = "Categoria",
    word_col = "Termo"
  )

  # 3. Chamar a função principal com os dados preparados
  widget <- wordstream(data = prepared_data)

  # 4. Verificar se a saída é um objeto htmlwidget
  expect_s3_class(widget, "htmlwidget")

  # 5. Verificar se os dados dentro do widget correspondem aos dados preparados
  expect_equal(widget$x$data, prepared_data)

  # 6. Verificar se as configurações padrão estão sendo passadas
  expect_equal(widget$x$settings$minFontSize, 14)
  expect_equal(widget$x$settings$topWords, 60)

  # 7. Testar se a função para ao receber dados mal formatados
  bad_data <- list(list(a=1, b=2))
  expect_error(wordstream(data = bad_data))

})
