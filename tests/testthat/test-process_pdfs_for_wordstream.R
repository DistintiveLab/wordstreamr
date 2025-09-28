# tests/testthat/test-process_pdfs_for_wordstream.R

library(testthat)
library(mockery)
library(dplyr)
library(tibble)

test_that("process_pdfs_for_wordstream funciona corretamente e lida com LLM", {

  # --- 1. SETUP: Criar um ambiente de teste seguro e temporário ---

  # Cria uma pasta temporária que será automaticamente limpa após o teste
  temp_pdf_dir <- withr::local_tempdir(pattern = "wordstream_test")

  pdf1_content <- paste(
    rep("análise", 6),
    rep("dados", 5),
    collapse = " \n"
  )
  pdf2_content <- paste(
    rep("documento", 4),
    rep("visualização", 3),
    rep("projeto", 2),
    rep("excluir", 1),
    collapse = " \n"
  )
  # Criar um PDF de teste com metadados de data
  pdf_path1 <- file.path(temp_pdf_dir, "doc1.pdf")
  pdf(pdf_path1, title = "Documento 1")
  plot.new()
  text(0.5, 0.5, "Este é o primeiro documento para análise. Análise de dados é importante.")
  text(0.5,0.2,pdf1_content)
  dev.off()
  # A data de criação nos metadados é definida automaticamente

  # Criar um segundo PDF sem metadados de data para testar o fallback
  pdf_path2 <- file.path(temp_pdf_dir, "doc2.pdf")
  pdf(pdf_path2) # Sem metadados
  plot.new()
  text(0.5, 0.5, "Um segundo teste com novos termos. Termos técnicos.")
  text(0.5,0.2,pdf2_content)
  dev.off()
  # Definir manualmente a data de modificação para ser previsível
  Sys.setFileTime(pdf_path2, "2023-01-01 12:00:00")


  # --- 2. TESTE BÁSICO: Extração e contagem sem LLM ---

  result_no_llm <- process_pdfs_for_wordstream(
    folder_path = temp_pdf_dir,
    language = "pt",
    top_n_words = 5
  )

  # Verificar se a saída é um tibble e tem as colunas certas
  expect_s3_class(result_no_llm, "tbl_df")
  expect_equal(names(result_no_llm), c("term", "frequency"))

  # Verificar se a contagem e o stemming funcionaram como esperado
  # "análise" vira "analis", aparece 2x
  expect_equal(result_no_llm$term[1], "anális")
  expect_equal(result_no_llm$frequency[1], 8)

  # Verificar se o top_n_words foi respeitado
  expect_equal(nrow(result_no_llm), 5)


  # --- 3. TESTE AVANÇADO: Extração com MOCK do LLM ---

  # Criar uma resposta de LLM falsa e previsível
  mock_llm_response <- tibble(
    response = paste(
      "anális: Técnico",
      "dad: Técnico",
      "document: Geral",
      "term: Geral",
      # Deixamos "tecnic" de fora para testar o fallback "Indeterminado"
      sep = "\n"
    )
  )

  # Criar uma função de mock que substitui a chamada real
  # Ela ignora o prompt e sempre retorna nossa resposta falsa
  mock_generate <- function(model, prompt) {
    return(mock_llm_response)
  }

  # Usar mockery::stub para substituir a chamada `ollamar::generate`
  # dentro da nossa função `process_pdfs_for_wordstream` pela nossa mock_generate
  stub(process_pdfs_for_wordstream, 'ollamar::generate', mock_generate)
  stub(process_pdfs_for_wordstream, 'ollamar::test_connection', TRUE)
  # Executar a função com a categorização LLM ativada
  # Agora, quando ela tentar chamar ollamar::generate, nossa mock_generate será chamada
  result_with_llm <- process_pdfs_for_wordstream(
    folder_path = temp_pdf_dir,
    language = "pt",
    top_n_words = 5,
    categories = "auto", # Ativa a chamada ao LLM
    top_n_categories = 2 # Este valor será ignorado pela nossa mock
  )

  # Verificar se a saída tem a nova coluna 'category'
  expect_s3_class(result_with_llm, "tbl_df")
  expect_equal(names(result_with_llm), c("term", "frequency", "category"))

  # Verificar se as categorias da nossa resposta mock foram atribuídas corretamente
  result_filtered <- result_with_llm %>% filter(term %in% c("anális", "document"))
  expect_equal(result_filtered$category, c("Técnico", "Geral"))

  # Verificar se o termo que não estava na resposta mock recebeu a categoria 'Indeterminado'
  term_nao_categorizado <- result_with_llm %>% filter(term == "dados anális")
  expect_equal(term_nao_categorizado$category, "Indeterminado")

})
