#' Processa PDFs, extrai termos e os categoriza opcionalmente usando um LLM
#'
#' Esta função varre uma pasta em busca de arquivos PDF, extrai texto e datas,
#' processa o texto para encontrar termos relevantes (palavras e n-grams),
#' e opcionalmente usa um modelo de linguagem grande (LLM) para categorizá-los.
#'
#' @param folder_path Caminho para a pasta contendo os arquivos PDF.
#' @param language Idioma dos documentos para seleção da blacklist e stemming. Padrão "pt".
#'   Valores possíveis: "pt", "en", "es".
#' @param ngram_range Um vetor numérico indicando os tamanhos de n-gram a serem extraídos.
#'   Padrão `1:2` (palavras únicas e bigramas).
#' @param blacklist Uma lista de palavras a serem removidas. Se NULL (padrão), usa uma
#'   blacklist interna para o idioma selecionado.
#' @param top_n_words Número máximo de termos a serem retornados, baseado na frequência. Padrão 100.
#' @param categories Define o método de categorização. Padrão "auto". Pode ser:
#'   - `"auto"`: Se `top_n_categories` for definido, o LLM criará as categorias livremente.
#'   - `"pos"`: O LLM atuará como um tagger de Part-of-Speech.
#'   - `"ner"`: O LLM atuará como uma ferramenta de Named-Entity Recognition.
#'   - Um vetor de strings (ex: `c("Tecnologia", "Finanças")`): Força o LLM a usar
#'     essas categorias temáticas pré-definidas.
#' @param top_n_categories Número de categorias a serem geradas pelo LLM. Relevante apenas
#'   quando `categories = "auto"`. Se NULL (padrão), a categorização por LLM é desativada.
#' @param llm_provider O provedor de LLM a ser usado. Padrão "ollamar".
#'   Alternativas: "chatgpt", "claude", etc. (depende do pacote `chatLLM`).
#'
#' @return Um data frame (tibble) "tidy" com as colunas `date`, `term`, `frequency` e
#'   `category` (se a categorização LLM for ativada). Perfeito para usar com
#'   `prepare_wordstream_data`.
#'
#' @import pdftools
#' @import tidytext
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import SnowballC
#'
#' @export
process_pdfs_for_wordstream <- function(folder_path,
                                        language = "pt",
                                        ngram_range = 1:2,
                                        blacklist = NULL,
                                        top_n_words = 100,
                                        categories = "auto",
                                        top_n_categories = NULL,
                                        llm_provider = "ollamar") {

  # --- Validações e Dependências ---
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("O pacote 'pdftools' \u00e9 necess\u00e1rio. Por favor, instale-o com install.packages('pdftools').")
  }
  if (!requireNamespace("tidytext", quietly = TRUE)) {
    stop("O pacote 'tidytext' \u00e9 necess\u00e1rio. Por favor, instale-o com install.packages('tidytext').")
  }
  if (!requireNamespace("SnowballC", quietly = TRUE)) { # NOVA VERIFICA\u00c7\u00c3O
    stop("O pacote 'SnowballC' \u00e9 necess\u00e1rio. Por favor, instale-o com install.packages('SnowballC').")
  }

  # --- 1. Extração de Texto e Datas dos PDFs ---

  pdf_files <- list.files(folder_path, pattern = "\\\\.pdf$", full.names = TRUE, ignore.case = TRUE)
  if (length(pdf_files) == 0) stop("Nenhum arquivo PDF encontrado na pasta especificada.")

  doc_data <- tibble(file_path = pdf_files) %>%
    mutate(
      text = map_chr(.data$file_path, ~paste(pdftools::pdf_text(.x), collapse = " ")),
      info = map(.data$file_path, pdftools::pdf_info),
      meta_created = map(.data$info, "created"),
      file_info = map(.data$file_path, file.info),
      file_mtime = map(.data$file_info, "mtime"),
      date = coalesce(as.Date(.data$meta_created), as.Date(.data$file_mtime))
    ) %>%
    select(.data$date, .data$text) %>%
    filter(!is.na(.data$date))

  # --- 2. Blacklist Padrão ---

  if (is.null(blacklist)) {
    stopwords_pt <- c("de", "a", "o", "que", "e", "do", "da", "em", "um", "para", "\u00e9", "com", "n\u00e3o", "uma", "os", "no", "se", "na", "por", "mais", "as", "dos", "como", "mas", "foi", "ao", "ele", "das", "tem", "\u00e0", "seu", "sua", "ou", "ser", "quando", "muito", "h\u00e1", "nos", "j\u00e1", "est\u00e1", "eu", "tamb\u00e9m", "s\u00f3", "pelo", "pela", "at\u00e9", "isso", "ela", "entre", "era", "depois", "sem", "mesmo", "aos", "ter", "seus", "quem", "nas", "me", "esse", "eles", "est\u00e3o", "voc\u00ea", "tinha", "foram", "essa", "num", "nem", "suas", "meu", "\u00e0s", "minha", "t\u00eam", "numa", "pelos", "elas", "havia", "seja", "qual", "ser\u00e1", "n\u00f3s", "tenho", "lhe", "deles", "essas", "esses", "pelas", "este", "fosse", "dele", "tu", "te", "voc\u00eas", "vos", "lhes", "meus", "minhas", "teu", "tua", "teus", "tuas", "nosso", "nossa", "nossos", "nossas", "dela", "delas", "esta", "estes", "estas", "aquele", "aquela", "aqueles", "aquelas", "isto", "aquilo", "estou", "est\u00e1", "estamos", "est\u00e3o", "estive", "esteve", "estivemos", "estiveram", "estava", "est\u00e1vamos", "estavam", "estivera", "estiv\u00e9ramos", "esteja", "estejamos", "estejam", "estivesse", "estiv\u00e9ssemos", "estivessem", "estiver", "estivermos", "estiverem", "hei", "h\u00e1", "havemos", "h\u00e3o", "houve", "houvemos", "houveram", "houvera", "houv\u00e9ramos", "haja", "hajamos", "hajam", "houvesse", "houv\u00e9ssemos", "houvessem", "houver", "houvermos", "houverem", "houverei", "houver\u00e1", "houveremos", "houver\u00e3o", "houveria", "houver\u00edamos", "houveriam", "sou", "somos", "s\u00e3o", "era", "\u00e9ramos", "eram", "fui", "foi", "fomos", "foram", "fora", "f\u00f4ramos", "seja", "sejamos", "sejam", "fosse", "f\u00f4ssemos", "fossem", "for", "formos", "forem", "serei", "ser\u00e1", "seremos", "ser\u00e3o", "seria", "ser\u00edamos", "seriam", "tenho", "tem", "temos", "t\u00e9m", "tinha", "t\u00ednhamos", "tinham", "tive", "teve", "tivemos", "tiveram", "tivera", "tiv\u00e9ramos", "tenha", "tenhamos", "tenham", "tivesse", "tiv\u00e9ssemos", "tivessem", "tiver", "tivermos", "tiverem", "terei", "ter\u00e1", "teremos", "ter\u00e3o", "teria", "ter\u00edamos", "teriam")
    if (language %in% c("en", "es")) {
      if (!requireNamespace("tm", quietly = TRUE)) {
        stop("O pacote 'tm' \u00e9 necess\u00e1rio para obter a lista de stopwords padr\u00e3o em ingl\u00eas/espanhol.\\n",
             "Por favor, instale-o com `install.packages('tm')` ou forne\u00e7a uma blacklist personalizada.", call. = FALSE)
      }
      stopwords_en <- tm::stopwords("en")
      stopwords_es <- tm::stopwords("es")
    } else {
      stopwords_en <- character(0) # Vetor vazio se n\u00e3o for usado
      stopwords_es <- character(0)
    }


    blacklist <- switch(language,
                        "pt" = stopwords_pt,
                        "en" = stopwords_en,
                        "es" = stopwords_es,
                        stop("Idioma n\u00e3o suportado. Escolha 'pt', 'en' ou 'es'.")
    )
  }

  # --- 3. Tokenização, Stemming e Limpeza ---
  # Mapeamento do idioma para o formato do SnowballC
  stem_language <- switch(language,
                          "pt" = "portuguese",
                          "en" = "english",
                          "es" = "spanish",
                          stop("Idioma n\u00e3o suportado para stemming.")
  )

  terms <- doc_data %>%
    unnest_tokens(.data$term, .data$text, token = "ngrams", n_min = min(ngram_range), n = max(ngram_range)) %>%
    filter(!str_detect(.data$term, paste0("\\\\b(", paste(blacklist, collapse="|"), ")\\\\b"))) %>%
    # Stemming (radicaliza\u00e7\u00e3o) com SnowballC
    mutate(term = SnowballC::wordStem(.data$term, language = stem_language)) # LINHA ALTERADA

  # --- 4. Contagem e Filtragem ---
  # (Esta seção permanece inalterada)
  top_terms <- terms %>%
    count(.data$term, sort = TRUE) %>%
    slice_max(order_by = n, n = top_n_words) %>%
    rename(frequency = n)

  # --- 5. Categorização com LLM (Condicional) ---
  # (Esta seção permanece inalterada)
  if (!is.null(top_n_categories) || (is.character(categories) && length(categories) > 1) || categories %in% c("pos", "ner")) {
    if (!requireNamespace("ollamar", quietly = TRUE) && !requireNamespace("chatLLM", quietly = TRUE)) {
      stop("Para categoriza\u00e7\u00e3o, instale o pacote 'ollamar' ou 'chatLLM'.")
    }

    term_list <- paste(top_terms$term, collapse = "\\n")

    prompt <- case_when(
      categories == "pos" ~ paste0(
        "Voc\u00ea \u00e9 um especialista em lingu\u00edstica (Part-of-Speech Tagger). ",
        "Categorize cada termo da lista abaixo em uma das seguintes categorias gramaticais: ",
        "NOUN (substantivo), VERB (verbo), ADJECTIVE (adjetivo), ADVERB (adv\u00e9rbio), ou OTHER (outro). ",
        "Responda APENAS com o termo, seguido de dois pontos e a categoria. Exemplo: 'casa: NOUN'.\\n\\n",
        "TERMOS:\\n", term_list
      ),

      categories == "ner" ~ paste0(
        "Voc\u00ea \u00e9 uma ferramenta de Reconhecimento de Entidades Nomeadas (NER). ",
        "Categorize cada termo da lista abaixo como: PERSON (pessoa), ORGANIZATION (organiza\u00e7\u00e3o), ",
        "LOCATION (local), ou OTHER (outro). ",
        "Responda APENAS com o termo, seguido de dois pontos e a categoria. Exemplo: 'Microsoft: ORGANIZATION'.\\n\\n",
        "TERMOS:\\n", term_list
      ),

      is.character(categories) && length(categories) > 1 ~ paste0(
        "Categorize cada termo da lista abaixo em uma das seguintes categorias: ",
        paste(categories, collapse = ", "),
        ". Se um termo n\u00e3o se encaixar em nenhuma delas, use a categoria 'Outros'. ",
        "Responda APENAS com o termo, seguido de dois pontos e a categoria. Exemplo: 'termo: Categoria'.\\n\\n",
        "TERMOS:\\n", term_list
      ),

      TRUE ~ paste0(
        "Analise a lista de termos abaixo e agrupe-os em ", top_n_categories, " categorias distintas ",
        "baseadas em seus temas ou focos principais. Nomeie as categorias de forma concisa. ",
        "Responda APENAS com o termo, seguido de dois pontos e a categoria que voc\u00ea criou. Exemplo: 'termo: Categoria'.\\n\\n",
        "TERMOS:\\n", term_list
      )
    )

    message("Enviando termos para o LLM para categoriza\u00e7\u00e3o. Isso pode levar um tempo...")
    llm_response <- tryCatch({
      if (llm_provider == "ollamar") {
        if (!ollamar::test_connection(logical=TRUE)) stop("Ollama n\u00e3o est\u00e1 rodando. Inicie o servi\u00e7o Ollama para continuar.")
        res_df <- ollamar::generate("llama3", prompt)
        res_df$response
      } else {
        chatLLM::call_llm(prompt, provider = llm_provider)
      }
    }, error = function(e) {
      warning("Falha na chamada ao LLM: ", e$message, "\\nRetornando dados n\u00e3o categorizados.")
      return(NULL)
    })

    if (!is.null(llm_response)) {
      category_lookup <- llm_response %>%
        str_split("\\n", simplify = TRUE) %>%
        as.character() %>%
        str_trim() %>%
        as_tibble_col(column_name = "raw") %>%
        filter(raw != "" & str_detect(raw, ":")) %>%
        separate(raw, into = c("term", "category"), sep = ":\\\\s*", fill = "right") %>%
        mutate(category = ifelse(is.na(.data$category), "Indeterminado", str_trim(.data$category)))

      top_terms <- top_terms %>%
        left_join(category_lookup, by = "term") %>%
        mutate(category = coalesce(.data$category, "Indeterminado"))
    } else {
      top_terms$category <- "N\u00e3o categorizado"
    }
  }

  return(top_terms)
}
