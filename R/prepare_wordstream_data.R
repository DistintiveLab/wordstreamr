#' Prepara os dados para a visualização WordStream
#'
#' Esta função recebe um data frame em formato "tidy" (um registro por palavra),
#' calcula as frequências e o transforma na estrutura de lista aninhada
#' requerida pela função `wordstream()`.
#'
#' @param data Um data frame.
#' @param time_col O nome da coluna (string) que marca o tempo (ex: "Week", "Year").
#' @param group_col O nome da coluna (string) que define a categoria/grupo (ex: "POS", "Topic").
#' @param word_col O nome da coluna (string) que contém as palavras ou termos.
#' @param top_n O número de palavras mais frequentes a serem mantidas para cada grupo em cada período.
#'
#' @return Uma lista de listas no formato adequado para a função `wordstream`.
#'
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import purrr
#'
#' @export
prepare_wordstream_data <- function(data, time_col, group_col, word_col, top_n = 60) {

  # 1. Contar as frequências de cada palavra, por tempo e por grupo
  word_frequencies <- data %>%
    count(!!sym(time_col), !!sym(group_col), !!sym(word_col), name = "frequency")

  # 2. Filtrar para manter apenas as top_n palavras por grupo/tempo
  top_words <- word_frequencies %>%
    group_by(!!sym(time_col), !!sym(group_col)) %>%
    slice_max(order_by = .data$frequency, n = top_n) %>%
    ungroup()

  # 3. Renomear colunas para o padrão esperado (date, topic, text)
  formatted_data <- top_words %>%
    rename(
      date = !!sym(time_col),
      topic = !!sym(group_col),
      text = !!sym(word_col)
    )

  # 4. Garantir a ordem original da coluna de tempo
  time_levels <- unique(data[[time_col]])
  formatted_data$date <- factor(formatted_data$date, levels = time_levels)

  # 5. Transformar para a estrutura de lista aninhada final
  data_for_js <- formatted_data %>%
    group_by(date) %>%
    nest(words_data = c(.data$topic, .data$text, .data$frequency)) %>%
    mutate(words = map(.data$words_data, function(df) {
      df %>%
        group_by(.data$topic) %>%
        nest(words_df = c(.data$text, .data$frequency)) %>%
        mutate(words_list = map(.data$words_df, purrr::transpose)) %>%
        select(.data$topic, .data$words_list) %>%
        tibble::deframe()
    })) %>%
    select(.data$date, .data$words) %>%
    mutate(date = as.character(.data$date)) %>%
    purrr::transpose() %>%
    map(~.x)

  return(data_for_js)
}
