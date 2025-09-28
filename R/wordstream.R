#' Cria uma visualização WordStream a partir de dados pré-formatados
#'
#' @param data Uma lista de listas pré-formatada, geralmente criada pela
#'   função `prepare_wordstream_data`. A estrutura deve ser:
#'   `list(list(date="Week 1", words=list(NOUN=list(...), ADJ=list(...))), ...)`
#' @param width Largura do widget em pixels.
#' @param height Altura do widget em pixels.
#' @param min_font_size Tamanho mínimo da fonte.
#' @param max_font_size Tamanho máximo da fonte.
#' @param top_words_vis Número de palavras a serem renderizadas pela biblioteca JS.
#'   Note que a filtragem principal é feita em `prepare_wordstream_data`.
#'
#' @import htmlwidgets
#'
#' @export
wordstream <- function(data, width = NULL, height = NULL,
                       min_font_size = 14, max_font_size = 30, top_words_vis = 60) {

  # Valida\u00e7\u00e3o b\u00e1sica da entrada
  if (!is.list(data) || (!"date" %in% names(data[[1]])) || (!"words" %in% names(data[[1]]))) {
    stop("A estrutura dos dados de entrada n\u00e3o parece correta. Use prepare_wordstream_data() para format\u00e1-los.", call. = FALSE)
  }

  # Configura\u00e7\u00f5es para a visualiza\u00e7\u00e3o JS
  settings <- list(
    minFontSize = min_font_size,
    maxFontSize = max_font_size,
    topWords = top_words_vis
  )

  # Agrupa os dados e as configurações
  x <- list(
    data = data,
    settings = settings
  )

  # Cria o widget
  htmlwidgets::createWidget(
    name = 'wordstreamr',
    x,
    width = width,
    height = height,
    package = 'wordstreamr'
  )
}

#' Shiny binding for wordstream
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit
#' (like \code{'100\%'}, \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @export
wordstreamOutput <- function(outputId, width = '100%', height = '500px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'wordstream', width, height, package = 'wordstreamr')
}

#' Render a wordstream in Shiny
#'
#' @param expr An expression that generates a wordstream
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @export
renderWordstream <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # Unquote expr
  htmlwidgets::shinyRenderWidget(expr, wordstreamOutput, env, quoted = TRUE)
}
