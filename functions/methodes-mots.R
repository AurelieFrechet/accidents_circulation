#' Title
#'
#' @param phrase 
#' @param mot 
#'
#' @return
#' @export
#'
#' @examples
detecte_mot <- function(phrase, mot) {
  stringr::str_detect(pattern = paste0("\\b", mot, "\\b"), string  = phrase)
}




#' Title
#'
#' @param mot1 
#' @param mot2 
#'
#' @return
#' @export
#'
#' @examples
remplace_mot <- function(phrase, mot1, mot2){
  gsub(pattern = paste0("\\b", mot1, "\\b"), 
       replacement = mot2, 
       x = phrase)
}

#' Title
#'
#' @param phrase 
#'
#' @return
#' @export
#'
#' @examples
supprime_accents <- function(phrase){
  iconv(x = phrase, from = "UTF-8", to = "ASCII//TRANSLIT")
}

#' Title
#'
#' @param phrase 
#'
#' @return
#' @export
#'
#' @examples
supprime_espaces <- function(phrase) {
  stringr::str_trim(stringr::str_squish(phrase))
}

#' Title
#'
#' @param phrase 
#' @param mot 
#'
#' @return
#' @export
#'
#' @examples
supprime_mot <- function(phrase, mot) {
  mots <- paste(paste0("\\b", mot, "\\b"), collapse = "|")
  supprime_espaces(gsub(
    pattern = mots,
    replacement = "",
    x = phrase
  ))
}


phrase <- 'du test duplication du de la lachaise'


detecte_mot(phrase, "p")
remplace_mot(phrase, "test", "chapeau")
supprime_mot(phrase, c("la", "du"))


