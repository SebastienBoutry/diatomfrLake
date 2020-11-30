#' Correction des fautes de frappes lors de la saisie du numéro d'omnidia / ou - au lieu *
#'
#' @param numomnidia numéro d'omnidia permet d'identifier la lame -format chaîne de caractère-
#'
#' @return numéro d'omnidia corrigé -format chaîne de caractère-
#' @export
#'
#' @examples
#' library(diatomfrlake)
#' formatting_numomnidia("170822-037")
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_count str_replace_all str_detect
formatting_numomnidia <- function(numomnidia) {
  numomnidia <- ifelse(stringr::str_detect(numomnidia,"^-"),stringr::str_replace(numomnidia,"^-",""),numomnidia)
  if(!is.na(numomnidia)){
    nstars <- stringr::str_count(numomnidia, "\\*")
    if(nstars == 0){
      numomnidia <- stringr::str_replace_all(numomnidia, "-", "*")
      numomnidia <- stringr::str_replace_all(numomnidia, "\\/", "*")
    }else{  }
  }else{}
  return(numomnidia)
}
