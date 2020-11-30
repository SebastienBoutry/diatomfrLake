#' Nettoyage du code Omnidia
#'
#' @param numomnidia numéro d'omnidia permet d'identifier la lame
#'
#' @return numéro d'omnidia à 9 étoiles -format chaîne de caractère-
#' @export
#'
#' @examples
#' library(diatomfrlake)
#' clean_numominidia("170822-037")
#'
#' @seealso \code{\link{formatting_numomnidia}},\code{\link{formattingdate_num}}
clean_numominidia <- function(numomnidia) {
  numominidia_output <- formatting_numomnidia(numomnidia)
  ## gestion du format date entre la première et la deuxième étoile
  numominidia_output <- formattingdate_num(numominidia_output)
  return(as.character(numominidia_output))
}

