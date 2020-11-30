#' Formatage de la date du numéro d'omnidia à 9 étoiles aaaa-mm-dd
#'
  #' @param numomnidia numéro d'omnidia à 9 étoiles -format chaine de caractère-
#'
#' @return numéro d'omnidia à 9 étoiles avec la date dans le format suivant aaaa-mm-dd -format chaine de charactère-
#' @export
#'
#' @examples
#' library(diatomfrlake)
#' formattingdate_num("037")
#' formattingdate_num("-167725522*27/07/2016********")
#'
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd dmy
#' @importFrom stringr str_split_fixed str_count str_locate str_c
#' @seealso \code{\link{clean_numomnidia}}
formattingdate_num <- function(numomnidia){
  numomnidia_output <- NULL
  table_vec <- stringr::str_split_fixed(numomnidia, "\\*", n = 10)
  ## formatting the date
  if(! nchar(table_vec[, 2]) %in% c(8,10) | !stringr::str_count(table_vec[, 2],"[:digit:]")==8){
    numomnidia_output <- numomnidia
    warning(paste("la date du numéro omnidia (9 étoiles) '",table_vec[, 2],"' n'est pas au bon format."))
  }else{
    loc_annee <- stringr::str_locate(table_vec[, 2], as.character("20(1|2)."))
    if(loc_annee[,"start"]==1){
      table_vec[, 2] <- lubridate::ymd(table_vec[, 2])
    }else {
      table_vec[, 2] <- as.character(lubridate::dmy(table_vec[, 2]))
      }
  }
  ##
  numomnidia_output <- stringr::str_c(table_vec[, 1], table_vec[, 2], table_vec[, 3],
                                       table_vec[, 4], table_vec[, 5], table_vec[, 6],
                                       table_vec[, 7], table_vec[, 8], table_vec[, 9], table_vec[, 10],
                                       sep = "*"
  )
  numomnidia_output <- ifelse(numomnidia_output %in% "*********", NA_character_, numomnidia_output)
  return(numomnidia_output)
}
