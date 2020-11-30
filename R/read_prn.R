#' Importation d'un fichier prn -format Omnidia-
#'
#' @param file nom de fichier Omnidia au format -prn-
#' @param repertoire chemin du répertoire par défaut le répertoire de travail
#' @param encoding code de l'encoding par défaut format windows CP1252
#'
#' @return
#' @export
#'
#' @examples
#' library(magrittr)
#' library(diatomfrlake)
#' path <- system.file("", package = "diatomfrlake")
#' read_prn(file ="3723.prn",repertoire=path)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
read_prn <- function(file, repertoire = getwd(),encoding="CP1252") {
  if (! stringr::str_detect(file,".prn$") ) {
    stop("le fichier n'est pas un objet prn")
  }
  if (str_detect(dir(repertoire),pattern=file) %>% sum()!=1) {
    stop(paste("le fichier n'est pas dans le repertoire suivant :",repertoire))
  }
  if (sum(is.na(scan(paste0(repertoire, file),
                     what = "char",
                     quote = "",
                     sep = "\t",
                     strip.white = T
  ) %>%
  matrix(ncol = 3, byrow = TRUE))) > 0)  {
    flore <- NULL
    stop("Erreur: Format", file, "non conforme\n")
  }
  flore <- scan(paste0(repertoire, file),
                what = "char",
                quote = "",
                sep = "\t",
                strip.white = T,
                fileEncoding = encoding
  ) %>%
    matrix(ncol = 3, byrow = TRUE) %>%
    as.data.frame()  %>%
    dplyr::mutate(fichier=file)
  ##
  return(flore)
}
