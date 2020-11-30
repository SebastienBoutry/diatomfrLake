#' Importation des données mésologiques à partir du fichier formulaire de saisie Phytobenthos Plan d'eau
#'
#' @param file nom du fichier à importer sous R format -chaine de charactère-
#' @param sheet feuille -chaine de charactère- 2 feuille A renseigner ou 3
#' @param repertoire dossier ou se trouve le ou les fichiers à importer
#'
#' @return
#' @export
#'
#' @examples
#' library(diatomfrlake)
#'
#' @importFrom magrittr %>%
#' @importFrom readODS get_num_sheets_in_ods read_ods
#' @importFrom stringr str_detect
#' @importFrom xlsx read.xlsx read.xlsx2
#'
import_formulaire <- function(file, sheet=NA, repertoire = getwd()) {
  if(!sheet %in% c(1,2,3)){
    stop("l'argument n'est pas égale à '1','2' ou '3'")
  }
  ##
  ods <- stringr::str_detect(file, ".ods$")
  ##
  if (ods) {
    ### feuilles à explorer : sheet 1 à 3
    # if (readODS::get_num_sheets_in_ods(paste0(repertoire, file)) == 1) {
    #   cat(error("Error: Format", file, "non conforme\n"))
    #   sheet_ouput <- NA
    # } else {
      sheet_ouput <- switch(as.character(sheet),
                            "1" = readODS::read_ods(paste0(repertoire, file), sheet = 1,
                                                    formula_as_formula = FALSE,col_names = FALSE),
                            "2" = readODS::read_ods(paste0(repertoire, file), sheet = 2,
                                                    formula_as_formula = FALSE,col_names = FALSE),
                            "3" = readODS::read_ods(paste0(repertoire, file), sheet = 3,
                                                    formula_as_formula = FALSE,col_names = FALSE)
      )
    # }
  } else {
    sheet_ouput <- switch(as.character(sheet),
                          "1" = xlsx::read.xlsx(
                            file = paste0(repertoire, file),
                            sheetIndex = 1,
                            # endRow = 9,
                            header = FALSE,
                            colClasses = "character",
                            stringsAsFactors = FALSE,
                            encoding = "UTF-8"
                          ),
                          "2" = xlsx::read.xlsx(
                            file = paste0(repertoire, file),
                            sheetIndex = 2,
                            # endRow = 9,
                            header = FALSE,
                            colClasses = "character",
                            stringsAsFactors = FALSE,
                            encoding = "UTF-8"
                          ),
                          "3" = xlsx::read.xlsx2(
                            file = paste0(repertoire, file),
                            sheetIndex = 3,
                            startRow = 1,
                            colIndex = 1:20,
                            # endRow = 40,
                            as.data.frame = TRUE,
                            colClasses = c(rep("character", 20)),
                            stringsAsFactors = FALSE,
                            header = FALSE,
                            encoding = "UTF-8"
                          )
    )
  }
  output <- sheet_ouput %>%
    dplyr::mutate(ods=ods)
  ###
  return(output)
}
