#' Importation du fichier flore (prn ou txt)
#'
#' @param filename nom du fichier soit .prn ou .txt
#' @param repertoire répertoire de données
#' @param encoding encoding en défaut CP1252
#'
#' @return tableau flore
#' @export
#'
#' @examples
#' library(diatomfrlake)
#'
read_diatom <- function(filename, repertoire=getwd(),encoding="CP1252"){
  if (! stringr::str_detect(filename,".prn$|.txt$") ) {
    stop("le fichier n'est pas un objet prn ou txt")
  }
  if(stringr::str_detect(filename,".prn$")){
    flore <- read_prn(filename, repertoire)
    #############################################################
  }else{
    # if(utils::read.delim2(paste0(repertoire,filename),h=FALSE,stringsAsFactors = FALSE,fileEncoding="CP1252") %>% ncol()>3){
    #   stop("Erreur: Format ", filename, " non conforme\n")
    # }
    flore<-utils::read.delim2(paste0(repertoire,filename),
                              h=FALSE,
                              stringsAsFactors = FALSE,
                              fileEncoding=encoding)  %>%
      dplyr::mutate(fichier=filename)
  }
  ##
  return(flore %>% mutate_all(as.character))
}
