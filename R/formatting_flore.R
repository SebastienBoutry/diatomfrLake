#' Mise en forme de la liste floristique dasn un forme de tableur
#'
#' @param flore tableau flore sortie de read_diatom ou de la fonction read_prn
#'
#' @return suppresion de lignes identiques ; ajout de la colonne id identifiant
#'
#'
#' @export
#'
#' @examples
#' library(diatomfrlake)
#'
formatting_flore <- function(flore){
  flore <- flore %>%
    dplyr::rename("id_florefloristique"="V1",
                  "taxons"="V2",
                  "ab"="V3") %>%
    dplyr::mutate_at(dplyr::vars(id_florefloristique,taxons,fichier),as.character) %>%
    dplyr::mutate_at(dplyr::vars(ab),as.numeric)# %>%
    # dplyr::distinct()
  ####################################################
  vecsit<-c(which(flore$id_florefloristique!=""))
  vecsitIdFloristique<-c(which(flore$id_florefloristique!=""),nrow(flore))
  nbrrep<-sapply(1:(length(vecsitIdFloristique)-1),function(x)length(vecsitIdFloristique[x]:vecsitIdFloristique[x+1])-1)
  id<-rep(flore$id_florefloristique[vecsit],times=nbrrep)
  id<-c(id,id[length(id)])
  flore$id<-id
  ##
  flore <- flore %>%
    dplyr::group_by(id,taxons,fichier) %>%
    dplyr::summarise(ab=sum(ab,na.rm=TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(id,taxons,ab,fichier) %>%
    mutate_all(as.character)%>%
    mutate(ab=as.numeric(ab))
##
  return(flore)
}
