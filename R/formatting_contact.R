#' Mise en forme la feuille "A renseigner" du formualire de saisie
#'
#' @param ods le fichier d'origine est un fichier avec une extension ods booléen TRUE (or FALSE)
#' @param sheet2 sortie import_formulaire de la feuille "2"
#'
#' @return
#' @export
#'
#' @examples
formatting_contact <- function(output_import) {
  ##
  ods <- unique(output_import$ods)
  sheet2 <-  output_import
  ##
  names_contact_outputs <- c("sandre_intervenant", "agence_commanditaire", "email", "num_tel", "organisme")
  Agence <- email <- num_tel <- version_formulaire <- Societe <- Societe_sandre <- NA
  contact_output <- NA

  if (sum(!is.na(sheet2)) == 0) {
    contact_output <- as.data.frame(cbind(Societe_sandre, Agence, email, num_tel, Societe),
                             stringsAsFactors = FALSE)
    names(contact_output) <- names_contact_outputs
    return(contact_output)
  } else {
    ##########################
    if (ods == TRUE) {
      if (ncol(sheet2) > 0) {
        ## colonne 2
        if (ncol(sheet2) > 1) {
          Agence <- sheet2[6, 2]
          email <- sheet2[7, 2]
          num_tel <- sheet2[8, 2]
          version_formulaire <- sheet2[9, 2]
        }
        if (ncol(sheet2) > 2) {
          ## colonne 3
          Societe <- sheet2[5, 3]
        }
        if (ncol(sheet2) > 3) {
          ## colonne 4
          Societe_sandre <- sheet2[6, 4]
        }
        ###################################################
      }
    } else {
      if (ncol(sheet2) > 0) {
        ## colonne 2
        Agence <- sheet2[6, 4]
        email <- sheet2[7, 3]
        num_tel <- sheet2[8, 3]
        version_formulaire <- sheet2[9, 4]

        ## colonne 3
        Societe <- sheet2[5, 3]
        ## colonne 4
        Societe_sandre <- sheet2[6, 8]
        ###################################################
      }
    }
    ###############################################
    contact_output <- as.data.frame(cbind(Societe_sandre, Agence, email, num_tel, Societe),
                             stringsAsFactors = FALSE)
    contact_output[contact_output == ""] <- NA
  }
  names(contact_output)<-names_contact_outputs
  ## outuput
  return(contact_output)
}
