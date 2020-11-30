#' Import data with PhytoBenthos OFB's sheet
#'
#' @param file filename of the phytobenthos OFB's sheet
#' @param repertoire The path of folder
#'
#' @return output
#' @export
#'
#' @examples
#' library(diatomfrlake)
#'
import_formatOFB <- function(file, repertoire = getwd(),sheet=1,select=""){
  if(!str_detect(select,"data|contact")){
    stop("l'argument select est différent de data ou contact")
  }
  ##
  sheet_ouput <- import_formulaire(file,sheet=as.character(sheet),repertoire)
  if(!str_detect(sheet_ouput[1,1],"Diatomées en plan d'eau [-|–] Données soutenant la biologie")){
    stop("le format ne correspond pas au format https://hydrobio-dce.inrae.fr/telecharger/phytobenthos-plans-deau/")
  }
  ods <- stringr::str_split(file,"[.]")[[1]][length(stringr::str_split(file,"[.]")[[1]])]
  if(select=="data"){
    departement <- nom_PE <- Societe <- Operateur <- NouvelleUO <- SubstratDur <- ProfMaxDur <- NumInvSubstratDur <- SubstratVeg <- ProfMaxVeg <- NumInvSubstratVeg <- Temp <- O2dissous <- Cond <- ImpactHumains <- DistRive <- SD <- NA
    CoordX <- NumUO <- Date <- TypeUO <- Colmatage <- NbrTige <- PH <- SDdeterminable <- CodePE <- SatO2 <- CoordY <- photo <- NA
    Commentaires <- NomLatin <- DateVersion <- NA
    DonneesBrutes <- NULL

    if(ods %in% "xlsx"){
      sheet_ouput <- sheet_ouput[-c(35,42,49),]
    }else{}
    ## remplissage des données
    DateVersion <- paste0(
      stringr::str_split(sheet_ouput[1,1],"[-|–]")[[1]][5],
      "-",
      stringr::str_split(sheet_ouput[1,1],"[-|–]")[[1]][6]) %>%
      stringr::str_trim()
    departement <- sheet_ouput[6,2]
    nom_PE <- sheet_ouput[7,2]
    Societe <- sheet_ouput[15,2]
    Operateur <- sheet_ouput[17,2]
    NouvelleUO <- sheet_ouput[26,2]
    SubstratDur <- sheet_ouput[32,2]
    ProfMaxDur <- sheet_ouput[34,2]
    NumInvSubstratDur <- sheet_ouput[31,2]
    SubstratVeg <- sheet_ouput[37,2]
    ProfMaxVeg <- sheet_ouput[40,2]
    NumInvSubstratVeg <- sheet_ouput[36,2]
    Temp <- sheet_ouput[42,2]
    O2dissous <- sheet_ouput[43,2]
    Cond <- sheet_ouput[44,2]
    ImpactHumains <- sheet_ouput[48,2]
    DistRive <- sheet_ouput[49,2]
    SD<- sheet_ouput[50,2]
    CoordX <- sheet_ouput[22,2]
    NumUO <- sheet_ouput[27,2]
    Date <-  lubridate::as_date(ifelse(ods!="ods",
                                       lubridate::as_date(as.numeric(sheet_ouput[11,2]), origin = "1899-12-30"),
                                       lubridate::dmy(sheet_ouput[11,2])
    )) %>% as.character()
    TypeUO <- str_extract(sheet_ouput[28,2],"[:digit:]")
    Colmatage <- sheet_ouput[33,2]
    NbrTige <- sheet_ouput[38,2]
    PH <- sheet_ouput[46,2]
    SDdeterminable <- sheet_ouput[51,2]
    CodePE <- NA
    SatO2 <- sheet_ouput[44,2]
    CoordY <- sheet_ouput[23,2]
    photo <- NA_character_ ## non renseigné
    Commentaires <- sheet_ouput[53,1]
    NomLatin <- sheet_ouput[39,2]
    ##
    output <-data.frame(
      cbind(ods, DateVersion, departement, nom_PE, Societe, Operateur, NouvelleUO, SubstratDur, ProfMaxDur, NumInvSubstratDur, SubstratVeg, ProfMaxVeg, NumInvSubstratVeg, Temp, O2dissous, Cond, ImpactHumains, DistRive, SD, CoordX, NumUO, Date, TypeUO, Colmatage, NbrTige, PH, SDdeterminable, CodePE, SatO2, CoordY, photo, Commentaires, NomLatin),
      stringsAsFactors=FALSE)
    vec_noms<-c("logiciel", "version", "departement", "nom_pe", "organisme", "operateur", "coord_hors_uo_macro", "substrat_dur", "prof_max_dur", "num_omnidia_dur", "substrat_veg", "prof_max_veg", "num_omnidia_veg", "temperature", "o2_dissous", "cond", "impact_humains", "dist_rive", "disque_secchi", "coord_x", "num_uo", "date", "type_dominant", "colmatage", "nbr_tiges", "ph", "transparence_secchi", "code_pe", "sat_o2", "coord_y", "nom_photo", "commentaires", "nom_latin")
    names(output)<-vec_noms
  }else{}
  if(select=="contact"){
    ##
    Agence <- email <- num_tel <- version_formulaire <-  Agence <- Societe <- Societe_sandre <- NA
    output <- NA
    Societe <- sheet_ouput[15,2]
    Societe_sandre  <- sheet_ouput[14,2]
    output <- as.data.frame(cbind(Societe_sandre, Agence, email, num_tel, Societe),
                                    stringsAsFactors = FALSE)
    names_contact_outputs <- c("sandre_intervenant", "agence_commanditaire", "email", "num_tel", "organisme")
    names(output)<-names_contact_outputs
  }else{}
  ##
  #########################################
  return(output)
}



# file="soutienbio_Mutsche_UO3.xlsx";repertoire="data/";sheet=1;select="data"
# xls <- readxl::read_xlsx("data/soutienbio_Mutsche_UO3.xlsx")
# tidyxl::xlsx_names("data/soutienbio_Mutsche_UO3.xlsx")
