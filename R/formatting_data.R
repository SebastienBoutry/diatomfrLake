#' Mise en forme la feuille "Données générales" du formualire de saisie
#'
#' @param output_import objet de sortie de la fonction \code{\link{import_formualire}}
#'
#' @return table rempli par les données mésologiques -objet data.frame-
#' @export
#'
#' @examples
#' @importFrom dplyr mutate_all
formatting_data <- function(output_import) {
  ##
  ods <- unique(output_import$ods)
  sheet3 <-  output_import
  ##
  vec_noms<-c("logiciel", "version", "departement", "nom_pe", "organisme", "operateur", "coord_hors_uo_macro", "substrat_dur", "prof_max_dur", "num_omnidia_dur", "substrat_veg", "prof_max_veg", "num_omnidia_veg", "temperature", "o2_dissous", "cond", "impact_humains", "dist_rive", "disque_secchi", "coord_x", "num_uo", "date", "type_dominant", "colmatage", "nbr_tiges", "ph", "transparence_secchi", "code_pe", "sat_o2", "coord_y", "nom_photo", "commentaires", "nom_latin")
  departement <- nom_PE <- Societe <- Operateur <- NouvelleUO <- SubstratDur <- ProfMaxDur <- NumInvSubstratDur <- SubstratVeg <- ProfMaxVeg <- NumInvSubstratVeg <- Temp <- O2dissous <- Cond <- ImpactHumains <- DistRive <- SD <- NA
  CoordX <- NumUO <- Date <- TypeUO <- Colmatage <- NbrTige <- PH <- SDdeterminable <- CodePE <- SatO2 <- CoordY <- photo <- NA
  Commentaires <- NomLatin <- DateVersion <- NA
  if (sum(!is.na(sheet3)) %in% 0) {
    DonneesBrutes <- data.frame(cbind(ods, DateVersion, departement, nom_PE, Societe, Operateur, NouvelleUO, SubstratDur, ProfMaxDur, NumInvSubstratDur, SubstratVeg, ProfMaxVeg, NumInvSubstratVeg, Temp, O2dissous, Cond, ImpactHumains, DistRive, SD, CoordX, NumUO, Date, TypeUO, Colmatage, NbrTige, PH, SDdeterminable, CodePE, SatO2, CoordY, photo, Commentaires, NomLatin),
                                stringsAsFactors=FALSE) %>%
      dplyr::mutate_all(as.character)
    names(DonneesBrutes)<-vec_noms
    return(DonneesBrutes)
  } else {
    #########################################################
    ## colonnne 2
    if (ods %in% TRUE) {
      ### version du fichier origine ods
      DateVersion <- sheet3[1, 2]
      departement <- sheet3[5, 2]
      nom_PE <- sheet3[6, 2]
      Societe <- sheet3[7, 2] # commun
      Operateur <- sheet3[8, 2] # commun
      NouvelleUO <- sheet3[9, 2] #
      SubstratDur <- sheet3[14, 2] # commun
      # ProfMaxDur<-sheet3[15,2]
      # NumInvSubstratDur<-sheet3[16,2] # numero d'omnidia
      # SubstratVeg<-sheet3[17,2]
      # ProfMaxVeg<-sheet3[18,2]
      # NumInvSubstratVeg<-sheet3[19,2]
      # Temp<-sheet3[21,2]
      # O2dissous<-sheet3[22,2]
      # Cond<-sheet3[23,2]
      # ImpactHumains<-sheet3[26,2]
      # DistRive<-sheet3[27,2]
      # SD<-sheet3[28,2]
      #########################################################
      ## colonnne 3
      CoordX <- sheet3[9, 5]
      if (is.na(CoordX) | CoordX %in% "") {
        CoordX <- sheet3[9, 6]
      }
      ## colonnne 4
      NumUO <- sheet3[6, 4]
      Date <- sheet3[7, 4]
      TypeUO <- sheet3[8, 4]
      if (TypeUO %in% "Numéro du type de rive dominant :") {
        TypeUO <- sheet3[8, 5]
      }
      Colmatage <- sheet3[14, 4] ## commun
      # NbrTige<-sheet3[17,4] # spécifique
      # if(NbrTige%in%"Nombres de tiges :"){NbrTige<-sheet3[17,6]}# spécifique
      # NomLatin<-sheet3[18,4] # pas pris en compte
      # PH<-sheet3[22,4]  # spécifique
      # SDdeterminable<-sheet3[27,4]  # spécifique
      ## colonne 5
      CodePE <- sheet3[5, 5]
      # SatO2<-sheet3[21,5] # spécifique
      ## colonne 6
      CoordY <- sheet3[10, 3]
      # photo<-sheet3[36,6]


      if (DateVersion != "sept.-15") {
        # DateVersion<-"09-2015"
        # departement<-sheet3[5,2]
        # nom_PE<-sheet3[6,2]
        # Societe<-sheet3[7,2] #commun
        # Operateur<-sheet3[8,2] # commun
        # NouvelleUO<-sheet3[9,2] #
        # SubstratDur<-sheet3[14,2] #commun
        ProfMaxDur <- sheet3[15, 2]
        NumInvSubstratDur <- sheet3[16, 2] # numero d'omnidia
        SubstratVeg <- sheet3[17, 2]
        ProfMaxVeg <- sheet3[18, 2]
        NumInvSubstratVeg <- sheet3[19, 2]
        Temp <- sheet3[21, 2]
        O2dissous <- sheet3[22, 2]
        Cond <- sheet3[23, 2]
        ImpactHumains <- sheet3[26, 2]
        DistRive <- sheet3[27, 2]
        SD <- sheet3[28, 2]
        #########################################################
        ## colonnne 3
        # CoordX<-sheet3[9,5]
        # if(CoordX%in%""){	CoordX<-sheet3[9,6]}
        # ## colonnne 4
        # NumUO<-sheet3[6,4]
        # Date<-sheet3[7,4]
        # TypeUO<-sheet3[8,4]
        # if(TypeUO%in%"Num?ro du type de rive dominant :"){TypeUO<-sheet3[8,5]}
        # Colmatage<-sheet3[14,4] ## commun
        NbrTige <- sheet3[17, 4] # spécifique
        if (NbrTige %in% "Nombres de tiges :") {
          NbrTige <- sheet3[17, 6]
        } # spécifique
        NomLatin <- sheet3[18, 4] # pas pris en compte
        PH <- sheet3[22, 4] # spécifique
        SDdeterminable <- sheet3[27, 4] # spécifique
        ## colonne 5
        # CodePE<-sheet3[5,5]
        SatO2 <- sheet3[21, 5] # spécifique
        ## colonne 6
        # CoordY<-sheet3[10,3]
        photo <- sheet3[36, 6]
        # 		Commentaires<-
        # tempsfin<-Sys.time()
        # tempsfin-tempsdep
      } else {
        DateVersion <- "09-2015"
        NumInvSubstratDur <- NA
        if (ncol(sheet3) %in% c(7,8)) {
          NumInvSubstratDur <- sheet3[14, 7]
        } # numero d'omnidia
        NumInvSubstratVeg <- NA
        if (ncol(sheet3) %in% c(7,8)) {
          NumInvSubstratVeg <- sheet3[15, 6]
        } # numero d'omnidia
        # NumInvSubstratVeg<-sheet3[19,2]
        SubstratVeg <- sheet3[15, 2]
        ProfMaxDur <- sheet3[16, 2]
        ProfMaxVeg <- sheet3[16, 2]
        NomLatin <- NA
        Temp <- sheet3[18, 2]
        O2dissous <- sheet3[19, 2]
        Cond <- sheet3[20, 2]
        ImpactHumains <- sheet3[23, 2]
        DistRive <- sheet3[24, 2]
        SD <- sheet3[25, 2]
        NbrTige <- sheet3[15, 4]
        SatO2 <- sheet3[18, 5]
        PH <- sheet3[19, 4]
        SDdeterminable <- sheet3[24, 4]
        photo <- sheet3[33, 2]
        Commentaires <- sheet3[28, 4]
      }
    } else {
      # 		deux versions excel
      # 	print(sheet3[1,9])
      # 	sheet3[,9]<-sheet3[,9]+25569
      if (sheet3[1, 9] != "16770") {
        DateVersion <- as.Date(as.numeric(sheet3[1, 9]), format = "%m-%d-%Y", origin = "01-01-1900")
        DateVersion <- format(DateVersion, format = "%m-%Y")
      } else {
        ## format
        sheet3[1, 9] <- as.numeric(sheet3[1, 9]) + 25569
        DateVersion <- as.Date(sheet3[1, 9], format = "%m-%d-%Y", origin = "01-01-1900")
        DateVersion <- format(DateVersion, format = "%m-%Y")
      }
      # 		print(DateVersion)
      if (DateVersion %in% "09-2015") {
        ## fichier excel
        departement <- sheet3[5, 4]
        nom_PE <- sheet3[6, 3]
        Societe <- sheet3[7, 3]
        Operateur <- sheet3[8, 3]
        NouvelleUO <- sheet3[9, 4]
        SubstratDur <- sheet3[14, 4] # pas sur
        ProfMaxDur <- "" # pas de case dans cette version sheet3[15,4]
        NumInvSubstratDur <- sheet3[14, 16]
        SubstratVeg <- sheet3[15, 4]
        ProfMaxVeg <- sheet3[16, 4]
        NumInvSubstratVeg <- sheet3[15, 16]
        Temp <- sheet3[18, 3]
        O2dissous <- sheet3[19, 4]
        Cond <- sheet3[20, 3]
        ImpactHumains <- sheet3[23, 4]
        DistRive <- sheet3[24, 4]
        SD <- sheet3[25, 4]
        #########################################################
        ## colonnne 3
        CoordX <- sheet3[9, 8]
        ## colonnne 4
        NumUO <- sheet3[6, 9]
        Date <- as.character(as.Date(as.numeric(sheet3[7, 8]), format = "%m-%d-%Y", origin = "12-30-1899"))
        # 					sheet3[7,8]
        TypeUO <- sheet3[8, 9]
        Colmatage <- sheet3[14, 8]
        NbrTige <- sheet3[15, 8]
        PH <- sheet3[19, 8]
        SDdeterminable <- sheet3[24, 9]
        ## colonne 5
        CodePE <- sheet3[5, 8]
        SatO2 <- sheet3[21, 6]
        ## colonne 6
        CoordY <- sheet3[10, 8]
        photo <- sheet3[36, 7]
        Commentaires <- sheet3[28, 4]
        tempsfin <- Sys.time()
        tempsfin - tempsdep
      } else {
        departement <- sheet3[5, 4]
        nom_PE <- sheet3[6, 3]
        Societe <- sheet3[7, 3]
        Operateur <- sheet3[8, 3]
        NouvelleUO <- sheet3[9, 4]
        SubstratDur <- sheet3[14, 4]
        ProfMaxDur <- sheet3[15, 4]
        NumInvSubstratDur <- sheet3[16, 4]
        SubstratVeg <- sheet3[17, 4]
        ProfMaxVeg <- sheet3[18, 4]
        NumInvSubstratVeg <- sheet3[19, 4]
        Temp <- sheet3[21, 3]
        O2dissous <- sheet3[22, 3]
        Cond <- sheet3[23, 3]
        ######################################
        #####################################
        ##
        ImpactHumains <- sheet3[26, 4]
        DistRive <- sheet3[27, 4]
        SD <- sheet3[28, 4]
        #########################################################
        ## colonnne 3
        CoordX <- sheet3[9, 8]
        ## colonnne 4
        NumUO <- sheet3[6, 9]
        Date <- as.character(as.Date(as.numeric(sheet3[7, 8]), format = "%m-%d-%Y", origin = "12-30-1899"))
        # 			Date<-as.Date(as.numeric(sheet3[7,8]),format="%m-%d-%Y",origin="01-01-1900")
        TypeUO <- sheet3[8, 9]
        Colmatage <- sheet3[14, 8]
        NbrTige <- sheet3[17, 8]
        NomLatin <- sheet3[18, 8]
        PH <- sheet3[22, 8]
        SDdeterminable <- sheet3[27, 9]
        ## colonne 5
        CodePE <- sheet3[5, 8]
        SatO2 <- sheet3[21, 8]
        ## colonne 6
        CoordY <- sheet3[10, 8]
        photo <- sheet3[36, 6]
        Commentaires <- sheet3[30, 4]
      }
    }
    ################################################
    DonneesBrutes <-data.frame(cbind(ods, DateVersion, departement, nom_PE, Societe, Operateur, NouvelleUO, SubstratDur, ProfMaxDur, NumInvSubstratDur, SubstratVeg, ProfMaxVeg, NumInvSubstratVeg, Temp, O2dissous, Cond, ImpactHumains, DistRive, SD, CoordX, NumUO, Date, TypeUO, Colmatage, NbrTige, PH, SDdeterminable, CodePE, SatO2, CoordY, photo, Commentaires, NomLatin),
                               stringsAsFactors=FALSE)
  }
  names(DonneesBrutes)<-vec_noms
  return(DonneesBrutes)
}
