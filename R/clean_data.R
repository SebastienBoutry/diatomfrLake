search_code_pe <- function(vec,code_pe,plan_eau=plan_eau){
  code_output <- ifelse(!is.na(code_pe),code_pe,{
    code <- unique(plan_eau$code_lac[str_detect(
      str_replace_all(
        str_replace_all(
          str_to_upper(
            plan_eau$nom_lac),"[:punct:]"," "),
        "[:space:]{2,}"," ") ,
      str_replace_all(str_to_upper(str_trim(str_replace_all(vec,"[:punct:]"," "))),"[:space:]{2,}"," "))])
    # print(code)
    ifelse(length(code)>0,code,NA_character_)
    # return(code)
  })
  return(unlist(code_output))
}


clean_data <- function(table){
  table %>%
    clean_names() %>%
    as_tibble() %>%
    mutate(id_uo = 1:length(logiciel)) %>%
    mutate(date = ifelse(str_detect(date, "\\/"), as.character(lubridate::dmy(date)), as.character(lubridate::ymd(date)))) %>%
    mutate(date = lubridate::ymd(date)) %>%
    mutate(annee = lubridate::year(date)) %>%
    mutate(coord_hors_uo_macro = tolower(coord_hors_uo_macro)) %>%
    mutate(coord_hors_uo_macro = case_when(
      coord_hors_uo_macro %in% "oui" ~ TRUE,
      coord_hors_uo_macro %in% "non" ~ FALSE,
      TRUE ~ NA
    )) %>%
    mutate(coord_hors_uo_macro = as.logical(coord_hors_uo_macro)) %>%
    # mutate_at(vars(num_omnidia_dur, num_omnidia_veg), ~ net_num_ominidia(., annee)) %>%
    # mutate(nom_photo_net=net_nom_photo(nom_photo,date,nom_pe,num_uo)) %>%
    # mutate(nom_photo_bis=search_photos_directory(nom_photo,date,nom_pe,num_uo)) %>%
    mutate(substrat_dur=str_replace_all(substrat_dur,"Pierres\\,[:space:]galets[:space:]{1,}\\[25\\-250[:space:]mm\\]","Pierres, galets")) %>%
    mutate(substrat_veg=ifelse(nom_latin == "Phragmites australis","Hélophytes",substrat_veg)) %>%
    mutate(transparence_secchi=as.logical(fct_recode(factor(toupper(transparence_secchi)),"FALSE" = "NON","TRUE" = "OUI"))) %>%
    mutate(code_pe=map2(nom_pe,code_pe,search_code_pe)) %>% # cherche les codes plan d'eau irstea
    mutate(code_pe=unlist(code_pe)) %>%
    mutate_at(vars(num_omnidia_dur,num_omnidia_veg),~str_replace_all(.,"J8605305","00000054"))
}
# data <- import_formulaire(file="Lunax_INRAE.xls",sheet=3,repertoire = "img/")
