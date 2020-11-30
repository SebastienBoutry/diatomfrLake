
#' Nettoyage du nom des photos
#'
#' @param nom_photo
#' @param date
#' @param nom_pe
#' @param num_uo
#'
#' @return
#' @export
#'
#' @examples
clean_filename_photo <- function(nom_photo,date,nom_pe,num_uo) {
  # print(vec)
  vec <- stringr::str_replace_all(vec, "\\/", "-")
  vec <- stringr::str_replace_all(vec, "\n|\\,", ";")
  vec <- stringr::str_replace_all(vec, "photos fiches phytobenthos 2016\\-", "")
  vec <- stringr::str_replace_all(vec, "\\..{3}", "")
  vec <- stringr::str_replace_all(vec, "\\»", "")
  vec <- stringr::str_replace_all(vec, "\\«", "")
  vec <- stringr::str_replace_all(vec, "[:blank:]{2,}", ";")
  vec <- stringr::str_replace_all(vec, "^;", "")
  vec <- stringr::str_replace_all(vec,"[:digit:][:blank:][:digit:]",";")
  vec <- stringr::str_replace_all(vec,"(?<!(\\_|\\-))(UO|uo)","_UO")
  # vec <- str_replace_all(vec,"\\_uo([:digit:]+)\\_","_UO\\1_")
  vec <- stringr::str_trim(vec)
  if (sum(stringr::str_detect(vec, "1et2$|1\\_2$|1\\;2\\;3$"),na.rm = TRUE)>1) {
    sit <- stringr::str_which(vec, "1et2$|1\\_2$|1\\;2\\;3$")
    # vec[sit]
    nom_photo <- sapply(
      sit,
      function(i) stringr::str_split(vec[i], "1et2|1\\_2|1\\;2\\;3")[[1]][1]
    )
    vec[sit] <- sapply(
      1:length(sit),
      function(i) paste0(nom_photo[i], 1:2, sep = "", collapse = " ; ")
    )
  } else {

  }
  if(sum(stringr::str_detect(stringr::str_to_upper(vec),"PHOTO[:space:]1"),na.rm = TRUE)>=1){
    sit <- stringr::str_which(stringr::str_to_upper(vec), "PHOTO[:space:]1")
    vec[sit]<-paste(as.character(date[sit]),stringr::str_to_title(nom[sit]),num[sit],"Photo1",sep="_")
    vec[sit]<-stringr::str_replace_all(vec[sit],"[:space:]Lac[:space:]D(e[:space:]|[:space:])","")
    vec[sit]<-stringr::str_replace_all(vec[sit],"[:space:]Du[:space:]","")
    vec[sit]<-stringr::str_replace_all(vec[sit],"Laffrey[:space:]Grand","Laffrey")
  }else{

  }
  vec<-stringr::str_replace_all(vec,
                                "Grand([:blank:]{0,1}|\\-{0,1})Large",
                                "Grand([:blank:]{0,1}|\\-{0,1})Large")
  vec<-stringr::str_replace_all(vec,
                                "[:space:]\\-[:space:]",
                                " ; ")
  vec<-stringr::str_replace_all(vec,
                                "aulnes",
                                "Aulnes")
  # print(vec)
  # vec<-str_replace_all(vec,"^([:digit:]{1,})\\-([:alpha:]{1,})\\-(UO){0,1}0{0,1}([:digit:]+)\\-(.*)","\\1\\_\\2\\_(UO){0,1}0{0,1}\\4\\_\\5")
  data_photos <- stringr::str_split_fixed(vec,pattern=";",n=4) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate_all(stringr::str_trim) %>%
    dplyr::mutate_all(dplyr::na_if,"")
  #########################
  vec<-stringr::str_replace_all(data_photos %>% dplyr::select(V1) %>% dplyr::pull(),
                                "^([:digit:]{2,4})(\\-{0,1})([:digit:]{0,2})(\\-{0,1})([:digit:]{0,4})(\\-|\\_)([:lower:]|[:upper:])(.{1,})(\\-|\\_)(UO|uo){0,1}0{0,1}([:digit:]+)(\\-|\\_)(Photo|photo|PHOTO){0,1}(.*)",
                                "paste0('\\1\\2\\3\\4\\5\\_',paste0('(',str_to_lower('\\7'),'|',str_to_upper('\\7'),')','(','\\8','|',str_to_lower('\\8'),'|',str_to_upper('\\8'),')'),'\\_(UO|uo){0,1}0{0,1}\\11\\_(Photo|photo|PHOTO){0,1}\\14')")
  vec<-sapply(vec,function(i)eval(parse(text=i)))
  return(vec)
}

