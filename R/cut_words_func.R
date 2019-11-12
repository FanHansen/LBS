#' Cut words
#'
#' \code{cut_words} is used for cutting sentences as words.
#' @param string A string.
#' @param lowercase Whether transform string to lowercase. Default is TRUE.
#' @param engine  Words segment engine of jiebar packages.
#' @param quick_seg Logical,quick segmentation of sentence.Default is TRUE.
#' @importFrom  jiebaR worker segment DICTPATH
#' @importFrom data.table fwrite
#' @importFrom stringr boundary str_to_lower
#' @importFrom utils data
#' @examples
#' data(dict_add_cn)
#' cut_words(paste(substr(dict_add_cn[[1]][1],1,2),substr(dict_add_cn[[1]][2],1,2),
#' substr(dict_add_cn[[1]][3],1,3)))
#' @export


cut_words <- function (string, lowercase = TRUE,engine = NULL,quick_seg = TRUE){
  string = gsub("[^\u4e00-\u9fa5,^A-Za-z,^0-9]", "",string)
   if(is.character(string)&& length(string) == 1) {
     if(quick_seg){
       out = stringr::str_split(string,stringr::boundary("word"))[[1]]
     }else{
       if(is.null(engine)){
         utils::data(dict_add_cn)
         dict_add_cn = eval(parse(text = "dict_add_cn"))
         dict_add_cn  = as.data.frame(dict_add_cn)
         engine =jiebaR:: worker()
        data.table::fwrite(dict_add_cn,
                            file = engine$PrivateVarible$dict,append = TRUE)
         engine =jiebaR:: worker()
       }
       out =  jiebaR::segment(string, jiebar = engine)

     }

  }
  if (lowercase){
    stringr::str_to_lower(out)
  }else {
    string
  }
}
