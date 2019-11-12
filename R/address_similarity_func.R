#' Address similarity
#'
#' \code{address_similarity} is used for computing jaccard and cos similarity between addresses.
#' \code{multi_address_similarity} is used for computing jaccard and cos similarity between multiple addresses.
#' @param dat data.frame contains address variables.
#' @param x_list Names of addresses.
#' @param add_list A list of addresses.
#' @param ex_cols A list of excluded variables. Regular expressions can also be used to match
#' variable names. Default is NULL.
#' @param ID The name of ID of observations. Default is NULL.
#' @param method  .jaccard similarity or cos similarity,Default is "cos".
#' @param engine  Words segment engine of jiebar packages.
#' @param quick_seg Logical,quick segmentation of sentence.Default is TRUE.
#' @return A data.frame with similarities of all observations.
#' @importFrom creditmodel loop_function stop_parallel_computing start_parallel_computing
#' @importFrom stats aggregate
#' @importFrom  cli cat_line
#' @importFrom  jiebaR worker segment
#' @importFrom utils data
#' @examples
#' data(dict_add_cn)
#' x = paste(substr(dict_add_cn[[1]][1],1,2),substr(dict_add_cn[[1]][2],1,2),
#' substr(dict_add_cn[[1]][3],1,3),substr(dict_add_cn[[1]][4],1,3))
#' y= paste(substr(dict_add_cn[[1]][1],1,2),substr(dict_add_cn[[1]][2],1,2),
#' substr(dict_add_cn[[1]][3],1,3),substr(dict_add_cn[[1]][5],1,3))
#' z = paste(substr(dict_add_cn[[1]][1],1,2),substr(dict_add_cn[[1]][2],1,2),
#' substr(dict_add_cn[[1]][3],1,3),substr(dict_add_cn[[1]][7],1,3))
#' multi_address_similarity(data.frame(x=x,y=y,z = z), method = 'jaccard')
#' @export



multi_address_similarity <- function(dat,x_list = NULL,ID = NULL,ex_cols = NULL,
                                    method = 'cos',engine = NULL,quick_seg = TRUE){
  opt = options(scipen=100, stringsAsFactors = FALSE)
  dat = checking_data(dat)
  if (is.null(ID)) {
    dat$ID = rownames(dat)
    ID = 'ID'
  }
  if(!is.null(x_list)){
    add_list = dat[x_list]
  }else{
    x_list = get_names(dat = dat,
                       types = c('character', 'factor'),
                       ex_cols = c(ID, ex_cols), get_ex = FALSE)
    add_list = dat[x_list]
  }

  words_list = list()

  for (i in 1: nrow(add_list)) {
    words_list[i] = list(unlist(add_list[i,]))
    names(words_list[[i]]) <- sub("_lng",'',names(words_list[[i]]))
  }
  if(!quick_seg && (is.null(engine)||length(engine)==0)){
    utils::data("dict_add_cn")
    dict_add_cn = eval(parse(text = "dict_add_cn"))
    dict_add_cn  = as.data.frame(dict_add_cn)
    engine =jiebaR:: worker()
    data.table::fwrite(dict_add_cn,
                       file = engine$PrivateVarible$dict,append = TRUE)
    engine =jiebaR:: worker()
  }
  sims = data.frame(t(sapply(1:length(words_list), function(i.)address_similarity(add_list = words_list[[i.]],
                                                                                    engine = engine,
                                                                                  quick_seg = quick_seg,
                                                                         method = method))))
  add_comp_sims <- cbind(dat[ID], sims)
  return(add_comp_sims)
  on.exit(options(opt))
}

#' @rdname multi_address_similarity
#' @export
address_similarity <- function(add_list,method = 'cos',engine = NULL,quick_seg = TRUE){
  add_list = unlist(add_list)
  add = data.frame(t(add_list),stringsAsFactors = FALSE )
  if(!quick_seg && (is.null(engine)||length(engine)==0)){
    utils::data("dict_add_cn")
    dict_add_cn = eval(parse(text = "dict_add_cn"))
    dict_add_cn  = as.data.frame(dict_add_cn)
    engine =jiebaR:: worker()
    data.table::fwrite(dict_add_cn,
                       file = engine$PrivateVarible$dict,append = TRUE)
    engine =jiebaR:: worker()
  }
  sims = list()
  colname <- list()
  if(method =='cos'){
    for (i in 1:ncol(add)) {
      if  ( i >= ncol(add)) break
      sims[[i]] = apply(add[ ,i:ncol(add)], 2,
                         function(x) words_cos_similarity(add[ ,i], x,engine = engine,quick_seg = quick_seg))
      colname[[i]] = lapply(names(add)[i:(ncol(add))],
                             function(n) paste(names(add)[i], n, sep = '_sim_'))
    }

  }else{
    for (i in 1:ncol(add)) {
      if  ( i >= ncol(add)) break
      sims[[i]] = apply(add[ ,i:ncol(add)], 2,
                         function(x) words_jaccard_similarity(add[ ,i],x,engine = engine, quick_seg = quick_seg))
      colname[[i]] = lapply(names(add)[i:(ncol(add))],
                             function(n) paste(names(add)[i], n, sep = '_sim_'))
    }
  }

  sims = unlist(sims)
  names(sims) = unlist(colname)
  splitvar = strsplit(names(sims),"_sim_")
  vars = c()
  for (i in 1:(length(sims))) {
    if (splitvar[[i]][1] == splitvar[[i]][2]) {
      vars[[i]] = names(sims)[i]
    } else {
      vars[[i]] = NA
    }
  }
  sims = sims[is.na(vars)]
  return(sims)
}

#' \code{words_jaccard_similarity} is for measuring jaccard similarity/dissimilarity in documents
#' \code{words_cos_similarity} is for measuring cos similarity/dissimilarity in documents
#' @param words1 Sentence 1.
#' @param words2 Sentence 2.
#' @param engine  Words segment engine of jiebar packages.
#' @param quick_seg Logical,quick segmentation of sentence.Default is TRUE.
#' @return A number of similarity
#' @importFrom data.table dcast setDT
#' @importFrom stats aggregate
#' @importFrom utils data
#' @examples
#' data(dict_add_cn)
#' words1 = paste(substr(dict_add_cn[[1]][1],1,2),substr(dict_add_cn[[1]][2],1,2),
#' substr(dict_add_cn[[1]][3],1,3),substr(dict_add_cn[[1]][4],1,3))
#' words2 = paste(substr(dict_add_cn[[1]][1],1,2),substr(dict_add_cn[[1]][2],1,2),
#' substr(dict_add_cn[[1]][3],1,3),substr(dict_add_cn[[1]][5],1,3))
#' words_jaccard_similarity(words1,words2)
#' words_cos_similarity(words1,words2)
#' @export
words_jaccard_similarity <- function(words1,words2,engine = NULL,quick_seg = TRUE){
  jaccard_sim = NA

  if(is.character(words1) && is.character(words2)&& length(words1) ==1 && length(words2)==1 ){
    if(!quick_seg && (is.null(engine)||length(engine)==0)){
      utils::data("dict_add_cn")
      dict_add_cn = eval(parse(text = "dict_add_cn"))
      dict_add_cn  = as.data.frame(dict_add_cn)
      engine =jiebaR:: worker()
      data.table::fwrite(dict_add_cn,
                         file = engine$PrivateVarible$dict,append = TRUE)
      engine =jiebaR:: worker()
    }
    term1 = cut_words(gsub("[^\u4e00-\u9fa5,^A-Za-z,^0-9]", "",words1),engine = engine, quick_seg = quick_seg)
    term2 = cut_words(gsub("[^\u4e00-\u9fa5,^A-Za-z,^0-9]", "",words2),engine = engine,quick_seg = quick_seg )
   jaccard_sim =  length(intersect(term1, term2))/length(union(term1,term2))
  }
  jaccard_sim
}

#' @rdname words_jaccard_similarity
#' @export
#'
words_cos_similarity <- function(words1,words2, engine = NULL,quick_seg = TRUE){
  words1 =  ifelse(is.null(words1) | words1 == "" | words1 == "null" |
           words1== "NULL" | words1 == " " | words1 %in% c("-1","Missing"), NA, words1)
  words2 =  ifelse(is.null(words2) | words2 == "" | words2 == "null" |
                      words2== "NULL" | words2 == " " | words2 %in% c("-1","Missing"), NA, words2)
  if(is.character(words1) && is.character(words2)&& length(words1) ==1 && length(words2)==1 &&
     words1 != ''&&words2 !=''){
    coments =  as.vector(c(words1,words2))
     id = logic = term = NULL
     x = y = NA
     if(!quick_seg && (is.null(engine)||length(engine)==0)){
       utils::data("dict_add_cn")
       dict_add_cn = eval(parse(text = "dict_add_cn"))
       dict_add_cn  = as.data.frame(dict_add_cn)
       engine =jiebaR:: worker()
       data.table::fwrite(dict_add_cn,
                          file = engine$PrivateVarible$dict,append = TRUE)
       engine =jiebaR:: worker()
     }
    words = lapply(coments,function(x)cut_words(gsub("[^\u4e00-\u9fa5,^A-Za-z,^0-9]", "", x),
                                                engine = engine, quick_seg = quick_seg))

    id = rep( 1:length(coments), unlist(lapply(words, length)))

    wordsterm = data.frame(id = id, term = unlist(words), stringsAsFactors = FALSE)
    wordsterm = wordsterm[grepl("\\S", wordsterm$term),]
    wordsterm$logic = rep(1, nrow(wordsterm))
    if(nrow(wordsterm)> 0){
      df = aggregate(logic ~ id +  term, data = wordsterm, FUN = sum)

      temp = data.frame(table(df$term),stringsAsFactors = FALSE)
      names(temp) = c("term", "df")
      df = merge(df, temp,by = 'term')
      tfidf = data.table::dcast(setDT(df), id ~ term, sum, value.var = "df")[,-1]
      x= tfidf[1,]
      y = tfidf[2,]
    }

    if(!any(is.na(x))&!any(is.na(y))){
      cos_sim(x,y,cos_margin = 2)
    }else{
      NA
    }

  }else{
    NA
  }
}

#' cos_sim
#'
#' This function is not intended to be used by end user.
#'
#' @param  x  A list of numbers
#' @param  y  A list of numbers
#' @param cos_margin Margin of matrix, 1 for rows and 2 for cols, Default is 1.
#' @return A number of cosin similarity

cos_sim <- function(x, y, cos_margin = 1) {
  opt = options(digits = 6)
  x = as.numeric(x)
  y = as.numeric(y)
  max_x = max(unlist(x), na.rm = TRUE)
  min_x = min(unlist(x), na.rm = TRUE)
  max_y = max(unlist(y), na.rm = TRUE)
  min_y = min(unlist(y), na.rm = TRUE)
  x = ifelse(x - min_x > 0, x - min_x, 0.00001) / ifelse((max_x - min_x) > 0, max_x - min_x, 1)
  y = ifelse(y - min_y > 0, y - min_y, 0.00001) / ifelse((max_y - min_y) > 0, max_y - min_y, 1)
  x = as.matrix(x)
  y = as.matrix(y)
  dist = NULL
  if (cos_margin == 1) {
    nr = nrow(y)
    dist = sapply(1:nr, function(j) colSums(t(x) * y[j,], na.rm = TRUE) /
                    sqrt(rowSums(x ^ 2, na.rm = TRUE) * sum(y[j,] ^ 2, na.rm = TRUE)))
    colnames(dist) = rownames(y)
  } else {
    nc = ncol(y)
    j =1
    dist = sapply(1:nc, function(j) colSums(x * y[, j], na.rm = TRUE) /
                    sqrt(colSums(x ^ 2, na.rm = TRUE) * sum(y[, j] ^ 2, na.rm = TRUE)))
    colnames(dist) = colnames(y)
  }
  return(round(dist,4))
  on.exit(options(opt))
}


