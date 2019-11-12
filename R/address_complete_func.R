#' Complete addresses
#'
#' \code{complete_address} is used for getting complete address from map API.
#' @param address Name of address variable of dat, or a list of addresses.
#' @param map Which map API, 'baidu' and 'ampap' are available.
#' @param key  Key of map. Apply from \url{https://lbs.amap.com/api/webservice/guide/api/georegeo/} or \url{https://developer.baidu.com/map/android-mobile-apply-key.htm}
#' @return A list contains geo information
#' @seealso \code{\link{multi_complete_address}},\code{\link{complete_address_all}}
#' @importFrom rjson  fromJSON
#' @importFrom utils  URLencode data
#' @importFrom creditmodel loop_function stop_parallel_computing start_parallel_computing
#' @importFrom  cli cat_line
#' @importFrom  jiebaR qseg
#' @importFrom utils data
#' @examples
#' data(dict_add_cn)
#' addr = paste(substr(dict_add_cn[[1]][1],1,2),substr(dict_add_cn[[1]][2],1,2),
#' substr(dict_add_cn[[1]][3],1,3),substr(dict_add_cn[[1]][4],1,3))
#' complete_address(addr)
#' @export


complete_address <- function(address,key = NULL,map = 'baidu') {

  lat = lng = address_complete = address_formatted =
    province =country= city= district= address_description = address_level=
    address_confidence =address_comprehension = address_precise = NA
  
  if(is.null(key)){
    cat_line("Please register amap account or baidu map account to apply for Key before use.\n",
             col = love_color("dark_red"))
    key = "XuTLxT21oCalIGHOqxGyc3un0qINOXzd"
  }
  if(length(key)>1){
    sub_key = sample(key,1,replace = FALSE)
  }else{
    sub_key = key
  }
  address =gsub(enc2utf8("[^\u4e00-\u9fa5,^a-zA-Z,^0-9,^-]"), "", address)
  address =  enc2utf8(address)
  if(is.null(map))map = 'baidu'
  if (!is.na(address)) {
    if(map == 'baidu'){
      m_url = paste("http://api.map.baidu.com/geocoding/v3/?address=",address,
                   "&output=json&ak=", sub_key,sep = "")
    }else{
      m_url = paste('http://restapi.amap.com/v3/place/text?key=',sub_key,'&keywords=',address,
                    '&types=&city=&children=1&offset=1&page=1&extensions=base',sep='')
    }
    url_string = try(URLencode(m_url),silent = TRUE)
    connect = try(url(url_string),silent = TRUE)
    temp_geo = geo = NULL
    if( length(summary(connect)$`can read`)>0 && summary(connect)$`can read` == 'yes'){
    m_json =  try(readLines(connect, warn= FALSE, encoding = 'UTF-8',skipNul = TRUE),silent = TRUE)
    temp_geo = try(fromJSON(paste(m_json, collapse = "")),silent = TRUE)
    close(connect)
    }
   if(any(grepl("Error|error",temp_geo))){
      if(length(key)>1){
    sub_key = sample(key,1,replace = FALSE)
  }else{
    sub_key = key
  }
    if(map == 'baidu'){
      m_url = paste("http://api.map.baidu.com/geocoding/v3/?address=",address,
                   "&output=json&ak=", sub_key,sep = "")
    }else{
      m_url = paste('http://restapi.amap.com/v3/place/text?key=',sub_key,'&keywords=',address,
                    '&types=&city=&children=1&offset=1&page=1&extensions=base',sep='')
    }
    url_string = try(URLencode(m_url),silent = TRUE)
    connect = try(url(url_string), silent = TRUE)
    temp_geo = geo = NULL
    if( length(summary(connect)$`can read`)>0 && summary(connect)$`can read` == 'yes'){
    m_json =  try(readLines(connect, warn= FALSE, encoding = 'UTF-8',skipNul = TRUE),silent = TRUE)
    temp_geo = try(fromJSON(paste(m_json, collapse = "")),silent = TRUE)
    close(connect)
    }
   }
    if(!any(grepl("Error|error",temp_geo)) && !is.null(temp_geo) && length(temp_geo$status) > 0 &&
     length(temp_geo$result) >0 &&
     ((map == 'baidu' &&  as.numeric(temp_geo$status) != 302 ) ||
       (map == 'amap' && temp_geo$info != "DAILY_QUERY_OVER_LIMIT" ))){
      if (map == 'baidu' & (as.numeric(temp_geo$status) == 0)) {
        lat = ifelse(length(temp_geo$result$location$lat) >0 ,temp_geo$result$location$lat,NA)
        lng = ifelse(length(temp_geo$result$location$lng) >0 , temp_geo$result$location$lng,NA)
        content = paste(lat, lng, sep=',')
          if(length(key)>1){
            sub_key = sample(key,1,replace = FALSE)
            }else{
               sub_key = key
            }
        c_url = paste('http://api.map.baidu.com/reverse_geocoding/v3/?ak=',sub_key,
                      '&output=json&location=',content,sep='')
        c_url_string = URLencode(c_url)
        c_connect = url(c_url_string)
        geo = NULL
        if( length(summary(c_connect)$`can read`)>0 && summary(c_connect)$`can read` == 'yes'){
        
        c_json =  try(readLines(c_connect, warn=FALSE, encoding = 'UTF-8',skipNul = TRUE),silent = TRUE)
        geo = try(fromJSON(paste(c_json, collapse = "")),silent = TRUE)
        close(c_connect)
        }
        if(any(grepl("Error|error",geo))){
          if(length(key)>1){
    sub_key = sample(key,1,replace = FALSE)
  }else{
    sub_key = key
  }
        c_url = paste('http://api.map.baidu.com/reverse_geocoding/v3/?ak=',sub_key,
                      '&output=json&location=',content,sep='')
        c_url_string = URLencode(c_url)
        c_connect = url(c_url_string)
        if(length(summary(c_connect)$`can read`)>0 && summary(c_connect)$`can read` == 'yes'){
        
        c_json =  try(readLines(c_connect, warn=FALSE, encoding = 'UTF-8',skipNul = TRUE),silent = TRUE)
        geo = try(fromJSON(paste(c_json, collapse = "")),silent = TRUE)
        close(c_connect)
        }
        }

        if(!any(grepl("Error|error",geo)) && !is.null(geo) && length(geo$result) >0 && is.list(geo)){
          address_formatted =  ifelse(length(geo$result$formatted_address) >0 ,try(geo$result$formatted_address,silent = TRUE),NA)
        if(length(geo$result$sematic_description) >0 && geo$result$sematic_description != ''){
          address_description = ifelse(length(geo$result$sematic_description) >0,paste(address_formatted,
                                      '(',geo$result$sematic_description[1],')',sep=''),NA)

        }else{
          if(length(geo$result$business[1]) > 0  && geo$result$business[1] != ''){
            address_description = ifelse(length(geo$result$business[1]) > 0,
             paste(address_formatted,paste('(',geo$result$business[1],')',sep='')),NA)
          }else{
            address_description = address_formatted
          }

        }
        address_precise = ifelse(length(temp_geo$result$precise) >0 ,try(temp_geo$result$precise,silent = TRUE),NA)
        address_confidence =  ifelse(length(temp_geo$result$confidence) > 0 , try(temp_geo$result$confidence,silent = TRUE),NA)
        address_comprehension = ifelse(length(temp_geo$result$comprehension) > 0 , try(temp_geo$result$comprehension,silent = TRUE),NA)
        address_level = ifelse(length(temp_geo$result$level) >0 ,try(temp_geo$result$level,silent = TRUE) , NA)
        country= ifelse(length(geo$result$addressComponent$country)>0 ,try(geo$result$addressComponent$country,silent = TRUE) ,NA)
        province = ifelse(length(geo$result$addressComponent$province)>0 ,try(geo$result$addressComponent$province,silent = TRUE), NA)
        city = ifelse(length(geo$result$addressComponent$city)>0 ,try(geo$result$addressComponent$city,silent = TRUE), NA)
        district = ifelse(length(geo$result$addressComponent$district)>0 , try(geo$result$addressComponent$district,silent = TRUE), NA)
        }
       
      }else{
        if (map == 'amap' && as.numeric(temp_geo$status) == 1 && as.numeric(temp_geo$count)!= 0) {

          content = temp_geo$pois[[1]]$location
          lat = strsplit(content,split =',')[[1]][2]
          lng =  strsplit(content,split =',')[[1]][1]

          c_url = paste('https://restapi.amap.com/v3/geocode/regeo?output=json&location=',
                        content,'&key=',sub_key,'&radius=1000&extensions=all',sep='')
          c_url_string = URLencode(c_url)
          c_connect = url(c_url_string)
          if( summary(c_connect)$`can read` == 'yes'){
            c_json = try(readLines(c_connect, warn=FALSE,encoding = 'UTF-8',skipNul = TRUE),silent = TRUE)
            geo =  try(fromJSON(paste(c_json, collapse = "")),silent = TRUE)
            address_formatted =  geo$regeocode$formatted_address
            address_description = paste(address_formatted,
                                        '(', geo$regeocode$addressComponent$streetNumber$street,
                                        geo$regeocode$addressComponent$streetNumber$number,
                                        geo$regeocode$addressComponent$streetNumber$direction,')',sep='')
            country = geo$regeocode$addressComponent$country
            province = geo$regeocode$addressComponent$province
            city = ifelse(is.null(geo$regeocode$addressComponent$city),province,geo$regeocode$addressComponent$city)
            district = geo$regeocode$addressComponent$district

          }
        }
      }

      #aquire the province & city & district of baidu return address
      cut_char1 =  paste(province,city,district,sep='')
      #complet original address
      if (!is.na(country)  && length(country) >0 &&gregexpr(country, address)[[1]][1] != -1) {
        address = substr(address, 3, nchar(address))
      }

      location4 = ifelse(!is.na(district)  && length(district) > 0 && gregexpr(substr(district,nchar(district),nchar(district)), address)[[1]][1] != -1,
                         gregexpr(substr(district,nchar(district),nchar(district)), address)[[1]][1],-1)
      if (location4 != -1) {
        if (location4 == 1) {
          w1_1 = cut_words(substr(address, 1, 1))
        } else {
          w1_1 = cut_words(substr(address, 1, location4-1))
        }
        w1_2 = substr(address, location4 + 1, nchar(address))
        w2 = unlist(lapply(w1_1, function(x) gregexpr(x, cut_char1)[[1]][1]))[1]
        if (w2 > 0) {
          w3 = unlist(lapply(w1_1, function(x) gregexpr(x, cut_char1)[[1]][1]))
          w4 = which(w3 < 0)[1]
          location5 = ifelse(is.na(w4), nchar(address), gregexpr(w1_1[w4], address)[[1]][1])
          cut_char2 = ifelse(is.na(w4),w1_2,substr(address, location5, nchar(address)))
        } else {
          cut_char2 = address
        }
      } else {
        a = cut_words(address)
        p = unlist(lapply(a, function(x) gregexpr(x, cut_char1)[[1]][1]))
        re = which(p < 0)[1]
        location6 <- ifelse(is.na(re),nchar(address),gregexpr(a[re], address)[[1]][1])
        cut_char2 <- ifelse(is.na(re), '', substr(address, location6, nchar(address)))
      }
      if ( is.na(province ) && is.na(city)) {
        address_complete = NA
      } else {
        address_complete = paste(cut_char1,cut_char2,sep = '')
      }
    }
  }


  return(list(address_original = address, address_formatted = address_formatted,
              address_completed = address_complete, address_description = address_description,
              province = province, city = city, district = district,lng = lng, lat = lat,
              address_level = address_level,
              address_confidence = address_confidence,address_comprehension = address_comprehension,
              address_precise = address_precise))
}

#' Multiple address geographic coding
#' \code{multi_complete_address} is used for multiple address geographic coding.
#' @param dat data.frame contains address variables.
#' @param x_list Names of addresses.
#' @param ex_cols A list of excluded variables. Regular expressions can also be used to match variable names. Default is NULL.
#' @param map Which map API, 'baidu' and 'amap' are available.
#' @param key  Key of map. Apply from \url{https://lbs.amap.com/api/webservice/guide/api/georegeo/} or \url{https://developer.baidu.com/map/android-mobile-apply-key.htm}
#' @return A data.frame
#' @seealso \code{\link{complete_address}},\code{\link{complete_address_all}}
#' @importFrom rjson  fromJSON
#' @importFrom utils  URLencode data
#' @importFrom creditmodel loop_function stop_parallel_computing start_parallel_computing
#' @importFrom  cli cat_line
#' @importFrom  jiebaR qseg
#' @importFrom utils data
#' @examples
#' data(dict_add_cn)
#' addr = paste(substr(dict_add_cn[[1]][1],1,2),substr(dict_add_cn[[1]][2],1,2),
#' substr(dict_add_cn[[1]][3],1,3),substr(dict_add_cn[[1]][4],1,3))
#' multi_complete_address(addr)
#' @export


multi_complete_address <-function(dat,x_list = NULL,ex_cols = NULL,map = NULL,
                                  key = NULL){

  if(is.null(map)|| (map == 'amap' & is.null(key)))map = 'baidu'
  if(is.null(key)){
    cat_line("Please register amap account or baidu map account to apply for Key before use.\n",
             col = love_color("dark_red"))
    key = "XuTLxT21oCalIGHOqxGyc3un0qINOXzd"
  }
  dat = data.frame(dat,stringsAsFactors = FALSE)

  if (is.null(x_list)) {
   add_x_list = get_names(dat = dat,
                           types = c('character', 'factor'),
                           ex_cols = c(ex_cols), get_ex = FALSE)
   }else{
    add_x_list = x_list
  }
  
  dat1 = lapply(unlist(dat[add_x_list]),function(x)complete_address(x,key = key,map = map))
  for (i in 1:length(dat1)){
    dat1[[i]] <-  t(unlist(dat1[i]))
  }
  names(dat1) = NULL
  dat2 = as.data.frame(dat1, stringsAsFactors=FALSE)
  dat2 = add_name(dat2,num = 13)
  rm(dat,dat1)
  return(dat2)
}
#' rename address variables
#' \code{add_name} is used for renaming address variables.
#' @param dat  A data.frame
#' @param num Num of address variables
#' @return A data.frame with new name.

add_name = function(dat, num) {

  for(i in 1:num) {
    new_name = switch (i,
                       "original",
                       "formatted",
                       "completed",
                       "description",
                       "province",
                       "city",
                       "district",
                       "lng",
                       "lat",
                       "level",
                       "confidence",
                       "comprehension" ,
                       "precise" )

    num_ind = grep(paste0(new_name,"$"), paste(colnames(dat)))
    colnames(dat)[num_ind] = gsub(paste("\\.\\S{1,100}$"),paste0('_',new_name), colnames(dat)[num_ind])
  }
  dat
}


#' Complete address all
#'
#' \code{complete_address_all} is used for multiple address geographic coding of all users.
#' @param dat data.frame contains address variables.
#' @param x_list Names of addresses.
#' @param ex_cols A list of excluded variables. Regular expressions can also be used to match variable names. Default is NULL.
#' @param ID The name of ID of observations. Default is NULL.
#' @param map Which map API, 'baidu' and 'ampap' are available.
#' @param key  Key of map. Apply from \url{https://lbs.amap.com/api/webservice/guide/api/georegeo/} or \url{https://developer.baidu.com/map/android-mobile-apply-key.htm}
#' @param parallel Parallel computing option.Default is FALSE.
#' @return A data.frame contains geo information
#' @seealso \code{\link{complete_address}},\code{\link{multi_complete_address}}
#' @importFrom rjson  fromJSON
#' @importFrom utils  URLencode data
#' @importFrom creditmodel loop_function stop_parallel_computing start_parallel_computing
#' @importFrom  cli cat_line
#' @importFrom  jiebaR qseg
#' @importFrom utils data
#' @examples
#' data(dict_add_cn)
#' addr = paste(substr(dict_add_cn[[1]][1],1,2),substr(dict_add_cn[[1]][2],1,2),
#' substr(dict_add_cn[[1]][3],1,3),substr(dict_add_cn[[1]][4],1,3))
#' @export

complete_address_all  <- function(dat,x_list = NULL,ID = NULL,ex_cols = NULL,
                                  key = NULL,map = NULL,
                                  parallel = FALSE){

  opt = options(scipen=100, stringsAsFactors = FALSE)
  if(is.null(map)|| (map == 'amap' & is.null(key)))map = 'baidu'
  dat = checking_data(dat,note = FALSE)
  if(is.null(key)){
    cat_line("Please register amap account or baidu map account to apply for Key before use..\n",
             col = love_color("dark_red"))
    key = "XuTLxT21oCalIGHOqxGyc3un0qINOXzd"
  }

  cat_line(paste("--", "Complete address.\n"), col = love_color("dark_green"))
  if (is.null(ID)) {
    dat$ID = rownames(dat)
    ID = 'ID'
  }

  if(!is.null(x_list)){
    address_list = dat[x_list]
  }else{
    x_list = get_names(dat = dat,
                        types = c('character', 'factor'),
                        ex_cols = c(ID, ex_cols), get_ex = FALSE)
    address_list = dat[x_list]
  }
  if (parallel) {
    parallel = creditmodel::start_parallel_computing(parallel)
    stopCluster = TRUE
  } else {
    parallel = stopCluster = FALSE
  }
  i. = NULL

  if (!parallel) {
    add_list =lapply(1:nrow(address_list), function(i.) multi_complete_address(
      dat = address_list[i.,],x_list = x_list, ex_cols = ex_cols,key = key,map = map))
    add_list = rbindlist(add_list ,fill=TRUE)
  } else {
    add_list = foreach(i. = 1:nrow(address_list),
                        .errorhandling = c('pass')) %dopar% {
                          try(do.call(multi_complete_address, args = list(dat = address_list[i.,], x_list = x_list, 
                           ex_cols = ex_cols, key = key, map = map)), silent = TRUE)
                        }
    add_list = rbindlist(add_list, fill=TRUE)
  }
  add_list = cbind(dat[ID],add_list)
   on.exit(options(opt))
   on.exit(if (parallel & stopCluster)creditmodel::stop_parallel_computing(attr(parallel, "cluster")))
  return(quick_as_df(add_list))
}


