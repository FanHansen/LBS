#' Transform longitude and latitude to address
#'
#' \code{lat_lng_to_address} is used for transforming longitude and latitude to address.
#' @param lng A list of Longitudes.
#' @param lat A list of Latitudes.
#' @param map Which map API, 'baidu' and 'amap' are available.
#' @param coordtype Coordtype of longitude and latitude. Default is wgs8411.
#' @param key  Key of map. Apply from \url{https://lbs.amap.com/api/webservice/guide/api/georegeo/}
#' or \url{https://developer.baidu.com/map/android-mobile-apply-key.htm}
#' @return A list of formmated address.
#' @importFrom rjson  fromJSON
#' @importFrom utils  URLencode
#' @importFrom creditmodel loop_function stop_parallel_computing start_parallel_computing
#' checking_data love_color get_names quick_as_df
#' @importFrom  cli cat_line
#' @importFrom  jiebaR qseg
#' @importFrom  data.table rbindlist
#' @importFrom foreach %dopar% foreach
#' @examples
#' lat_lng_to_address(lat = 31.231078,lng = 121.699373,key =  NULL)
#' @export

lat_lng_to_address <- function(lng = NULL,lat = NULL,map = 'baidu',key = NULL,coordtype = "wgs84ll") {
  baidu_address = baidu_lat = baidu_lng = province =city =district =
    address_formatted = address_description= NA
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
  if ((!is.null(lat)&!is.null(lng))&& (lat != ""& lng != "" ) && (!is.na(lng) & !is.na(lat))&&
      (as.numeric(lat) >0 & as.numeric(lng) > 0)) {
    lat = as.character(lat)
    lng = as.character(lng)
    content = paste(lat, lng, sep=',')
    if(is.null(map)){map = 'baidu' }
      if(map == 'baidu'){
      m_url = paste0('http://api.map.baidu.com/reverse_geocoding/v3/?ak=',sub_key,
                    '&output=json&coordtype=',coordtype,'&location=',content,sep='')
      url_string =  try(URLencode(m_url), silent = TRUE)
      connect = try(url(url_string),silent = TRUE)
      geo = NULL
      if( length(summary(connect)$`can read`) >0 && summary(connect)$`can read` == 'yes'){
        c_json =  try(readLines(connect, warn=FALSE, encoding = 'UTF-8',skipNul = TRUE),silent = TRUE)
        geo = try(fromJSON(paste(c_json, collapse = "")),silent = TRUE)
      }
     close(connect)
    }else{
      m_url = paste('https://restapi.amap.com/v3/geocode/regeo?output=json&location=',
                    content,'&key=',sub_key,'&radius=1000&extensions=all',sep='')
      url_string = try(URLencode(m_url), silent = TRUE)
      connect = try(url(url_string), silent = TRUE)
      geo = NULL
      if(length(summary(connect)$`can read`) >0 && summary(connect)$`can read` == 'yes'){
        c_json =  try(readLines(connect, warn=FALSE, encoding = 'UTF-8',skipNul = TRUE),silent = TRUE)
        geo = try(fromJSON(paste(c_json, collapse = "")),silent = TRUE)
      }
      close(connect)
    }
    if(is.null(geo) || any(grepl("Error|error",geo))){
          if(length(key)>1){
    sub_key = sample(key,1,replace = FALSE)
    }else{
    sub_key = key
    }
    if(map == 'baidu'){
      m_url = paste0('http://api.map.baidu.com/reverse_geocoding/v3/?ak=',sub_key,
                    '&output=json&coordtype=',coordtype,'&location=',content,sep='')
      url_string =  try(URLencode(m_url), silent = TRUE)
      connect = try(url(url_string),silent = TRUE)
      geo = NULL
      if( length(summary(connect)$`can read`) >0 && summary(connect)$`can read` == 'yes'){
        c_json =  try(readLines(connect, warn=FALSE, encoding = 'UTF-8',skipNul = TRUE),silent = TRUE)
        geo = try(fromJSON(paste(c_json, collapse = "")),silent = TRUE)
      }
     close(connect)
    }else{
      m_url = paste('https://restapi.amap.com/v3/geocode/regeo?output=json&location=',
                    content,'&key=',sub_key,'&radius=1000&extensions=all',sep='')
      url_string = try(URLencode(m_url), silent = TRUE)
      connect = try(url(url_string), silent = TRUE)
      geo = NULL
      if(length(summary(connect)$`can read`) >0 && summary(connect)$`can read` == 'yes'){
        c_json =  try(readLines(connect, warn=FALSE, encoding = 'UTF-8',skipNul = TRUE),silent = TRUE)
        geo = try(fromJSON(paste(c_json, collapse = "")),silent = TRUE)
      }
       close(connect)

    }
    }
    if(!any(grepl("Error|error",geo)) && !is.null(geo) && is.list(geo) && (map == 'baidu' & as.numeric(geo$status) == 0)){
      address_formatted = ifelse(length(geo$result$formatted_address) >0 , geo$result$formatted_address, NA)
      if(length(geo$result$sematic_description)>0 && geo$result$sematic_description != ''){
        address_description = paste(address_formatted,
                                    '(',geo$result$sematic_description[1],')',sep='')

      }else{
           if(length(geo$result$business[1]) > 0  && geo$result$business[1] != ''){
            address_description = paste(address_formatted,paste('(',geo$result$business[1],')',sep=''))
          }else{
            address_description = address_formatted
          }
      }
      country= ifelse(length(geo$result$addressComponent$country) >0 ,geo$result$addressComponent$country, NA)
      province = ifelse(length(geo$result$addressComponent$province) >0,geo$result$addressComponent$province, NA)
      city = ifelse(length(geo$result$addressComponent$city) >0,geo$result$addressComponent$city, NA)
      district = ifelse(length(geo$result$addressComponent$district) >0,geo$result$addressComponent$district, NA)
    }else{
      if(!any(grepl("Error|error",geo)) && !is.null(geo) && map == 'amap'){
        address_formatted =  geo$regeocode$formatted_address
        address_description = paste(address_formatted,
                                    '(', geo$regeocode$addressComponent$streetNumber$street,
                                    geo$regeocode$addressComponent$streetNumber$number,
                                    geo$regeocode$addressComponent$streetNumber$direction,')',sep='')

        province = geo$regeocode$addressComponent$province
        city = ifelse(is.null(geo$regeocode$addressComponent$city),
                      province,geo$regeocode$addressComponent$city)
        district = geo$regeocode$addressComponent$district

      }

    }

  }
  return(list(address_formatted = address_formatted, address_description = address_description,
              province = province,city = city ,district = district))

}

#' Transform multiple longitude and latitude to address
#'
#' \code{lat_lng_to_address_all} is used for transforming multiple longitude and latitude to address.
#' @param dat data.frame contains Longitude and Latitudes.variables.
#' @param lng Name of Longitude variable of dat, or a list of Longitudes.
#' @param lat  Name of Latitude variable of dat, or a list of Latitudes.
#' @param ID The name of ID of observations. Default is NULL.
#' @param map Which map API, 'baidu' and 'ampap' are available.
#' @param coordtype coordtype of longitude and latitude. Default is wgs8411.
#' @param key  Key of map. Apply from \url{https://lbs.amap.com/api/webservice/guide/api/georegeo/}
#' or \url{https://developer.baidu.com/map/android-mobile-apply-key.htm}
#' @param parallel Parallel computing option.Default is FALSE.
#' @return A data.frame with formatted address.
#' @importFrom rjson  fromJSON
#' @importFrom utils  URLencode
#' @importFrom creditmodel loop_function stop_parallel_computing start_parallel_computing
#' checking_data love_color get_names quick_as_df
#' @importFrom  cli cat_line
#' @importFrom  jiebaR qseg
#' @importFrom  data.table rbindlist
#' @importFrom foreach %dopar% foreach
#' @examples
#' lat_lng_to_address_all(lat = c(30.00,40.00), lng = c(120,110),parallel = FALSE)
#' @export

lat_lng_to_address_all  <- function(dat = NULL,lng = NULL,lat = NULL, ID = NULL, map = 'baidu',key = NULL,
                                parallel = FALSE, coordtype = "wgs84ll"){
  opt = options(scipen=100, stringsAsFactors = FALSE)
  cat_line(paste("--", "Transform longitude and latitude to address.\n"), col = love_color("dark_green"))

  if(is.null(key)){
    cat_line("Please register amap account or baidu map account to apply for Key before use.\n",
             col = love_color("dark_red"))
    key = "XuTLxT21oCalIGHOqxGyc3un0qINOXzd"
  }
  if (is.null(ID)) {
     dat$ID = rownames(dat)
     ID = 'ID'
   }
  if(is.null(map))map = 'baidu'
  if (parallel) {
    parallel = start_parallel_computing(parallel)
    stopCluster = TRUE
  } else {
    parallel= stopCluster = FALSE
  }
  if(is.character(lng) && length(lng)==1){
    lng_list = dat[,lng]
  }else{
    lng_list = unlist(lng)
  }
  if(is.character(lat) && length(lat)==1){
    lat_list = dat[,lat]
  }else{
    lat_list = unlist(lat)
  }
  if(length(lat_list)!= length(lat_list))stop("Longitude and Latitude must be same length.\n")
  i. = NULL
  if (!parallel) {
    add_list = lapply(1:length(lat_list), function(i.) lat_lng_to_address(lng = lng_list[[i.]],
                                                              lat = lat_list[[i.]], map = map,key = key))
    add_list = data.table::rbindlist(add_list)
  } else {
    add_list = foreach(i. = 1:length(lat_list),
                          .errorhandling = c('pass')) %dopar% {
                            try(do.call(lat_lng_to_address, args = list(lng = lng_list[[i.]],
                                                                lat = lat_list[[i.]],map = map,
                                                                key = key)), silent = TRUE)
                          }
    add_list = data.table::rbindlist(add_list)
  }
  add_list = cbind(dat[ID],add_list)
  on.exit(options(opt))
  on.exit(if (parallel & stopCluster) stop_parallel_computing(attr(parallel, "cluster")))
  return(quick_as_df(add_list))
}



