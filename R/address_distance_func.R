#' Address distance
#'
#' \code{address_distance} is for computing distance between multi sets of address.
#' @param add list of longitude and latitude.
#' @return A list contains address distance
#' @importFrom utils  URLencode
#' @importFrom creditmodel loop_function stop_parallel_computing start_parallel_computing
#' @importFrom cli cat_line
#' @importFrom  jiebaR qseg
#' @importFrom  parallel makeCluster parSapply detectCores
#' @importFrom  geosphere distm distVincentyEllipsoid
#' @seealso \code{\link{multi_address_distance}}
#' @examples
#' add = list(x = c(120,30),y = c(120,20),z = c(120,10))
#' address_distance(add)
#' @export



address_distance <- function(add) {
  add = data.frame(add,stringsAsFactors = FALSE )
  dist = list()
  colname = list()
  for (i in 1:ncol(add)) {
    if  ( i >= ncol(add)) break
    dist[[i]] = apply(add[ ,i:ncol(add)], 2,
                       function(x) round(geosphere::distm(as.numeric(add[ ,i]),
                                               as.numeric(x),
                                               fun = geosphere::distVincentyEllipsoid),0))
    colname[[i]] = lapply(names(add)[i:(ncol(add))],
                           function(n) paste(names(add)[i], n, sep = '_to_'))
  }
  dist = unlist(dist)
  names(dist) = unlist(colname)
  splitvar = strsplit(names(dist),"_to_")
  vars = c()
  for (i in 1:(length(dist))) {
    if (splitvar[[i]][1] == splitvar[[i]][2]) {
      vars[[i]] = names(dist)[i]
    } else {
      vars[[i]] = NA
    }
  }
  dist = dist[is.na(vars)]
  return(dist)
}

#' Multiple address distance
#'
#' \code{multi_address_distance} is for computing  distance between multi sets of addresses.
#' @param dat data.frame contains address variables.
#' @param lng Name of Longitude variable of dat, or a list of Longitudes.
#' @param lat  Name of Latitude variable of dat, or a list of Latitudes.
#' @param ID The name of ID of observations. Default is NULL.
#' @param parallel Parallel computing option.Default is FALSE.
#' @return A data.frame contains address distance.
#' @seealso \code{\link{address_distance}}
#' @importFrom utils  URLencode
#' @importFrom creditmodel loop_function stop_parallel_computing start_parallel_computing
#' @importFrom cli cat_line
#' @importFrom  jiebaR qseg
#' @importFrom  parallel makeCluster parSapply detectCores
#' @importFrom  geosphere distm distVincentyEllipsoid
#' @examples
#' add = rbind(add1 = c(x = c(120,30),y = c(120,20),z = c(120,10)),
#'                 add2 = c(x = c(130,30),y = c(140,30),z = c(120,30)))
#' multi_address_distance(dat = add,lat = "2",lng ="1")
#' @export

multi_address_distance <- function(dat,lat= 'lat',lng = 'lng', ID = NULL,parallel = FALSE){
  opt = options(scipen=100, stringsAsFactors = FALSE)
   dat = checking_data(dat)
   if (is.null(ID)) {
     dat$ID = rownames(dat)
     ID = 'ID'
   }
  if(is.character(lng) && length(lng)==1){
    latitude = dat[grepl(lat ,paste(colnames(dat)))]

  }else{
    latitude = lat
  }
  if(is.character(lat) && length(lat)==1){
    longitude = dat[grepl(lng,paste(colnames(dat)))]

  }else{
    longitude = lng
  }

  lat_lng = list()

  for (i in 1: nrow(latitude)) {
    lat_lng[i] = list(rbind((unlist(longitude[i,])) ,unlist(latitude[i,])))
    colnames(lat_lng[[i]]) = sub("_lng",'',colnames(lat_lng[[i]]))
  }
  if(parallel) {
  dist = data.frame(t(parSapply(cl = makeCluster(detectCores(logical = FALSE)-1),
                                X = lat_lng, FUN = get("address_distance"))))
  }else {
    dist = data.frame(t(sapply(lat_lng, function(x)address_distance(add = x))))

  }
  splitvar = strsplit(names(dist),"_to_")
  vars = c()
  for (i in 1:(length(dist))) {
    if (splitvar[[i]][1] == splitvar[[i]][2]) {
      vars[[i]] = names(dist)[i]
    } else {
      vars[[i]] = NA
    }
  }
  add_comp_dist = cbind(dat[ID], dist[is.na(vars)])
  on.exit(options(opt))
  return(add_comp_dist)
}


