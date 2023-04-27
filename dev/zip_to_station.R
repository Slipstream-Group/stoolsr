.libPaths("S:\\R-Shared\\packagelib - r4 - current")

library(fuzzyjoin)
library(zipcodeR)
library(uk)

zip_to_stn <- function(zip5,
                       states = NULL,
                       begin_date = NULL,
                       end_date = "2021-01-01",
                       max_dist = 100, return_all = FALSE){
  zip_df <- zipcodeR::reverse_zipcode(zip5)[, c("zipcode", "state", "lat", "lng")]
  names(zip_df)[3:4] <- c("lat_zip","lon_zip")
  if(is.null(states)){
    states <- zip_df$state
  }
  zip_df$state <- NULL
  stns <- illumer2::isd_stns[illumer2::isd_stns$state %in% states, ]
  if(!is.null(begin_date)){
    stns <- stns[stns$begin<=as.Date(begin_date),]
  }
  if(!is.null(end_date)){
    stns <- stns[stns$end>=as.Date(end_date),]
  }

  stns <- stns[, c("stn_id", "usaf", "wban", "state", "lat", "lon", "begin", "end")]
  names(stns)[5:6] <- c("lat_stn", "lon_stn")

  match_df <- fuzzyjoin::geo_left_join(zip_df, stns,
                                       by = c("lat_zip"="lat_stn",
                                              "lon_zip"="lon_stn"),
                                       unit = "miles",
                                       distance_col = "miles_to_stn",
                                       max_dist = max_dist)


  if(all(is.na(match_df$miles_to_stn))){
    stop("No stations found within the max_dist arg. Try increasing max_dist")
  }

  if(!return_all){
    match_df <- match_df[match_df$miles_to_stn == min(match_df$miles_to_stn),]
  }

  match_df$lat_zip <- NULL
  match_df$lon_zip <- NULL



  return(match_df)


}

x <- zip_to_stn(zip5 = "53715", max_dist =1, return_all = TRUE)
x

library(tidyverse)

library(ggmap)

snailturtle_api = "AIzaSyA-y3Q1u0Dxz_gEJ7T9tbtBY6rxE0A4894"

register_google(key = snailturtle_api)

coords = apply(stn_id_tbl[,c("longitude", "latitude")], 1, function(x) list(as.numeric(x)))
coords = flatten(coords)
addresses = lapply(coords, revgeocode, output = "address")

stn_id_tbl$address = unlist(addresses)
# extract town and zip with regex
stn_id_tbl$zip5 = str_extract(stn_id_tbl$address, "[:digit:]{5}(?=, USA)")
stn_id_tbl <- stn_id_tbl[!is.na(stn_id_tbl$zip5),]
stn_id_tbl$town = str_extract(stn_id_tbl$address, "\\w*[:space:]*[:alpha:]+(?=, [:upper:]{2} [:digit:]{5})")
stn_id_tbl <- stn_id_tbl[!is.na(stn_id_tbl$town),]
stn_id_tbl$town = str_trim(stn_id_tbl$town, side = "both")
