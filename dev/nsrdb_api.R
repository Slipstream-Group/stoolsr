.libPaths("S:\\R-Shared\\packagelib - r4 - current")

library(tidyverse)


#' Get TMY Data for Lat/Lon from NREL NSRDB API
#'
#' @param lat numeric latitude for site
#' @param lon numeric longitude for site
#'
#' @return list containing 1) TMY data as data frame and 2) metadata as named list
#' @export
#'
#' @examples
#' stns <- illumer2::get_stns()
#' stn <- sample_n(stns, 1)
#' tmy <- get_tmy(stn$lat, stn$lon)

get_tmy <- function(lat, lon){
  # https://nsrdb.nrel.gov/about/tmy.html

  # You must request an NSRDB api key from the link above
  api_key = 'hn7pzescl5xtiibLsBQEP7KYpSJKebG7YZUOIGL5'
  # Set the attributes to extract (e.g., dhi, ghi, etc.), separated by commas.
  # OPTIONS: dew_point, dhi, dni, ghi, surface_albedo, surface_pressure, air_temperature, wind_direction, wind_speed"
  vars = 'air_temperature,dew_point,wind_speed'
  # Choose year of data
  year = 'tmy'
  # Set leap year to true or false. True will return leap day data if present, false will not.
  leap_year = 'false'
  # Set time interval in minutes, i.e., '30' is half hour intervals. Valid intervals are 30 & 60.
  interval = '60'
  # Specify Coordinated Universal Time (UTC), 'true' will use UTC, 'false' will use the local time zone of the data.
  # NOTE: In order to use the NSRDB data in SAM, you must specify UTC as 'false'. SAM requires the data to be in the
  # local time zone.
  utc = 'false'
  # Your full name, use '+' instead of spaces.
  myname = 'ILLUME+Analyst'
  # Your reason for using the NSRDB.
  reason = 'evaluation'
  # Your affiliation
  affiliation = 'ILLUME+Advising'
  # Your email address
  email = 'research@illumeadvising.com'
  # Please join our mailing list so we can keep you up-to-date on new developments.
  mailing_list = 'true'

  url <- paste0("https://developer.nrel.gov/api/solar/nsrdb_psm3_download.csv?wkt=POINT(",lon,
                "%20",lat,
                ")&names=",year,
                "&leap_day=",leap_year,
                "&interval=",interval,
                "&utc=",utc,
                "&full_name=",myname,
                "&email=",email,
                "&affiliation=",affiliation,
                "&mailing_list=",mailing_list,
                "&reason=",reason,
                "&api_key=",api_key,
                "&attributes=",vars
                )


  meta <- readr::read_lines(url, n_max = 2)
  mlist <- lapply(meta, strsplit,",")

  meta_data <- as.list(mlist[[2]][[1]])
  names(meta_data) <- mlist[[1]][[1]]



  data_df <- readr::read_csv(url, skip=2, readr::col_types = "dddddddd")

  res <- list(tmy_data = data_df, meta_data = meta_data)
  return(res)

  }


