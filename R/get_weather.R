#' Load Current ISD Station Meta-data
#'
#' @return data frame
#'
#'
#' Source: https://www1.ncdc.noaa.gov/pub/data/noaa/isd-history.csv
#'
#' @format data frame with 12 columns:
#' \describe{
#'   \item{stns_id}{combined usaf-wban codes string}
#'   \item{usaf}{USAF station code}
#'   \item{wban}{WBAN station code}
#'   \item{staion_name}{NOAA ISD station name}
#'   \item{ctry}{country abbreviation}
#'   \item{state}{state abbreviation}
#'   \item{icao}{ICAO station code}
#'   \item{lat}{station latitude}
#'   \item{lon}{station longitude}
#'   \item{evel_m}{station elevation in meters}
#'   \item{begin}{earliest data with available data}
#'   \item{end}{latest data with available data}
#'   ...
#' }
#' @export
#'
#' @examples
#' stn_data <- isd_stns()
isd_stns <- memoise::memoise(
  function(){
    isd_hist_url <- "https://www1.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"
    stns <- readr::read_csv(isd_hist_url, col_types = "ccccccdddcc")


    stns <- dplyr::filter(stns, !is.na(STATE), CTRY == "US")
    stns <- dplyr::rename_all(stns,tolower)
    stns <- dplyr::rename_all(stns, ~stringr::str_replace_all(., " ", "_"))
    stns <- dplyr::rename(stns, elev_m = `elev(m)`)
    stns <- dplyr::mutate_at(stns, c("begin", "end"), lubridate::ymd)
    stns <- dplyr::mutate(stns, stn_id = paste(usaf,wban, sep = "-"))
    stns <- dplyr::select(stns, stn_id, dplyr::everything())

    return(stns)
  }
)


#' Read ISD-Lite Hourly Data from NOAA FTP Site
#'
#' @param ftp_url ftp path to ".gz" file as character
#'
#' @return data frame
#' @export
#'
#' @examples
#' df <- read_isdlite("https://www.ncei.noaa.gov/pub/data/noaa/isd-lite/2021/025420-99999-2021.gz")
read_isdlite <- function(ftp_url){
  cat("Reading data from", ftp_url,"\n")
  x <- readr::read_table(ftp_url,
                  col_names = c("yr",
                                "mon",
                                "dy",
                                "hr",
                                "airtemp",
                                "dptemp",
                                "pressure",
                                "wind_dir",
                                "windspeed",
                                "skycond",
                                "precip1hr",
                                "precip6hr" ),
                  col_types =
                    readr::cols(
                      yr = readr::col_integer(),
                      mon = readr::col_integer(),
                      dy = readr::col_integer(),
                      hr = readr::col_integer(),
                      airtemp = readr::col_double(),
                      dptemp = readr::col_double(),
                      pressure = readr::col_double(),
                      wind_dir = readr::col_double(),
                      windspeed = readr::col_double(),
                      skycond = readr::col_double(),
                      precip1hr = readr::col_double(),
                      precip6hr = readr::col_double()
                    ) ,na = c("-9999")

  )

  return(x)


}

#' Download and Process Hourly ISDLite Data for Range of Years and Stations
#'
#' @param yr_start Earliest year for which to pull data
#' @param yr_end Most recent year for which to pull data
#' @param stn_ids Character combination of usaf and wban code in format "usaf-wban"
#' @param zip5 5-digit zip code if not including stn_id (can't include both)
#' @param temp_unit "F" for degrees Fahrenheit or "C" for Celsius
#'
#' @return data frame
#' @export
#'
#' @examples
#' # Use station_id:
#' stn_data <- stoolsr::isd_stns()
#' stns <- stn_data[stn_data$end>=as.Date("2020-01-01"),]
#' use_stns <- sample(stns$stn_id, 2)
#' weather_data <- get_isdlite(yr_start = 2019, yr_end = 2020, stn_ids = use_stns)
#'
#' # Or use zip codes directly which will automatically pull weather from
#' # nearest station to zip code centroid
#' weather_data <- get_isdlite(yr_start = 2019, yr_end = 2020, 
#'  zip5 = c("53715", "53235", "54255"))
get_isdlite <- function(yr_start,
                        yr_end,
                        stn_ids = NULL,
                        zip5 = NULL,
                        temp_unit = "F") {

  get_weather <- function(yr_start,
                          yr_end,
                          stn_ids,
                          temp_unit) {
    base_url <- "https://www.ncei.noaa.gov/pub/data/noaa/isd-lite/"
    (yr_urls <- paste0(base_url, yr_start:yr_end,"/"))
    stns_rep <- rep(stn_ids, each=length(yr_urls))
    (id_string <- paste0(paste(stns_rep, yr_start:yr_end, sep="-"),".gz"))
    (data_urls <- paste0(yr_urls, id_string))
    names(data_urls) <- stns_rep

    dlist <- lapply(data_urls, stoolsr::read_isdlite)

    decivars <- c("airtemp", "dptemp", "windspeed")

    dlist <- lapply(dlist, function(x) dplyr::mutate_at(x, decivars, ~.x/10))

    if(temp_unit=="F") {
      CtoF <- function(x) x*(9/5) + 32
      dlist <- lapply(dlist, function(x) dplyr::mutate_at(x, decivars[1:2], CtoF))
    }

    dlist <- purrr::map2(.x = dlist, .y = names(dlist), ~dplyr::mutate(.x, stn_id = .y))
    df <- dplyr::bind_rows(dlist)
    df <- dplyr::select(df, stn_id, dplyr::everything())

    return(df)

  }

  if(is.null(stn_ids) & is.null(zip5)) {

    stop("You must supply either a zip code or station id.")

  }

  if(!is.null(stn_ids) & !is.null(zip5)) {

    stop("Supply either stn_ids OR zip5 arguments, NOT both.")

  } else if(!is.null(zip5)) {

    end_date <- lubridate::make_date(yr_end, 1, 1)
    stns <- zip_to_stn(zip5 = zip5, end_date = end_date)
    stn_ids <- stns$stn_id[stns$match_status == "Success"]

    if(length(stn_ids) == 0) {
      print(stns)
      stop("No zip codes matched weather stations.")
    } else {

      df <- get_weather(yr_start = yr_start,
                        yr_end = yr_end,
                        stn_ids = stn_ids,
                        temp_unit = temp_unit)

      df <- dplyr::left_join(stns, df, by = "stn_id")
    }

  } else {

    df <- get_weather(yr_start = yr_start,
                      yr_end = yr_end,
                      stn_ids = stn_ids,
                      temp_unit = temp_unit)

  }

  return(df)

}


#' Get TMY Data for Lat/Lon from NREL NSRDB API
#'
#' @param lat numeric latitude for site
#' @param lon numeric longitude for site
#' @param api_key NREL API key (see:  https://nsrdb.nrel.gov/about/tmy.html)
#'
#' @return list containing 1) TMY data as data frame and 2) metadata as named list
#' @export
#'
#' @examples
#' stn_data <- isd_stns()
#' stns <- stn_data[stn_data$end>=as.Date("2020-01-01"),]
#' stn <- stns[sample(nrow(stns),1),]
#' 
#' "tmy <- get_tmy(stn$lat, stn$lon, api_key = mykey)"


get_tmy <- function(lat, lon, api_key = NULL){
  
  if(is.null(api_key)){
    stop("This function requires a valid api key from https://nsrdb.nrel.gov/about/tmy.html")
  }
  # https://nsrdb.nrel.gov/about/tmy.html

  # You must request an NSRDB api key from the link above
  #api_key = 'hn7pzescl5xtiibLsBQEP7KYpSJKebG7YZUOIGL5'
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
  myname = 'Slipstream+Analyst'
  # Your reason for using the NSRDB.
  reason = 'evaluation'
  # Your affiliation
  affiliation = 'SlipstreamINC'
  # Your email address
  email = 'research@slipstreaminc.org'
  # Please join our mailing list so we can keep you up-to-date on new developments.
  mailing_list = 'true'

  url_call <- paste0("https://developer.nrel.gov/api/solar/nsrdb_psm3_download.csv?wkt=POINT(",lon,
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


  meta <- readr::read_lines(url_call, n_max = 2)
  mlist <- lapply(meta, strsplit,",")

  meta_data <- as.list(mlist[[2]][[1]])
  names(meta_data) <- mlist[[1]][[1]]



  data_df <- readr::read_csv(url_call, skip=2, col_types = "dddddddd")

  res <- list(tmy_data = data_df, meta_data = meta_data)
  return(res)
  }


#' Find Nearest NOAA ISD Station for a Zip Code
#'
#' @param zip5 vector of valid 5-digit zip codes
#' @param states optional character vector of states. Defaults to zip5 state.
#' @param begin_date optional begin date for needed weather data (YYYY-MM-DD)
#' @param end_date optional end date for needed weather data (YYYY-MM-DD)
#' @param max_dist maximum allowable distance for matching a station
#' @param return_all return all stations rather than only nearest
#' @param exclude_ids station_ids (use isd_stns() to get ids) to exclude from matching
#'
#' @return data frame
#' @export
#'
#' @examples
#' zip_to_stn(zip5 = c("53715", "53235", "54255"), states = "WI", end_date = Sys.Date()-2)
#'
zip_to_stn <- function (zip5, states = NULL, begin_date = NULL, end_date = NULL,
                        max_dist = 100, return_all = FALSE, exclude_ids = NULL
) {
  zip5 <- as.character(zip5)
  zip_df_subset <- zipcodeR::zip_code_db[, c("zipcode", "state",
                                             "lat", "lng")]

  res <- vector(mode = "list", length = length(zip5))

  for(i in seq_along(res)) {
    zc <- zip5[i]

    zip_df <- zip_df_subset[zip_df_subset$zipcode == zc, ]

    if(any(!zc %in% zip_df$zipcode, is.na(zip_df$lat), is.na(zip_df$lng))){
      match_df <- data.frame(
        zipcode = zc, stn_id = NA, usaf = NA,
        wban = NA, state = NA,
        lat_stn = NA, lon_stn = NA,
        begin = NA, end = NA,
        miles_to_stn = NA,
        match_status = "Failure: No matching zip code in zip code database"
      )

      res[[i]] <- match_df

    } else {
      names(zip_df)[3:4] <- c("lat_zip", "lon_zip")
      if (is.null(states)) {
        states <- zip_df$state
      }
      zip_df$state <- NULL

      isd_stns <- isd_stns()
      stns <- isd_stns[isd_stns$state %in%
                         states, ]
      if (!is.null(begin_date)) {
        begin_date <- as.character(begin_date)
        stns <- stns[stns$begin <= as.Date(begin_date), ]
      }
      if (!is.null(end_date)) {
        end_date <- as.character(end_date)
        stns <- stns[stns$end >= as.Date(end_date), ]
      }

      if(!is.null(exclude_ids)){
        stns <- stns[!stns$stn_id %in% exclude_ids,]
      }

      stns <- stns[, c("stn_id", "usaf", "wban",
                       "state", "lat", "lon", "begin",
                       "end")]
      names(stns)[5:6] <- c("lat_stn", "lon_stn")

      match_df <- fuzzyjoin::geo_left_join(zip_df, stns,
                                           by = c(lat_zip = "lat_stn",
                                                  lon_zip = "lon_stn"),
                                           unit = "miles",
                                           distance_col = "miles_to_stn",
                                           max_dist = max_dist)
      if (all(is.na(match_df$miles_to_stn))) {
        match_df$match_status <- "Failure: No stations found within the max_dist arg. Try increasing max_dist"
      } else {

        match_df$match_status <- "Success"
      }
      if (!return_all) {
        match_df <- match_df[match_df$miles_to_stn == min(match_df$miles_to_stn),
        ]
      }
      match_df$lat_zip <- NULL
      match_df$lon_zip <- NULL

      res[[i]] <- match_df


    }



  }

  res <- dplyr:: bind_rows(res)
  return(res)
}
