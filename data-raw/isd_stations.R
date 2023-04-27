
#' Read and format ISD station table from NOAA website
#'
#' @return data frame
#' @export
#'
#' @examples
#' stns <- get_stns()
#' stns <- get_stns(earliest_year = 2015, latest_year = 2020)
get_stns <- function(
  #earliest_year = NULL,
  #latest_year = NULL
  ){
  isd_hist_url <- "https://www1.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"
  stns <- readr::read_csv(isd_hist_url, col_types = "ccccccdddcc")

  # if(is.null(earliest_year)){
  #   earliest_year <- lubridate::year(Sys.Date())-1
  #   cat("Keeping stations with data from year", earliest_year, "or earlier\n")
  # } else{
  #   cat("Keeping stations with data from year", earliest_year, "or earlier\n")
  # }
  #
  # if(is.null(latest_year)) {
  #   latest_year <- lubridate::year(Sys.Date())
  #   cat("Keeping only stations with most recent data in past year:", latest_year)
  # }else{
  #   cat("Keeping only stations with most recent data in:", latest_year, "or after")
  # }

  stns <- dplyr::filter(stns, !is.na(STATE), CTRY == "US")
  stns <- dplyr::rename_all(stns,tolower)
  stns <- dplyr::rename_all(stns, ~stringr::str_replace_all(., " ", "_"))
  stns <- dplyr::rename(stns, elev_m = `elev(m)`)
  stns <- dplyr::mutate_at(stns, c("begin", "end"), lubridate::ymd)
  #stns <- dplyr::filter(stns, lubridate::year(end)>=latest_year,
  #                      lubridate::year(begin)<=earliest_year)
  stns <- dplyr::mutate(stns, stn_id = paste(usaf,wban, sep = "-"))
  stns <- dplyr::select(stns, stn_id, dplyr::everything())

  return(stns)
}

isd_stns <- get_stns()
save(isd_stns, file = "data/isd_stns.RData")
