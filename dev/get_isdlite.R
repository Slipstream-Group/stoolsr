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

    dlist <- lapply(data_urls, illumer2::read_isdlite)

    decivars <- c("airtemp", "dptemp", "windspeed")

    dlist <- lapply(dlist, function(x) dplyr::mutate_at(x, decivars, ~.x/10))

    if(temp_unit=="F") {
      CtoF <- function(x) x*(9/5) + 32
      dlist <- lapply(dlist, function(x) dplyr::mutate_at(x, decivars[1:2], CtoF))
    }

    dlist <- purrr::map2(.x = dlist, .y = names(dlist), ~dplyr::mutate(.x, stn_id = .y))
    df <- dplyr::bind_rows(dlist)
    df <- dplyr::select(df, stn_id, everything())

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





x <- get_isdlite(2020, 2022, zip5 = c("53715", "53235", "54255"))
x


