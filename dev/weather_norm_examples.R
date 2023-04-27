.libPaths("S:\\R-Shared\\packagelib - r4 - current")

library(illumer2)

data("monthly_gas")
sample_id <- sample(monthly_gas$dummy_id, 1)
df <- dplyr::filter(monthly_gas, dummy_id == sample_id)
zip5 <- unique(df$zip5)
begin <- min(df$start_date)
end <- max(df$end_date)
stn_lu <- zip_to_stn(zip5, begin_date = begin, end_date = end)
hourly_weather <- get_isdlite(yr_start = lubridate::year(begin),
                           yr_end = lubridate::year(end),
                           stn_ids = stn_lu$stn_id)
hourly_weather <- dplyr::group_by(hourly_weather, stn_id, yr, mon, dy)
daily_weather <- dplyr::summarize_at(hourly_weather, "airtemp", mean)

daily_weather <- add_hdds(daily_weather, tempvar_name = "airtemp",
         min_href = 40,
         max_href = 80,
          temp_step = 2)



