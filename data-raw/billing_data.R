library(dplyr)
library(purrr)

load("data-raw/sample_billing_data.rdata")

sample_data <- sample_data %>%
  dplyr::arrange(unit, dummy_id, start_date) %>%
  dplyr::mutate(bp_days = as.integer(end_date-start_date))

sd_split <- split(sample_data, sample_data$unit)

# 1) Keep only positive consumption & remove any missing values
#    Allow 0 readings for gas (ala Caltrack)
sd_split <- purrr::map(sd_split, ~filter(.x, usage>0 & unit == "KWh" | usage >= 0 & unit == "Therms",
                        !is.na(usage)))
purrr::map(sd_split, ~summary(.x$usage))


# 2) remove true duplicate (identical) reads with same dates and values
sd_split <- purrr::map(sd_split, distinct)


# 3) drop multiple reads for same account and start_date
# flag them
# TODO: determine if these should be aggregated
sd_split<- purrr::map(sd_split,
           ~dplyr::group_by(.x, dummy_id, start_date)%>%
             dplyr::mutate(nreads=n())%>%
             dplyr::ungroup()%>%
             dplyr::filter(nreads==1)%>%
             dplyr::select(-nreads))


# 4) Remove unusally short of long billing periods
# set thresholds
# TODO: check thresholds
purrr::map(sd_split, ~summary(.x$bp_days))
bpdays_min <- 20
bpdays_max <- 40

sd_split <- purrr::map(sd_split, ~dplyr::filter(.x, between(bp_days, bpdays_min, bpdays_max)))


# Extreme values - remove if over 0.99 quantile
(upr99usage <- purrr::map(sd_split, ~quantile(.x$usage, .99)))
sd_split <- purrr::map2(.x = sd_split, .y = upr99usage, ~dplyr::filter(.x, usage < .y))

reads_count <- purrr::map(sd_split, ~dplyr::count(.x, dummy_id))
#reads_count$KWh %>% View
#reads_count$Therms %>% View

sample_data_cleaned <- dplyr::bind_rows(sd_split)

# accounts dropped
dplyr::n_distinct(sample_data$dummy_id)
dplyr::n_distinct(sample_data$dummy_id) - dplyr::n_distinct(sample_data_cleaned$dummy_id)
# attrition % by row
(nrow(sample_data)-nrow(sample_data_cleaned))/nrow(sample_data)

monthly_elec <- sample_data_cleaned %>% filter(unit=="KWh") %>% select(-unit)
monthly_gas <- sample_data_cleaned %>% filter(unit == "Therms") %>% select(-unit)

save(monthly_elec, file = "data/monthly_elec.RData")
save(monthly_gas, file = "data/monthly_gas.RData")
