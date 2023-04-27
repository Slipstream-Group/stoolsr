#' Add range of HDD variables to data frame
#' @description Calculates the a range of heating degree day (HDD) values
#'     for a outdoor temperature value across a range of heating reference
#'     temperatures. The values for each heating reference temperature are
#'     added as new variables to data frame.
#' @param df Data frame with a series of temperature readings.
#' @param tempvar_name column name of temperature reading variable.
#' @param min_href Minimum heating reference temperature to calculate
#' @param max_href Maximum heating reference temperature to calculate
#' @param temp_step Interval by which to increment from \code{min_href}
#'     to \code{max_href}.
#' @return Data frame with series of variables representing heating degree day
#'     values calculated from range of heating reference temperatures from
#'     from \code{min_href} to \code{max_href} by \code{temp_step}.
#' @export
#'
#' @examples
#' data(nhtemp)
#' nhtemp_df = data.frame(tavg = nhtemp)
#' add_hdds(df = nhtemp_df, temp_step = 4)

add_hdds <- function(df,
                     tempvar_name = "tavg",
                     min_href = 40,
                     max_href = 80,
                     temp_step = 1) {
    hdd_range <- seq(from = min_href, to = max_href, by = temp_step)
    hdds <- vector("list", length = length(hdd_range))
    temp_f <- unlist(df[, tempvar_name])
    for (i in seq_along(hdd_range)) {
        t <- hdd_range[i]
        hdds[[i]] <- ifelse(temp_f < t, t - temp_f, 0)
    }
    names(hdds) <- paste0("hdd", hdd_range)
    hdds_df <- dplyr::bind_cols(df, hdds)
    return(hdds_df)
}


#' Add range of CDD variables to data frame
#' @description Calculates the a range of cooling degree day (CDD) values
#'     for a outdoor temperature value across a range of cooling reference
#'     temperatures. The values for each cooling reference temperature are
#'     added as new variables to data frame.
#' @param df Data frame with a series of temperature readings.
#' @param tempvar_name column name of temperature reading variable.
#' @param min_cref Minimum cooling reference temperature to calculate
#' @param max_cref Maximum cooling reference temperature to calculate
#' @param temp_step Interval by which to increment from \code{min_cref} to
#'     \code{max_cref}.
#' @return Data frame with series of variables representing cooling degree day
#'     values calculated from range of cooling reference temperatures from
#'     from \code{min_href} to \code{max_href} by \code{temp_step}.
#' @export
#'
#' @examples
#' data(nhtemp)
#' nhtemp_df = data.frame(tavg = nhtemp)
#' add_cdds(df = nhtemp_df, temp_step = 4)

add_cdds <- function(df,
                     tempvar_name = "tavg",
                     min_cref = 40,
                     max_cref = 80,
                     temp_step = 1) {
    cdd_range <- seq(from = min_cref, to = max_cref, by = 1)
    cdds <- vector("list", length = length(cdd_range))
    temp_f <- unlist(df[, tempvar_name])
    for (i in seq_along(cdd_range)) {
        t <- cdd_range[i]
        cdds[[i]] <- ifelse(temp_f > t, temp_f - t, 0)
    }
    names(cdds) <- paste0("cdd", cdd_range)
    cdds_df <- dplyr::bind_cols(df, cdds)
    return(cdds_df)
}


#' Winsorize vector
#' @description Sets extreme values to upper and lower quantile.
#' @param x Numeric vector.
#' @param lower lower quantile
#' @param upper upper quantile
#' @return Vector with extreme values set to quantile value. Missing
#'     values also removed.
#' @export
#'
#' @examples
#'  data(precip)
#' par(mfrow = c(2,1))
#' hist(precip)
#' hist(winsorize(precip))

winsorize = function(x, lower = 0.025, upper = 0.975) {
    x <- x[!is.na(x)]
    limits <- stats::quantile(x, probs = c(upper, lower))
    x[x <= min(limits)] <- min(limits)
    x[x >= max(limits)] <- max(limits)
    return(x)
}


#' Estimate heating reference temp
#' @description Fits series of linear models across a range of
#'     potential heating  reference temperatures. Returns
#'     temperature with the largest r-squared value or liklihood
#'     score based on a custom loss function.
#' @param x Data frame with series of variables representing range of
#'     HDD values for series of billing periods.
#' @param usage_var Name of usage variable as string.
#' @param hdd_prefix something like "HDD" which is used to select
#'     range of variables for fitting models. Assumes series of variables with
#'     names like "HDD40", "HDD41"..."HDD80".
#' @param calc_loss Use built-in function to calculate liklikhood score. The
#'     default is to select the reference temp resulting in largest r-squared
#'     value.
#' @param prior_mean Estimated mean reference temperature of population.
#' @param prior_sd Estimated standard deviation of the reference temperature
#'     in the population.
#' @return Named character vector with estimated reference temp and r-squared
#'     value for the linear model.
#' @export
#'
#' @examples
#' To be added... Need sample data

get_href <- function(x, usage_var = "usage", hdd_prefix = "hdd",
                     calc_loss = FALSE, prior_mean = NULL, prior_sd = NULL) {
    (dd_names = names(x)[str_detect(names(x), hdd_prefix)])
    (dds = as.integer(str_extract(dd_names, "[:digit:]{2}")))
    dd_rsq <- vector("double", length(dd_names))
    if(calc_loss){
        n <- nrow(x)
        densities <- stats::dnorm(x = dds, mean = prior_mean, sd = prior_sd) / stats::dnorm(x = prior_mean, mean = prior_mean, sd = prior_sd)
        scores <- vector("double", length(dd_names))
    }
    for(i in seq_along(dd_names)) {
        rsq <- summary(
            stats::lm(x[[usage_var]] ~ x[[dd_names[i]]])
        )$adj.r.squared
        dd_rsq[[i]] <- rsq
        if(calc_loss) {
            d <- densities[i]
            scores[i] <- (1-rsq)^(-(n)/2) * d
        }
    }
    if(calc_loss){
        max_index = which.max(scores)
        ref_temp = dds[max_index]

    } else {
        max_index <- which.max(dd_rsq)
        ref_temp <- dds[max_index]
    }
    rsq <- dd_rsq[max_index]
    results <- c(href = ref_temp, r_squared = rsq)
    return(results)
}

#' Estimate cooling reference temp
#' @description Fits series of linear models across a range of
#'     potential cooling reference temperatures. Returns
#'     temperature with the largest r-squared value or liklihood
#'     score based on a custom loss function.
#' @param x Data frame with series of variables representing range of CDD
#'     values for series of billing periods.
#' @param usage_var Name of usage variable as string.
#' @param cdd_prefix something like "CDD" which is used to select
#'     range of variables for fitting models. Assumes series of variables with
#'     names like "CDD40", "CDD41"..."CDD80".
#' @param calc_loss Use built-in function to calculate liklikhood score. The
#'     default is to select the cooling reference temperature resulting in
#'     largest r-squared value.
#' @param prior_mean Estimated mean heating reference temperature of the
#'     population.
#' @param prior_sd Estimated standard deviation of the cooling reference
#'     temperature in the population.
#' @return Named character vector with estimated cooling reference temp
#'     and r-squared value for the best linear model.
#' @export
#'
#' @examples
#' To be added... Need sample data

get_cref <- function(x, usage_var = "usage", cdd_prefix = "cdd",
                     calc_loss = FALSE, prior_mean = NULL, prior_sd = NULL) {
    (dd_names = names(x)[stringr::str_detect(names(x), cdd_prefix)])
    (dds = as.integer(stringr::str_extract(dd_names, "[:digit:]{2}")))
    dd_rsq <- vector("double", length(dd_names))
    if(calc_loss){
        n <- nrow(x)
        densities <- stats::dnorm(x = dds, mean = prior_mean, sd = prior_sd) / stats::dnorm(x = prior_mean, mean = prior_mean, sd = prior_sd)
        scores <- vector("double", length(dd_names))
    }
    for(i in seq_along(dd_names)) {
        rsq <- summary(
            stats::lm(x[[usage_var]] ~ x[[dd_names[i]]])
        )$adj.r.squared
        dd_rsq[[i]] <- rsq
        if(calc_loss) {
            d <- densities[i]
            scores[i] <- (1-rsq)^(-(n)/2) * d
        }
    }
    if(calc_loss){
        max_index = which.max(scores)
        ref_temp = dds[max_index]

    } else {
        max_index <- which.max(dd_rsq)
        ref_temp <- dds[max_index]
    }
    rsq <- dd_rsq[max_index]
    results <- c(cref = ref_temp, r_squared = rsq)
    return(results)
}


#' Estimate reference temps with heating and cooling model
#' @description Fits series of linear models across a range of
#'     potential heating and cooling reference temperatures. Returns
#'     temperature with the largest r-squared value or liklihood
#'     score based on a custom loss function.
#' @param x Data frame with series of variables representing range of CDD and
#'     HDD values for series of billing periods.
#' @param usage_var Name of usage variable as string.
#' @param hdd_prefix typically either "CDD" or "HDD" which is used to select
#'     range of variables for fitting models. Assumes series of variables with
#'     names like "CDD40", "CDD41"..."CDD80".
#' @return Named character vector with estimated heating and cooling reference
#'     temps and r-squared value for the best linear model.
#' @export
#'
#' @examples
#' To be added... Need sample data

get_hcref <- function(x,
                      usage_var = "usage",
                      hdd_prefix = "hdd",
                      cdd_prefix = "cdd") {
    hdd_names = names(x)[stringr::str_detect(names(x), hdd_prefix)]
    cdd_names = names(x)[stringr::str_detect(names(x), cdd_prefix)]
    sample_grid <- expand.grid(
        hdd = as.integer(stringr::str_extract(hdd_names, "[:digit:]{2}")),
        cdd = as.integer(stringr::str_extract(cdd_names, "[:digit:]{2}")))
    sample_grid = dplyr::filter(sample_grid, hdd <= cdd)
    hdd = paste0(hdd_prefix,sample_grid$hdd)
    cdd = paste0(cdd_prefix, sample_grid$cdd)
    rsq = purrr::map2_dbl(.x = hdd,.y=cdd,
                              ~summary(stats::lm(x[[usage_var]] ~ x[[.x]] + x[[.y]]))$adj.r.squared)
    max_i <- which.max(rsq)
    results <- c(href = hdd[max_i],
                 cref = cdd[max_i],
                 r_squared = rsq[max_i])
    return(results)

}

