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
#' @param zero_dd_thresh optional value (0 to 1) setting threshold for minimum
#'        fraction of positive degree day values for candidate balance point
#'        temperatures.
#' @return Data frame with series of variables representing heating degree day
#'     values calculated from range of heating reference temperatures from
#'     from \code{min_href} to \code{max_href} by \code{temp_step}.
#' @export
#'
#' @examples
#' require(stoolsr)
#' data(nhtemp)
#' nhtemp_df = data.frame(tavg = nhtemp)
#' add_hdds(df = nhtemp_df, temp_step = 4)




add_hdds <- function(df,
                     tempvar_name = "tavg",
                     min_href = 40,
                     max_href = 80,
                     temp_step = 1,
                     zero_dd_thresh = NULL) {
    hdd_range <- seq(from = min_href, to = max_href, by = temp_step)
    hdds <- vector("list", length = length(hdd_range))
    names(hdds) <- paste0("hdd", hdd_range)
    temp_f <- df[[tempvar_name]]
    for (i in seq_along(hdd_range)) {
        t <- hdd_range[i]
        hdds[[i]] <- ifelse(temp_f < t, t - temp_f, 0)
    }
    if(!is.null(zero_dd_thresh)) {
      frac_over0 <- purrr::map_dbl(hdds, ~mean(.x > 0))
      drop_hrefs <- names(frac_over0)[frac_over0 <= zero_dd_thresh]
      keep_hrefs <- names(frac_over0)[frac_over0 >= zero_dd_thresh]
      hdds <- hdds[keep_hrefs]
      ndropped <- length(drop_hrefs)
      if(ndropped > 0) {
        cat("Note: Dropped ", ndropped, " balance point temps with ",
            100*zero_dd_thresh, "% or less non-zero degree day values.\n",
            sep ="")
      }
    }
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
#' @param zero_dd_thresh optional value (0 to 1) setting threshold for minimum
#'        fraction of positive degree day values for candidate balance point
#'        temperatures.
#' @return Data frame with series of variables representing cooling degree day
#'     values calculated from range of cooling reference temperatures from
#'     from \code{min_href} to \code{max_href} by \code{temp_step}.
#' @export
#'
#' @examples
#' data(nhtemp)
#' nhtemp_df = data.frame(tavg = nhtemp)
#' add_cdds(df = nhtemp_df, temp_step = 4, )

add_cdds <- function(df,
                     tempvar_name = "tavg",
                     min_cref = 40,
                     max_cref = 80,
                     temp_step = 1,
                     zero_dd_thresh = NULL) {
    cdd_range <- seq(from = min_cref, to = max_cref, by = 1)
    cdds <- vector("list", length = length(cdd_range))
    names(cdds) <- paste0("cdd", cdd_range)
    temp_f <- df[[tempvar_name]]
    for (i in seq_along(cdd_range)) {
        t <- cdd_range[i]
        cdds[[i]] <- ifelse(temp_f > t, temp_f - t, 0)
    }

    if(!is.null(zero_dd_thresh)) {
      frac_over0 <- purrr::map_dbl(cdds, ~mean(.x > 0))
      drop_crefs <- names(frac_over0)[frac_over0 <= zero_dd_thresh]
      keep_crefs <- names(frac_over0)[frac_over0 >= zero_dd_thresh]
      cdds <- cdds[keep_crefs]
      ndropped <- length(drop_crefs)
      if(ndropped > 0) {
        cat("Note: Dropped ", ndropped, " balance point temps with ",
            100*zero_dd_thresh, "% or less non-zero degree day values.\n",
            sep ="")
        }
      }

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
#' @param coef_screen exclude candidate models where one or more coefficients
#'        are negative.
#' @param detailed_results return summary object for best candidate model.
#'
#' @return Either named character vector with estimated heating and cooling
#'         reference temps and r-squared value for the best linear model or a
#'         model summary object for the best model if detailed_results selected.
#' @export
#'
#' @examples
#' "TODO: Add example with example monthly usage data."

get_href <- function(x, 
                     usage_var = "usage", 
                     hdd_prefix = "hdd",
                      coef_screen = FALSE,
                      detailed_results = FALSE,
                      calc_loss = FALSE, 
                     prior_mean = NULL, 
                     prior_sd = NULL) {
  (bp_vars <- names(x)[stringr::str_detect(names(x), hdd_prefix)])
  names(bp_vars) <- bp_vars
  (bps <- as.integer(stringr::str_extract(bp_vars, "[:digit:]{2}")))
  bps_df <- data.frame(bp_var = bp_vars,
                       bp_val = bps
  )


  if(calc_loss){
    if(any(is.null(c(prior_mean, prior_sd)))) {
      stop("You must provide priors for both mean and sd to calc_loss")
    }
    n <- nrow(x)
    densities <- stats::dnorm(x = bps, mean = prior_mean, sd = prior_sd) /
      stats::dnorm(x = prior_mean, mean = prior_mean, sd = prior_sd)
  }

  res <- purrr::map(bp_vars, ~summary(stats::lm(x[[usage_var]] ~ x[[.x]])))
  if(coef_screen) {
    (coefs <- purrr::map(res, ~.x$coefficients[,1]))
    (pos_coefs <- purrr::map_lgl(coefs, ~all(.x >= 0)))
    cat("Dropped ", sum(pos_coefs == FALSE), "of", length(res), "candidate models with one or more negative coefficients.\n")
    res <- res[pos_coefs]

    if(length(res) == 0) {
      results <- c(href = NA,
                   r_squared = NA)

      warning("No candidate models passed positive coefficients screen.")
      return(results)
    }

  }
  rsq <- purrr::map_dbl(res, ~.x$adj.r.squared)
  if(calc_loss) {
    scores <- purrr::map2_dbl(.x = rsq, .y = densities, ~(1-.x)^(-(n)/2) * .y)
    max_id <- names(scores)[which.max(scores)]
  } else {
    max_id <- names(rsq)[which.max(rsq)]
  }

  if(detailed_results) {
    results <- res[max_id]
    return(results)
  } else {
    ref_temp <- bps_df$bp_val[bps_df$bp_var == max_id]
    rsq <- rsq[max_id]
    names(rsq) <- NULL
    results <- c(href = ref_temp, r_squared = rsq)
    return(results)
  }

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
#' @param coef_screen exclude candidate models where one or more coefficients
#'        are negative.
#' @param detailed_results return summary object for best candidate model.
#'
#' @return Either named character vector with estimated heating and cooling
#'         reference temps and r-squared value for the best linear model or a
#'         model summary object for the best model if detailed_results selected.
#' @export
#'
#' @examples
#' cat("TODO: Add example with example monthly usage data.")


get_cref <- function(x, usage_var = "usage", cdd_prefix = "cdd",
                      coef_screen = FALSE,
                      detailed_results = FALSE,
                      calc_loss = FALSE, prior_mean = NULL, prior_sd = NULL) {
  (bp_vars <- names(x)[stringr::str_detect(names(x), cdd_prefix)])
  names(bp_vars) <- bp_vars
  (bps <- as.integer(stringr::str_extract(bp_vars, "[:digit:]{2}")))
  bps_df <- data.frame(bp_var = bp_vars,
                       bp_val = bps
  )


  if(calc_loss){
    if(any(is.null(c(prior_mean, prior_sd)))) {
      stop("You must provide priors for both mean and sd to calc_loss")
    }
    n <- nrow(x)
    densities <- stats::dnorm(x = bps, mean = prior_mean, sd = prior_sd) /
      stats::dnorm(x = prior_mean, mean = prior_mean, sd = prior_sd)
  }

  res <- purrr::map(bp_vars, ~summary(stats::lm(x[[usage_var]] ~ x[[.x]])))
  if(coef_screen) {
    (coefs <- purrr::map(res, ~.x$coefficients[,1]))
    (pos_coefs <- purrr::map_lgl(coefs, ~all(.x >= 0)))
    cat("Dropped ", sum(pos_coefs == FALSE), "of", length(res), "candidate models with one or more negative coefficients.\n")
    res <- res[pos_coefs]

    if(length(res) == 0) {
      results <- c(cref = NA,
                   r_squared = NA)

      warning("No candidate models passed positive coefficients screen.")
      return(results)
    }

  }
  rsq <- purrr::map_dbl(res, ~.x$adj.r.squared)
  if(calc_loss) {
    scores <- purrr::map2_dbl(.x = rsq, .y = densities, ~(1-.x)^(-(n)/2) * .y)
    max_id <- names(scores)[which.max(scores)]
  } else {
    max_id <- names(rsq)[which.max(rsq)]
  }

  if(detailed_results) {
    results <- res[max_id]
    return(results)
  } else {
    ref_temp <- bps_df$bp_val[bps_df$bp_var == max_id]
    rsq <- rsq[max_id]
    names(rsq) <- NULL
    results <- c(cref = ref_temp, r_squared = rsq)
    return(results)
  }

}


#' Estimate reference temps with heating and cooling model
#' @description Fits series of linear models across a range of
#'     potential heating and cooling reference temperatures. Returns
#'     temperature with the largest r-squared value or liklihood
#'     score based on a custom loss function.
#' @param x Data frame with series of variables representing range of CDD and
#'     HDD values for series of billing periods.
#' @param usage_var Name of usage variable as string.
#' @param hdd_prefix something like "HDD" which is used to select
#'     range of variables for fitting models. Assumes series of variables with
#'     names like "HDD40", "HDD41"..."HDD80".
#' @param cdd_prefix something like "CDD" which is used to select
#'     range of variables for fitting models. Assumes series of variables with
#'     names like "CDD40", "CDD41"..."CDD80".
#' @param coef_screen exclude candidate models where one or more coefficients
#'        are negative.
#' @param detailed_results return summary object for best candidate model.
#'
#' @return Either named character vector with estimated heating and cooling
#'         reference temps and r-squared value for the best linear model or a
#'         model summary object for the best model if detailed_results selected.
#' @export
#'
#' @examples
#' cat("TODO: Add example with example monthly usage data.")
#'
get_hcref <- function(x,
                       usage_var = "usage",
                       hdd_prefix = "hdd",
                       cdd_prefix = "cdd",
                       coef_screen = FALSE,
                       detailed_results = FALSE
) {
  hdd_names <- names(x)[stringr::str_detect(names(x), hdd_prefix)]
  cdd_names <- names(x)[stringr::str_detect(names(x), cdd_prefix)]
  sample_grid <- expand.grid(
    hdd = as.integer(stringr::str_extract(hdd_names, "[:digit:]{2}")),
    cdd = as.integer(stringr::str_extract(cdd_names, "[:digit:]{2}")))
  sample_grid <- dplyr::filter(sample_grid, hdd <= cdd)
  sample_grid$hdd_var <- paste0(hdd_prefix,sample_grid$hdd)
  sample_grid$cdd_var <- paste0(cdd_prefix, sample_grid$cdd)
  sample_grid$id <- paste0(sample_grid$hdd_var, sample_grid$cdd_var)
  res <- purrr::map2(.x = sample_grid$hdd_var,.y = sample_grid$cdd_var,
                     ~summary(stats::lm(x[[usage_var]] ~ x[[.x]] + x[[.y]])))
  names(res) <- sample_grid$id
  if(coef_screen) {
    (coefs <- purrr::map(res, ~.x$coefficients[,1]))
    (pos_coefs <- purrr::map_lgl(coefs, ~all(.x >= 0)))
    cat("Dropped ", sum(pos_coefs == FALSE), "of", length(res), "candidate models with one or more negative coefficients.\n")
    res <- res[pos_coefs]

    if(length(res) == 0) {
      results <- c(href = NA,
                   cref = NA,
                   r_squared = NA)

      warning("No candidate models passed positive coefficients screen.")
      return(results)
    }

  }
  rsq <- purrr::map_dbl(res, ~.x$adj.r.squared)
  max_id <- names(rsq)[which.max(rsq)]
  best_res <- res[max_id]
  best_grid <- sample_grid[sample_grid$id == max_id,]
  if(detailed_results) {
    results <- best_res
  } else {
    results <- c(href = best_grid$hdd,
                 cref = best_grid$cdd,
                 r_squared = rsq[max_id][[1]])
  }

  return(results)


}

