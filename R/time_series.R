
#' Add unique ids to runs of TRUE values in binary vector
#'
#' @param x a binary vector.
#' @param id_prefix string.
#' @param no_id_tag label for FALSE condition of binary
#'
#' @return A vector with unique ids for each run of TRUE values \code{x}.
#' @export
#'
#' @examples
#' x = rbinom(20, 1, prob = 0.6)
#' ids = gen_flag_ids(x)
#' data.frame(flag = x, id = ids)
gen_flag_ids <- function(x, id_prefix = "x", no_id_tag = NA) {
  ids <- vector(mode="character", length = length(x))
  i=1
  j = 0
  while(i <=length(x)) {
    if (x[i] == FALSE | is.na(x[i])) {
       ids[i] <- no_id_tag
      i=i+1
    } else {
      j = j + 1
      id <-  paste0(id_prefix, j)
      ids[i] <- id
      while(x[i] == TRUE & !is.na(x[i]) & i <= length(x)) {
        ids[i] <- id
        i = i + 1
      }
    }
  }
  return(ids)
}
