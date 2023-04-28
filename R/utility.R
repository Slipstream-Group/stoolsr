# function to create key which is random combo of
# numbers and letters

#' Create a single alpha-numeric ID
#'
#' @param len Number of characters to include in the ID.
#' @param inc_letters Whether to include lower-case letters.
#'
#' @return A string
#' @export
#'
#' @examples
#' make_id()
#' make_id(len = 8, inc_letters = FALSE)
#'
make_id <- function(len = 4, inc_letters = TRUE){
  if(inc_letters){
    strng <- stringr::str_c(sample(c(letters,0:9), len, replace = TRUE), collapse = "")
  } else{
    strng <- stringr::str_c(sample(0:9, len, replace = TRUE), collapse = "")
  }

  return(strng)
}

#' Creates a vector of alpha-numeric IDs
#'
#' @param size Number of IDs to create.
#' @param unique_only Whether to allow duplicate IDs.
#' @param len Number of characters to include in the ID.
#' @param inc_letters Whether to include lower-case letters.
#'
#' @return Character vector
#' @export
#'
#' @examples
#'vmake_id()
#'set.seed(1984)
#'unique_ids <- vmake_id(size = 1e4, unique_only = TRUE, len = 4)
#'sum(duplicated(unique_ids))
#'non_unique_ids <-vmake_id(size = 1e4, unique_only = FALSE, len = 4)
#' sum(duplicated(non_unique_ids))
vmake_id <- function(size=10, unique_only=TRUE,len=4, inc_letters=TRUE){
  # call replicated on the make_id function to get the initial vector of
  # IDs:
  res <- replicate(size, make_id(len=len, inc_letters = inc_letters))
  if(unique_only){
    # check that getting enough unique permutations is possible:
    if(inc_letters){
      max_perm = 36^len
    } else{
      max_perm = 10^len
    }
    # Throw error if not possible.
    if(size>max_perm) {
      stop("It is not possible to generate that many unique permutations with so few characters.")
    }

    # ID indices of duplicated IDs then loop through them until there are no more
    # duplicated IDs.
    dups_i <- which(duplicated(res))
    while(length(dups_i>0)){
      n_replace <- length(dups_i)
      cat("Replacing", n_replace, "duplicated IDs...\n")
      res[dups_i] <- replicate(length(dups_i), make_id(len=len, inc_letters = inc_letters))
      dups_i <- which(duplicated(res))
    }

  }

  return(res)
}


#' Write dataframe to the clipboard for pasting in Excel
#' @param x Data frame
#' @param row.names Whether to include row names (default=FALSE)
#' @param col.names Whether to include column names (default = TRUE)
#' @param ... Additional arguments to pass to the write.table function
#'
#' @return Tab-separated table
#' @export
#'
#' @examples
#' df <- data.frame(x = rnorm(10), y = sample(letters, 10))
#' cpy_excel(df)  # next paste in Excel
cpy_excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  utils::write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}




#' Robust Standard Error
#'
#' @param model model object
#' @param cluster grouping variable such as account number or premise
#'
#' @return list of tables
#' @export
#'
#' @examples
#' "TODO: add example"
robust_se <- function(model, cluster){
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj <- apply(sandwich::estfun(model), 2, function(x) tapply(x, cluster, sum));
  rcse.cov <- dfc * sandwich::sandwich(model, meat = crossprod(uj)/N)
  rcse.se <- lmtest::coeftest(model, rcse.cov)
  return(list(rcse.cov=rcse.cov, rcse.se=rcse.se))
}


#' Valid Email Test
#'
#' @param x email string
#'
#' @return boolean
#' @export
#'
#' @examples
#' is_validemail("info@slipstreaminc.org")
#' is_validemail(c("info@slipstreaminc.org", "info@@slipstreaminc.org"))
is_validemail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
        as.character(x), ignore.case=TRUE)
}


#' Make File List
#' @description
#' Helper function for 
#' 
#' @param dir valid directory as string
#' @param pattern regex pattern string to return only file names with string match
#'
#' @return Named list where name is the file name and value is the full file path
#' @export
#'
#' @examples
#' # Ex. List files in current working dir
#' make_file_list(".")
#' 
#' # Ex. 2:  Return a list of some dummy .csv files with data
#' dfs <- lapply(rep(50, 20), function(x) data.frame(x= sample(LETTERS, x, replace = TRUE), y = rnorm(x)))
#' dp <- tempdir()
#' fps <- file.path(dp, paste0("file", 1:20, ".csv"))
#' for(i in 1:20) write.csv(dfs[[i]], fps[[i]])
#' make_file_list(dp, pattern = ".csv")
#' 
make_file_list <- function(dir, pattern = NULL) {
  flist <- as.list(list.files(dir, pattern = pattern, full.names = TRUE))
  names(flist) <- list.files(dir, pattern = pattern)
  return(flist)
}

