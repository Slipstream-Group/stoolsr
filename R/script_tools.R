#' Write a Script Header to Clipboard
#'
#' @return multi-line string write to clipboard
#' @export
#'
#' @examples
#' cpy_header()
cpy_header <- function () 
{
  utils::writeClipboard(
    paste0("# HEADER:    
           # PROJECT:    
           # DESCRIPTION    
           # R Version: ",  R.version.string, "    
           # Date: ", Sys.Date(), "    
           # PROGRAMMER: ",  toupper((Sys.getenv("RSTUDIO_USER_IDENTITY"))), 
           "\n # 
           # Section A: Setup -------------------------------------------------------------"))
}

#' Write a Section Break to Clipboard
#'
#' @return string
#' @export
#'
#' @examples
#' cpy_section()
cpy_section <- function(){
  utils::writeClipboard(
    "# Section X: Title -------------------------------------------------------------"
  )
}


