#' Meta Data
#'
#' @param class string (optional) specify class or classes to get metadata for
#' @param un string cartegraph username
#' @param pw string cartegraph password
#' @param org orginization API ID ie 'PittsburghPA'
#' @param base_url API Base URL (defaulted to "https://cgweb06.cartegraphoms.com/")
#'
#' @return list
#'
#'  @examples \dontrun{
#'  # Entire System Meta Data
#'  cgMeta()
#'  # Meta Data for two classes
#'  cgMeta(c("cgFacilitiesClass", "cgPavementClass"))
#'  }
#' @export
cgMeta <- function(class = "", un, pw, org, base_url = "https://cgweb06.cartegraphoms.com/") {
  if (is.list(class)) {
    classes <- paste(class, collpase = ",")
    url <- paste0(base_url, org, "/api/v1/meta/Classes?classNames=", classes)
  } else {
    url <- paste0(base_url, org, "/api/v1/meta/Classes/", class)
  }
  g <- httr::GET(url, httr::authenticate(un, pw, type = "basic"))

  c <- httr::content(g, "text")

  j <- jsonlite::fromJSON(c)

  if (httr::http_error(g)) {
    stop(j$Message)
  }

  return(j)
}
