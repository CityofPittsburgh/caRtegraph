
#' Put Data in Cartegraph
#'
#' @param class cartegraph class or class and attachment ie 'cgSigns_cgAttachmentsClass'
#' @param body DataFrame/tibble to be turned into JSON object
#' @param un api username
#' @param pw api password
#' @param org orginization API ID ie 'PittsburghPA'
#'
#' @return
#' @export
#'
#' @examples
cgPut <- function(class, body, un, pw, org) {
  json <- jsonlite::toJSON(body)

  url <- paste0("https://cgweb06.cartegraphoms.com/", org, "/api/v1/classes/", class)

  httr::PUT(url, httr::authenticate(un, pw, type = "basic"), body = json)
}

#' Title
#'
#' @param class cartegraph class or class and attachment ie 'cgSigns_cgAttachmentsClass'
#' @param oid Oid of the object you wish to delete
#' @param un api username
#' @param pw api password
#' @param org orginization API ID ie 'PittsburghPA'
#'
#' @return
#' @export
#'
#' @examples
cgDelete <- function(class, oid, un, pw, org) {
  url <- paste0("https://cgweb06.cartegraphoms.com/", org, "/api/v1/classes/", class, "/", oid)

  D <- httr::DELETE(url, httr::authenticate(un, pw, type = "basic"))

  print(httr::content(D))
}
