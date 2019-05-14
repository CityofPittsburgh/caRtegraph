
#' Put Data in Cartegraph
#'
#' @param class cartegraph class or class and attachment ie 'cgSigns_cgAttachmentsClass', must inlcude the Oid column
#' @param body DataFrame/tibble to be turned into JSON object
#' @param un api username
#' @param pw api password
#' @param org orginization API ID ie 'PittsburghPA'
#'
#' @return http status
#' @export
#'
#' @examples \dontrun{
#' df <- data.frame(Oid = 242283165,
#'      AddressNumberField = 4771,
#'      StreetField = '123 Main Street')
#'
#' cgPut("cgSignsClass", df, "fakeUN", "fakePW", "AnyTownUSA")
#' }
cgPut <- function(class, body, un, pw, org) {
  if ("Oid" %in% colnames(body)) {
    payload <- NULL
    payload[[class]] <- body

    json <- jsonlite::toJSON(payload)

    url <- paste0("https://cgweb06.cartegraphoms.com/", org, "/api/v1/classes/", class)

    P <- httr::PUT(url, httr::authenticate(un, pw, type = "basic"), body = json)
    print(httr::content(P))
  } else {
    stop("No Oid's included. API Put requests without Oid will not work")
  }
}

#' Delete
#'
#' @param class cartegraph class or class and attachment ie 'cgSigns_cgAttachmentsClass'
#' @param oid Oid of the object you wish to delete
#' @param un api username
#' @param pw api password
#' @param org orginization API ID ie 'PittsburghPA'
#'
#' @return http status
#' @export
#'
#' @examples \dontrun{
#' cgDelete("cgSignsClass", "123", "fakePW", "AnyTownUSA")
#' }
cgDelete <- function(class, oid, un, pw, org) {
  url <- paste0("https://cgweb06.cartegraphoms.com/", org, "/api/v1/classes/", class, "/", oid)

  D <- httr::DELETE(url, httr::authenticate(un, pw, type = "basic"))

  print(httr::content(D))
}

#' Create new record(s) in Cartegraph
#'
#' @param class cartegraph class or class and attachment ie 'cgSigns_cgAttachmentsClass'
#' @param body DataFrame/tibble to be turned into JSON object
#' @param un api username
#' @param pw api password
#' @param org orginization API ID ie 'PittsburghPA'
#'
#' @return http status
#' @export
#'
#' @examples \dontrun{
#' df <-data.frame(IDField = "Sign-50",
#'      AddressNumberField = 1765)
#'
#' cgPOST("cgSignsClass", df,"fakeUN", "fakePW", "AnyTownUSA")
#' }
cgPost <- function(class, body, un, pw, org) {
  payload <- NULL
  payload[[class]] <- body

  json <- jsonlite::toJSON(payload)

  url <- paste0("https://cgweb06.cartegraphoms.com/", org, "/api/v1/classes/", class)

  P <- httr::POST(url, httr::authenticate(un, pw, type = "basic"), body = json)
  print(httr::content(P))
}
