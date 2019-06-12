#' Update records in Cartegraph
#'
#' @param class cartegraph class or class and attachment ie 'cgSigns_cgAttachmentsClass', must inlcude the Oid column
#' @param body DataFrame/tibble to be turned into JSON object
#' @param un api username
#' @param pw api password
#' @param org orginization API ID ie 'PittsburghPA'
#' @param base_url API Base URL (defaulted to "https://cgweb06.cartegraphoms.com/")
#'
#' @return http status
#'
#' @examples \dontrun{
#' df <- data.frame(Oid = 242283165,
#'      AddressNumberField = 4771,
#'      StreetField = '123 Main Street')
#'
#' cgPut("cgSignsClass", df, "fakeUN", "fakePW", "AnyTownUSA")
#' }
#' @export
cgPut <- function(class, body, un, pw, org, base_url = "https://cgweb06.cartegraphoms.com/") {
  if ("Oid" %in% colnames(body)) {
    payload <- NULL
    if (grepl("Spatial", class(body))) {
      payload[[class]] <- cgShapeProcess(body)
    } else {
      payload[[class]] <- body
    }
    json <- jsonlite::toJSON(payload)

    url <- paste0(base_url, org, "/api/v1/classes/", class)

    P <- httr::PUT(url, httr::authenticate(un, pw, type = "basic"), body = json)
    if (P$status == 200) {
      print("Success")
    } else {
      stop(httr::content(P))
    }
  } else {
    stop("No Oid's included. API Put requests without Oid will not work")
  }
}

#' Delete a record in Cartegraph
#'
#' @param class cartegraph class or class and attachment ie 'cgSigns_cgAttachmentsClass'
#' @param oid Oid of the object you wish to delete
#' @param un api username
#' @param pw api password
#' @param org orginization API ID ie 'PittsburghPA'
#' @param base_url API Base URL (defaulted to "https://cgweb06.cartegraphoms.com/")
#'
#' @return http status
#' @export
#'
#' @examples \dontrun{
#' cgDelete("cgSignsClass", "123", "fakePW", "AnyTownUSA")
#' }
cgDelete <- function(class, oid, un, pw, org, base_url = "https://cgweb06.cartegraphoms.com/") {
  url <- paste0(base_url, org, "/api/v1/classes/", class, "/", oid)

  D <- httr::DELETE(url, httr::authenticate(un, pw, type = "basic"))

  if (D$status == 200) {
    print("Success")
  } else {
    stop(httr::content(D))
  }
}

#' Create new record(s) in Cartegraph
#'
#' @param class cartegraph class or class and attachment ie 'cgSigns_cgAttachmentsClass'
#' @param body DataFrame/tibble to be turned into JSON object
#' @param un api username
#' @param pw api password
#' @param org orginization API ID ie 'PittsburghPA'
#' @param base_url API Base URL (defaulted to "https://cgweb06.cartegraphoms.com/")
#'
#' @return http status
#'
#' @examples \dontrun{
#' df <- data.frame(IDField = "Sign-50",
#'      AddressNumberField = 1765)
#'
#' cgPOST("cgSignsClass", df,"fakeUN", "fakePW", "AnyTownUSA")
#' }
#' @export
cgPost <- function(class, body, un, pw, org, base_url = "https://cgweb06.cartegraphoms.com/") {
  payload <- NULL
  if (grepl("Spatial", class(body))) {
    payload[[class]] <- cgShapeProcess(body)
  } else {
    payload[[class]] <- body
  }
  json <- jsonlite::toJSON(payload, auto_unbox = TRUE, digits = 15)

  url <- paste0(base_url, org, "/api/v1/classes/", class)

  P <- httr::POST(url, httr::authenticate(un, pw, type = "basic"), body = json)

  if (P$status == 200) {
    print("Success")
  } else {
    stop(httr::content(P))
  }
}

#' **UNTESTED** Process a Spatial Shape to pass to Cartegraph API through POST or PUT. (Tested for Polygons, not tested for Lines or Points)
#'
#' @param shape Spatial DataFrame to be turned into nested CgShape column
#'
#' @return tibble with cgShape Column
#'
#' @examples
#' @export
cgShapeProcess <- function(shape) {
  if (grepl("Spatial", class(shape))) {
    df <- shape@data
  } else {
    stop("Please submit a Spatial (sp) Object to be processed")
  }
  final <- data.frame()
  # Process Polygons
  if (class(shape)[1] == "SpatialPolygonsDataFrame") {
    for (i in 1:length(shape@polygons)) {
      temp <- shape@polygons[[i]]@Polygons
      centroid <- shape@polygons[[i]]@Polygons[[1]]@labpt
      Center <- as.data.frame(cbind(Lat = centroid[2], Lng = centroid[1]))
      CgShape <- NULL
      if (length(temp) == 1) {
        Points <- subset(as.data.frame(shape@polygons[[i]]@Polygons[[1]]@coords))
        colnames(Points) <- c("Lat", "Lng")
        # Sort Points for API call

        CgShape$Points <- Points
        CgShape$Breaks <- list()
        CgShape$ShapeType <- 3
        CgShape$Center <- Center

        temp_df <- df[i,]
        temp_df$CgShape <- list(CgShape)
        # RBind
        final <- plyr::rbind.fill(final, temp_df)
      } else {
        warning("Your Spatial Object has rows with multiple polygons, they have been broken up and given a letter at the end of their IDField.")
        for (x in 1:length(temp)) {
          temp_df <- df[i,]
          temp_df$IDField <- paste(temp_df$IDField, toupper(letters[x]))

          Points <- subset(as.data.frame(shape@polygons[[i]]@Polygons[[x]]@coords))
          colnames(Points) <- c("Lat", "Lng")

          # Sort Points for API call
          CgShape$Points <- Points
          CgShape$Breaks <- list()
          CgShape$ShapeType <- 3
          CgShape$Center <- Center

          temp_df$CgShape <- list(CgShape)
          # RBind
          final <- plyr::rbind.fill(final, temp_df)
        }
      }
    }
  # Process Points
  } else if (class(shape)[1] == "SpatialPointsDataFrame") {
    coords <- as.data.frame(sp::coordinates(shape))
    for (i in 1:nrow(coords)) {
      CgShape <- NULL
      Points <- subset(coords[i,])
      colnames(Points) <- c("Lat", "Lng")

      CgShape$Points <- Points
      CgShape$Breaks <- list()
      CgShape$ShapeType <- 1
      CgShape$Center <- Points

      temp_df <- df[i,]
      temp_df$CgShape <- list(CgShape)
      # RBind
      final <- rbind(final, temp_df)
    }
  # Process Lines
  } else if (class(shape)[1] == "SpatialLinesDataFrame") {
    centroids <- rgeos::gCentroid(shape, byid = TRUE)@coords
    for (i in 1:nrow(df)) {
      CgShape <- NULL
      Points <- subset(data.frame(shape@lines[[i]]@Lines[[1]]@coords))
      colnames(Points) <- c("Lat", "Lng")

      CgShape$Points <- Points
      CgShape$Breaks <- list()
      CgShape$ShapeType <- 2
      CgShape$Center <- data.frame(Lat = centroids[i,2], Lng = centroids[i,1])

      temp_df <- df[i,]
      temp_df$CgShape <- list(CgShape)
      # RBind
      final <- rbind(final, temp_df)
    }
  }
  return(final)
}
