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
#' @export
#'
#' @examples \dontrun{
#' df <- data.frame(Oid = 242283165,
#'      AddressNumberField = 4771,
#'      StreetField = '123 Main Street')
#'
#' cgPut("cgSignsClass", df, "fakeUN", "fakePW", "AnyTownUSA")
#' }
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
    print(P$status)
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

  print(D$status)
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
#' @export
#'
#' @examples \dontrun{
#' df <- data.frame(IDField = "Sign-50",
#'      AddressNumberField = 1765)
#'
#' cgPOST("cgSignsClass", df,"fakeUN", "fakePW", "AnyTownUSA")
#' }
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
  print(P$status)
}

#' **UNTESTED** Process a Spatial Shape to pass to Cartegraph API through POST or PUT
#'
#' @param shape Spatial DataFrame to be turned into nested CgShape column
#'
#' @return tibble with cgShape Column
#' @export
#'
#' @examples
cgShapeProcess <- function(shape) {
  if (grepl("Spatial", class(shape))) {
    df <- shape@data
  } else {
    stop("Please submit a Spatial (sp) Object to be processed")
  }
  # Process Polygons
  if (class(shape)[1] == "SpatialPolygonsDataFrame") {
    for (i in 1:length(shape@polygons)) {
      temp <- shape@polygons[[i]]@Polygons
      centroid <- shape@polygons[[i]]@Polygons[[1]]@labpt
      Center <- as.data.frame(cbind(Lat = centroid[2], Lng = centroid[1]))
      CgShape <- NULL
      if (length(temp) == 1) {
        Points <- subset(as.data.frame(shape@polygons[[i]]@Polygons[[1]]@coords), select = c(V2, V1))
        colnames(Points) <- c("Lat", "Lng")
        Breaks <- list()
      } else {
        Breaks <- list(0)
        Points <- data.frame()
        for (x in 1:length(temp)) {
          Points <- rbind(Points, subset(as.data.frame(shape@polygons[[i]]@Polygons[[x]]@coords), select = c(V2, V1)))
          colnames(Points) <- c("Lat", "Lng")
          Breaks <- append(Breaks, nrow(Points))
        }
      }
      CgShape$Points <- Points
      CgShape$Center <- Center
      CgShape$Breaks <- Breaks
      CgShape$ShapeType <- 3

      temp_df <- df[i,]
      temp_df$CgShape <- list(CgShape)
      # RBind
      if (i == 1) {
        final <- temp_df
      } else {
        final <- rbind(final, temp_df)
      }
    }
  # Process Points
  } else if (class(shape)[1] == "SpatialPointsDataFrame") {
    coords <- as.data.frame(sp::coordinates(shape))
    for (i in 1:nrow(coords)) {
      CgShape <- NULL
      Points <- subset(coords[i,], select = c(V2, V1))
      colnames(Points) <- c("Lat", "Lng")

      CgShape$ShapeType <- 1
      CgShape$Breaks <- unlist(list())
      CgShape$Center <- Points
      CgShape$Points <- Points

      temp_df <- df[i,]
      temp_df$CgShape <- list(CgShape)
      # RBind
      if (i == 1) {
        final <- temp_df
      } else {
        final <- rbind(final, temp_df)
      }
    }
  # Process Lines
  } else if (class(shape)[1] == "SpatialLinesDataFrame") {
    centroids <- rgeos::gCentroid(shape, byid = TRUE)@coords
    for (i in 1:nrow(df)) {
      CgShape <- NULL
      CgShape$ShapeType <- 2
      Points <- subset(shape@lines[[i]]@Lines[[1]]@coords, select = c(V2, V1))
      colnames(Points) <- c("Lat", "Lng")
      Center <- data.frame(Lat = centroids[i,2], Lng = centroids[i,1])
      CgShape$Points <- Points
      CgShape$Center <- Center

      temp_df <- df[i,]
      temp_df$CgShape <- list(CgShape)
      # RBind
      if (i == 1) {
        final <- temp_df
      } else {
        final <- rbind(final, temp_df)
      }
    }
  }
  return(final)
}
