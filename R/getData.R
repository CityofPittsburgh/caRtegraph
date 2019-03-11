#' Return dataframe object rom CartegraphA PI
#'
#' @param class cartegraph class name
#' @param fields fields you want returned
#' @param filter optional filter
#' @param un api username
#' @param pw api password
#' @param org orginization API ID ie 'PittsburghPA'
#'
#' @return data.frame
#' @examples \dontrun{
#' pools <- cgDf("PoolsClass", "Oid,IDField,NeighborhoodField,PoolCapacityField,PoolTypeField,RetiredField,WaterSourceField", un = "fakeUn", pw = "fakePwd", org = "AnytownUSA")
#' }
#' @export
cgDf <- function(class, fields = "", filter = "", un, pw, org) {
  fields <- ifelse(is.list(fields), paste(fields, collapse = ","), fields)
  fields <- ifelse(fields == "", "", paste0("&fields=", fields))
  # Check for filter
  filter <- ifelse(filter == "", "", paste0("&filter=", filter))

  url <- paste0("https://cgweb06.cartegraphoms.com/", org, "/api/v1/classes/", class, "?limit=1000&offset=0", filter, fields)
  g <- httr::GET(url, httr::authenticate(un, pw, type = "basic"))
  content <- httr::content(g, as = "text")

  j <- jsonlite::fromJSON(content)
  if (httr::http_error(g)) {
    stop(j$Message)
  } else if (j$`_metadata`$totalCount == 0) {
    stop("No rows for this request")
  }

  total <- as.numeric(j$`_metadata`$totalCount)
  df_final <- data.frame(j[[class]])

  while(total > nrow(df_final)) {
    offset <- as.numeric(j$`_metadata`$offset + j$`_metadata`$limit)

    url <- paste0("https://cgweb06.cartegraphoms.com/", org, "/api/v1/classes/", class, "?fields=", fields, "&limit=1000&offset=", offset, filter)
    request <- httr::GET(url, httr::authenticate(un, pw, type = "basic"))
    content <- httr::content(request, as = "text")

    j <- jsonlite::fromJSON(content)

    df <- data.frame(j[class])

    df_final <- rbind(df_final, df)
  }
  return(df_final)
}

#' Return Points
#'
#' @param class cartegraph class name
#' @param fields fields you want returned
#' @param filter optional filter
#' @param un api username
#' @param pw api password
#' @param org orginization API ID ie 'PittsburghPA'
#'
#' @return SpatialPointsDataFrame
#'
#' @examples \dontrun{"
#' signs <- cgPoints(cgSignsClass",
#' c("MUTCDCodeField", "LocatorAddressNumberField", "PavementField", "LocatorStreetField", "LocatorCityField", "MountingFixtureField", "InstalledField"),
#' '(%5BMUTCDCode%5D%20in%20(%22R1-1%22,%20%22W3-D1%22))',
#' "fakeUn""fakePwd")
#' }
#' @export
cgPoints <- function(class, fields = "", filter = "", un, pw, org) {
  fields <- ifelse(is.list(fields), paste(fields, collapse = ","), fields)
  fields <- ifelse(fields == "", "", paste0("&fields=", fields, ",cgShape"))
  filter <- ifelse(filter == "", "", paste0("&filter=", filter))

  url <- paste0("https://cgweb06.cartegraphoms.com/", org, "/api/v1/classes/", class, "?limit=1000&offset=0", filter, fields)

  g <- httr::GET(url, httr::authenticate(un, pw, type = "basic"))
  load <- httr::content(g, as = "text")
  j <- jsonlite::fromJSON(load)

  if (httr::http_error(g)) {
    stop(j$Message)
  } else if (j$`_metadata`$totalCount == 0) {
    stop("No rows for this request")
  }

  df <- data.frame(j[class])
  total <- j$`_metadata`$totalCount

  # Clean Colnames
  colnames(df) <- gsub(paste0(class,"."), "",colnames(df))
  # Grab Coordinates
  Lng <- df$CgShape$Center$Lng
  Lat <- df$CgShape$Center$Lat
  Oid <- df$Oid
  pts <- data.frame(Lng, Lat, Oid)
  pts <- dplyr::mutate(pts, Lng = ifelse(is.na(Lng), 0, Lng),
                       Lat = ifelse(is.na(Lat), 0, Lat))
  df$CgShape <- NULL
  # Final
  points_final <- sp::SpatialPointsDataFrame(sp::coordinates(pts), df, match.ID = "Oid")
  sp::proj4string(points_final) <- sp::CRS("+proj=utm +north +zone=16T + datum=WGS84")

  while(total > nrow(points_final@data)) {
    offset <- as.numeric(j$`_metadata`$offset + j$`_metadata`$limit)

    url <- paste0("https://cgweb06.cartegraphoms.com/", org, "/api/v1/classes/", class, "?limit=1000&offset=", offset, filter, fields)

    g <- httr::GET(url, httr::authenticate(un, pw, type = "basic"))
    load <- httr::content(g, as = "text")
    j <- jsonlite::fromJSON(load)

    df <- data.frame(j[[class]])

    # Grab Coordinates
    Lng <- df$CgShape$Center$Lng
    Lat <- df$CgShape$Center$Lat
    Oid <- df$Oid
    pts <- data.frame(Lng, Lat, Oid)
    pts <- dplyr::mutate(pts, Lng = ifelse(is.na(Lng), 0, Lng),
                         Lat = ifelse(is.na(Lat), 0, Lat))
    df$CgShape <- NULL

    # Temp Data to Bind
    points_temp <- sp::SpatialPointsDataFrame(sp::coordinates(pts), df, match.ID = "Oid")
    sp::proj4string(points_temp) <- sp::CRS("+proj=utm +north +zone=16T + datum=WGS84")

    # Bind Temp to Final
    points_final <- maptools::spRbind(points_final, points_temp)
  }

  return(points_final)
}

#' Make Line
#'
#' @param x Coordinates
#'
#' @return Lines
#'
#' @examples
#' @export
makeLine <- function(x) {
  sp::Lines(sp::Line(cbind(x["Lng"], x["Lat"])), ID = paste(c(sample(1:9,1), sample( 0:9, 19, replace=TRUE)), collapse=""))
}

#' Return Lines
#'
#' @param class cartegraph class name
#' @param fields fields you want returned
#' @param filter optional filter
#' @param un api username
#' @param pw api password
#' @param org orginization API ID ie 'PittsburghPA'
#'
#' @return SpatialPointsDataFrame
#'
#' @examples \dontrun {
#' markings <- cgLine("cgMarkingsClass",
#'     "Oid,IDField,TypeField,StreetField,PavementField",
#'     un = "fakeUn",
#'     pw = "fakePwd",
#'     org = "AnytownUSA")
#' }
#' @export
# Cg General Shape API Call
cgLine <- function(class, fields = "", filter = "", un, pw, org) {
  fields <- ifelse(is.list(fields), paste(fields, collapse = ","), fields)
  fields <- ifelse(fields == "", "", paste0("&fields=", fields, ",cgShape"))
  filter <- ifelse(filter == "", "", paste0("&filter=", filter))

  url <- paste0("https://cgweb06.cartegraphoms.com/", org, "/api/v1/classes/", class, "?limit=1000&offset=0", filter, fields)

  g <- httr::GET(url, httr::authenticate(un, pw, type = "basic"))
  content <- httr::content(g, as = "text")
  j <- jsonlite::fromJSON(content)

  if (httr::http_error(g)) {
    stop(j$Message)
  } else if (j$`_metadata`$totalCount == 0) {
    stop("No rows for this request")
  }

  total <- j$`_metadata`$totalCount

  load <- data.frame(j[[class]])
  load <- load[!is.na(load$CgShape$ShapeType),]

  ap <- lapply(load$CgShape$Points, makeLine)
  lines <- sp::SpatialLines(ap, proj4string=sp::CRS("+proj=utm +north +zone=16T + datum=WGS84"))

  sp::spChFIDs(lines) <- load$Oid
  row.names(load) <- load$Oid

  final <- sp::SpatialLinesDataFrame(lines, load)
  final$CgShape <- NULL
  sent <- 1000

  while (total > sent) {
    offset <- as.numeric(j$`_metadata`$offset + j$`_metadata`$limit)

    url <- paste0("https://cgweb06.cartegraphoms.com/", org, "/api/v1/classes/", class, "?limit=1000&offset=", offset, filter, fields)

    g <- httr::GET(url, httr::authenticate(un, pw, type = "basic"))
    content <- httr::content(g, as = "text")
    j <- jsonlite::fromJSON(content)

    load <- data.frame(j[[class]])
    load <- load[!is.na(load$CgShape$ShapeType),]

    ap <- lapply(load$CgShape$Points, makeLine)
    lines <- sp::SpatialLines(ap, proj4string=CRS("+proj=utm +north +zone=16T + datum=WGS84"))

    sp::spChFIDs(lines) <- load$Oid
    row.names(load) <- load$Oid

    tmp <- SpatialLinesDataFrame(lines, load)
    tmp$CgShape <- NULL

    final <- maptools::spRbind(final, tmp)

    sent <- 1000 + offset
  }

  return(final)
}

#' Return Polygons
#'
#' @param class cartegraph class name
#' @param fields fields you want returned
#' @param filter optional filter
#' @param un api username
#' @param pw api password
#' @param org orginization API ID ie 'PittsburghPA'
#'
#' @return SpatialPointsDataFrame
#'
#' @examples \dontrun {
#' cgPoly("cgFacilities",
#'     filter = "([Inactive]%20=%20false)",
#'     un = "fakeUn",
#'     pw = "fakePwd",
#'     org = "AnytownUSA")
#' }
#' @export
cgPoly <- function(class, fields = "", filter = "", un, pw, org) {
  fields <- ifelse(is.list(fields), paste(fields, collapse = ","), fields)
  fields <- ifelse(fields == "", "", paste0("&fields=", fields, ",cgShape"))
  filter <- ifelse(filter == "", "", paste0("&filter=", filter))

  url <- paste0("https://cgweb06.cartegraphoms.com/", org, "/api/v1/classes/", class, "?limit=1000&offset=0", filter, fields)

  g <- httr::GET(url, httr::authenticate(un, pw, type = "basic"))
  content <- httr::content(g, as = "text")
  j <- jsonlite::fromJSON(content)

  if (httr::http_error(g)) {
    stop(j$Message)
  } else if (j$`_metadata`$totalCount == 0) {
    stop("No rows for this request")
  }

  total <- as.numeric(j$`_metadata`$totalCount)

  df <- data.frame(j[[class]])
  coords <-df$CgShape$Points

  df$CgShape <- NULL
  # Build Shapes
  count <- 1
  for (i in 1:length(coords)) {
    # Flip coords so Polygon reads correctly
    coords_t <- as.data.frame(coords[i])[c(2,1)]
    p <- sp::Polygon(coords_t)
    ps <- sp::Polygons(list(p),i)
    if (count == 1) {
      polys <-  sp::SpatialPolygons(list(ps), proj4string=sp::CRS("+proj=utm +north +zone=16T + datum=WGS84"))
      count <- 2
    } else {
      temp <-  sp::SpatialPolygons(list(ps), proj4string=sp::CRS("+proj=utm +north +zone=16T + datum=WGS84"))
      # Bind old polygons to new
      polys <- maptools::spRbind(polys, temp)
    }
  }
  # Same ID's
  row.names(df) <- df$Oid
  sp::spChFIDs(polys) <- df$Oid

  polys_final <- sp::SpatialPolygonsDataFrame(polys, df, match.ID = TRUE)

  sent <- 1000
  while(total > sent) {
    offset <- as.numeric(j$`_metadata`$offset + j$`_metadata`$limit)

    url <- paste0("https://cgweb06.cartegraphoms.com/", org, "/api/v1/classes/", class, "?limit=1000&offset=", offset, filter, fields)

    request <- httr::GET(url, httr::authenticate(un, pw, type = "basic"))
    content <- httr::content(request, as = "text")
    j <- jsonlite::fromJSON(content)

    # Form Dataframe & coordinates
    df <- data.frame(j[[class]])
    coords <- df$CgShape$Points
    df$CgShape <- NULL

    count <- 1
    for (i in 1:length(coords)) {
      # Flip coords so Polygon reads correctly
      coords_t <- as.data.frame(coords[i])[c(2,1)]
      p <- sp::Polygon(coords_t)
      ps <- sp::Polygons(list(p),i)
      if (count == 1) {
        polys <-  sp::SpatialPolygons(list(ps), proj4string=sp::CRS("+proj=utm +north +zone=16T + datum=WGS84"))
        count <- 2
      } else {
        temp <-  sp::SpatialPolygons(list(ps), proj4string=sp::CRS("+proj=utm +north +zone=16T + datum=WGS84"))
        # Bind old polygons to new
        polys <- maptools::spRbind(polys, temp)
      }
    }
    # Same ID's
    row.names(df) <- df$Oid
    spChFIDs(polys) <- df$Oid
    # Build Temp Shape
    polys_temp <- sp::SpatialPolygonsDataFrame(polys, df, match.ID = TRUE)
    # Add to previous
    polys_final <- maptools::spRbind(polys_final, polys_temp)

    sent <- 1000 + offset
  }

  return(polys_final)
}
