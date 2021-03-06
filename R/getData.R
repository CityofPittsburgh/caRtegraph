#' Return dataframe object rom CartegraphA PI
#'
#' @param class cartegraph class name
#' @param fields fields you want returned
#' @param filter optional filter
#' @param un api username
#' @param pw api password
#' @param org orginization API ID ie 'PittsburghPA'
#' @param base_url API Base URL (defaulted to "https://cgweb06.cartegraphoms.com/")
#'
#' @return data.frame
#' @examples \dontrun{
#' pools <- cgDf("PoolsClass",
#' "Oid,IDField,NeighborhoodField,PoolCapacityField,PoolTypeField,RetiredField,WaterSourceField",
#' un = "fakeUn", pw = "fakePwd", org = "AnytownUSA")
#' }
#' @export
cgDf <- function(class, fields = "", filter = "", un, pw, org, base_url = "https://cgweb06.cartegraphoms.com/") {
  fields <- ifelse(is.list(fields), paste(fields, collapse = ","), fields)
  fields <- ifelse(fields == "", "", paste0("&fields=", fields))
  # Check for filter
  filter <- ifelse(filter == "", "", paste0("&filter=", filter))

  url <- paste0(base_url, org, "/api/v1/classes/", class, "?limit=1000&offset=0", filter, fields)
  g <- httr::GET(url, httr::authenticate(un, pw, type = "basic"))
  content <- httr::content(g, as = "text")

  j <- jsonlite::fromJSON(content)
  if (httr::http_error(g)) {
    stop(j$Message)
  } else if (j$`_metadata`$totalCount == 0) {
    warning("No rows for this request")

    df_final <- data.frame()
  } else {

    total <- as.numeric(j$`_metadata`$totalCount)
    df_final <- data.frame(j[[class]])

    while(total > nrow(df_final)) {
      offset <- as.numeric(j$`_metadata`$offset + j$`_metadata`$limit)

      url <- paste0(base_url, org, "/api/v1/classes/", class, "?limit=1000&offset=", offset, filter, fields)
      request <- httr::GET(url, httr::authenticate(un, pw, type = "basic"))
      content <- httr::content(request, as = "text")

      j <- jsonlite::fromJSON(content)

      df <- data.frame(j[[class]])

      df_final <- rbind(df_final, df)
    }
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
#' @param base_url API Base URL (defaulted to "https://cgweb06.cartegraphoms.com/")
#'
#' @return SpatialPointsDataFrame
#'
#' @examples \dontrun{
#' signs <- cgPoints("cgSignsClass",
#' c("MUTCDCodeField", "LocatorAddressNumberField",
#' "PavementField", "LocatorStreetField", "LocatorCityField",
#' "MountingFixtureField", "InstalledField"),
#' '(%5BMUTCDCode%5D%20in%20(%22R1-1%22,%20%22W3-D1%22))',
#' "fakeUn""fakePwd")
#' }
#' @export
cgPoints <- function(class, fields = "", filter = "", un, pw, org, method = "sp", base_url = "https://cgweb06.cartegraphoms.com/") {
  fields <- ifelse(is.list(fields), paste(fields, collapse = ","), fields)
  fields <- ifelse(fields == "", "", paste0("&fields=", fields, ",cgShape"))
  filter <- ifelse(filter == "", "", paste0("&filter=", filter))

  url <- paste0(base_url, org, "/api/v1/classes/", class, "?limit=1000&offset=0", filter, fields)

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

  # Final Data to Bind t0
  points_final <- sp::SpatialPointsDataFrame(sp::coordinates(pts), jsonlite::flatten(df), match.ID = "Oid")
  sp::proj4string(points_final) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  while(total > nrow(points_final@data)) {
    offset <- as.numeric(j$`_metadata`$offset + j$`_metadata`$limit)

    url <- paste0(base_url, org, "/api/v1/classes/", class, "?limit=1000&offset=", offset, filter, fields)

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
    points_temp <- sp::SpatialPointsDataFrame(sp::coordinates(pts), jsonlite::flatten(df), match.ID = "Oid")
    sp::proj4string(points_temp) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

    # Bind Temp to Final
    points_final <- maptools::spRbind(points_final, points_temp)
  }

  if (method == "sf") {
    points_final <- sf::st_as_sf(points_final)
  }

  return(points_final)
}

#' Make Line
#'
#' @param x Coordinates
#'
#' @return Lines
#'
#' @examples \dontrun{
#' # Used in cgLine function
#' ap <- lapply(load$CgShape$Points, makeLine)
#' }
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
#' @param method "sp" for Spatial objects output, "sf" for simple features
#' @param base_url API Base URL (defaulted to "https://cgweb06.cartegraphoms.com/")
#'
#' @return SpatialPointsDataFrame
#'
#' @examples \dontrun{
#' markings <- cgLine("cgMarkingsClass",
#'     "Oid,IDField,TypeField,StreetField,PavementField",
#'     un = "fakeUn",
#'     pw = "fakePwd",
#'     org = "AnytownUSA")
#' }
#' @export
# Cg General Shape API Call
cgLine <- function(class, fields = "", filter = "", un, pw, org, method = "sp", base_url = "https://cgweb06.cartegraphoms.com/") {
  fields <- ifelse(is.list(fields), paste(fields, collapse = ","), fields)
  fields <- ifelse(fields == "", "", paste0("&fields=", fields, ",cgShape"))
  filter <- ifelse(filter == "", "", paste0("&filter=", filter))

  url <- paste0(base_url, org, "/api/v1/classes/", class, "?limit=1000&offset=0", filter, fields)

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
  lines <- sp::SpatialLines(ap, proj4string=sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

  sp::spChFIDs(lines) <- load$Oid
  row.names(load) <- load$Oid

  load$CgShape <- NULL
  final <- sp::SpatialLinesDataFrame(lines, jsonlite::flatten(load), match.ID = "Oid")

  sent <- 1000

  while (total > sent) {
    offset <- as.numeric(j$`_metadata`$offset + j$`_metadata`$limit)

    url <- paste0(base_url, org, "/api/v1/classes/", class, "?limit=1000&offset=", offset, filter, fields)

    g <- httr::GET(url, httr::authenticate(un, pw, type = "basic"))
    content <- httr::content(g, as = "text")
    j <- jsonlite::fromJSON(content)

    load <- data.frame(j[[class]])
    load <- load[!is.na(load$CgShape$ShapeType),]

    ap <- lapply(load$CgShape$Points, makeLine)
    lines <- sp::SpatialLines(ap, proj4string=sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

    sp::spChFIDs(lines) <- load$Oid
    row.names(load) <- load$Oid
    load$CgShape <- NULL

    tmp <- sp::SpatialLinesDataFrame(lines, jsonlite::flatten(load), match.ID = "Oid")

    final <- maptools::spRbind(final, tmp)

    sent <- 1000 + offset
  }

  if (method == "sf") {
    final <- sf::st_as_sf(final)
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
#' @param method "sp" for Spatial objects output, "sf" for simple features
#' @param base_url API Base URL (defaulted to "https://cgweb06.cartegraphoms.com/")
#'
#' @return SpatialPointsDataFrame
#'
#' @examples \dontrun{
#' cgPoly("cgFacilities",
#'     filter = "([Inactive]%20=%20false)",
#'     un = "fakeUn",
#'     pw = "fakePwd",
#'     org = "AnytownUSA")
#' }
#' @export
cgPoly <- function(class, fields = "", filter = "", un, pw, org, method = "sp", base_url = "https://cgweb06.cartegraphoms.com/") {
  fields <- ifelse(is.list(fields), paste(fields, collapse = ","), fields)
  fields <- ifelse(fields == "", "", paste0("&fields=", fields, ",cgShape"))
  filter <- ifelse(filter == "", "", paste0("&filter=", filter))

  url <- paste0(base_url, org, "/api/v1/classes/", class, "?limit=1000&offset=0", filter, fields)

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
  breaks <- df$CgShape$Breaks

  df$CgShape <- NULL
  # Build Shapes
  count <- 1
  for (i in 1:length(coords)) {
    # Flip coords so Polygon reads correctly
    coords_t <- as.data.frame(coords[i])[c(2,1)]
    breaks_t <- unlist(breaks[i])
    n_breaks <- length(breaks_t)

    # Create multiple polygons if break
    if (n_breaks > 0) {
      p_list <- list()
      for  (x in 1:n_breaks) {
        start <- breaks_t[x] + 1
        if (x == n_breaks) {
          end <- nrow(coords_t)
        } else {
          end <- breaks_t[x + 1]
        }
        coords_p <- coords_t[start:end,]
        p_list <- append(p_list, sp::Polygon(coords_p, hole = FALSE))
      }
      ps <- sp::Polygons(p_list, ID = i)
    } else{
      p <- sp::Polygon(coords_t)
      ps <- sp::Polygons(list(p), ID = i)
    }

    if (count == 1) {
      polys <-  sp::SpatialPolygons(list(ps), proj4string=sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      count <- 2
    } else {
      temp <-  sp::SpatialPolygons(list(ps), proj4string=sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      # Bind old polygons to new
      polys <- maptools::spRbind(polys, temp)
    }
  }
  # Same ID's
  row.names(df) <- df$Oid
  sp::spChFIDs(polys) <- df$Oid

  polys_final <- sp::SpatialPolygonsDataFrame(polys, jsonlite::flatten(df), match.ID = "Oid")

  sent <- 1000
  while(total > sent) {
    offset <- as.numeric(j$`_metadata`$offset + j$`_metadata`$limit)

    url <- paste0(base_url, org, "/api/v1/classes/", class, "?limit=1000&offset=", offset, filter, fields)

    request <- httr::GET(url, httr::authenticate(un, pw, type = "basic"))
    content <- httr::content(request, as = "text")
    j <- jsonlite::fromJSON(content)

    # Form Dataframe & coordinates
    df <- data.frame(j[[class]])
    coords <- df$CgShape$Points
    breaks <- df$CgShape$Breaks

    df$CgShape <- NULL

    count <- 1
    for (i in 1:length(coords)) {
      # Flip coords so Polygon reads correctly
      coords_t <- as.data.frame(coords[i])[c(2,1)]
      breaks_t <- unlist(breaks[i])
      n_breaks <- length(breaks_t)

      # Create multiple polygons if break
      if (n_breaks > 0) {
        p_list <- list()
        for  (x in 1:n_breaks) {
          start <- breaks_t[x] + 1
          if (x == n_breaks) {
            end <- nrow(coords_t)
          } else {
            end <- breaks_t[x + 1]
          }
          coords_p <- coords_t[start:end,]
          p_list <- append(p_list, sp::Polygon(coords_p, hole = FALSE))
        }
        ps <- sp::Polygons(p_list, ID = i)
      } else{
        p <- sp::Polygon(coords_t)
        ps <- sp::Polygons(list(p), ID = i)
      }

      if (count == 1) {
        polys <-  sp::SpatialPolygons(list(ps), proj4string=sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
        count <- 2
      } else {
        temp <-  sp::SpatialPolygons(list(ps), proj4string=sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
        # Bind old polygons to new
        polys <- maptools::spRbind(polys, temp)
      }
    }
    # Same ID's
    row.names(df) <- df$Oid
    sp::spChFIDs(polys) <- df$Oid

    # Build Temp Shape
    polys_temp <- sp::SpatialPolygonsDataFrame(polys, jsonlite::flatten(df), match.ID = "Oid")
    # Add to previous
    polys_final <- maptools::spRbind(polys_final, polys_temp)

    sent <- 1000 + offset
  }
  if (method == "sf") {
    polys_final <- sf::st_as_sf(polys_final)
  }

  return(polys_final)
}

#' Get a single primary Attachment
#'
#' @param class class requesting attachment from
#' @param filename output file name, names not ending in ".jpg" will have value appended.
#' @param Oid Oid of the attachment
#' @param un api username
#' @param pw api password
#' @param org orginization API ID ie 'PittsburghPA'
#' @param base_url API Base URL (defaulted to "https://cgweb06.cartegraphoms.com/")
#'
#' @return A saved jpeg image
#' @export
#'
#' @examples \dontrun{
#' cgAttachment("cgFacilitiesClass",
#'     Oid = 31459,
#'     un = "fakeUn",
#'     pw = "fakePwd",
#'     org = "AnytownUSA")
#' }
cgAttachment <- function(class, filename, Oid, un, pw, org, base_url = "https://cgweb06.cartegraphoms.com/") {
  url <- paste0(base_url, org, "/api/v1/attachments/primary/", class, "/", Oid)
  filename <- ifelse(grepl(".jpg$", filename), filename, paste0(filename, ".jpg"))
  filename <- gsub(" ", "_", filename)
  r <- httr::GET(url, httr::authenticate(un, pw, type = "basic"))

  if (r$status_code == 200) {
  type <- tryCatch({
      check <- httr::content(r, type = httr::headers(r)$`content-type`)
      type <- httr::headers(r)$`content-type`
    }, error = function(x) {
      type <-"image/jpeg"
    })
    if (type == "image/jpeg") {
      jpg <- httr::content(r, type = "image/jpeg")
      jpeg::writeJPEG(jpg, filename)
    } else if (type == "image/png") {
      png <- httr::content(r, type = "image/png")
      filename <- gsub(".jpg", ".png", filename)
      png::writePNG(png, filename)
    } else {
      warning("Attachment for ", Oid, " is not a jpeg or png file. Failed to save image")
    }
  } else {
    warning("Failed to get/save image: ", Oid)
  }
}

#' Get multiple primary attachments from a class
#'
#' @param class class requesting attachments from
#' @param outDir save directory defaults to class name, optional
#' @param filter filter to be applied to class
#' @param zip option to zip the folder
#' @param un api username
#' @param pw api password
#' @param org orginization API ID ie 'PittsburghPA'
#' @param base_url API Base URL (defaulted to "https://cgweb06.cartegraphoms.com/")
#'
#' @return folder of saved jpeg images
#' @export
#'
#' @examples \dontrun{
#' cgAttachments("cgFacilities",
#'     filter = "([Inactive]%20=%20false)",
#'     un = "fakeUn",
#'     pw = "fakePwd",
#'     org = "AnytownUSA")
#' }
cgAttachments <- function(class, outDir ="", filter = '([PrimaryAttachment]%20<>%20"")', zip = FALSE, un, pw, org, base_url = "https://cgweb06.cartegraphoms.com/") {
  # Create Folder for Class images
  filter = ifelse(filter == '([PrimaryAttachment]%20<>%20"")', filter, gsub("\\*)$", '%20AND%20[PrimaryAttachment]%20<>%20"")'))
  outDir = ifelse(outDir == "", class, outDir)
  dir.create(outDir, showWarnings = FALSE)
  # Request class
  df <- caRtegraph::cgDf(class, "IDField,PrimaryAttachmentField", filter, un, pw, org)
  # Grab attachments by Oid
  for (i in 1:nrow(df)) {
    filename = paste0(outDir, "/", df$IDField[i])
    cgAttachment(class, filename, df$Oid[i], un, pw , org, base_url)
  }
  if (zip) {
    files2zip <- dir(outDir, full.names = TRUE)
    zip(zipfile = outDir, files = files2zip)
    unlink(outDir, recursive = TRUE)
  }
}
