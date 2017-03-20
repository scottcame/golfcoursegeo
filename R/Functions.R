packageEnvironment <- new.env()
DEBUG <- FALSE

#' Read golf hole info from a KML layer
#'
#' Read a Folder structure from a KML file created in accordance with the conventions of this package, and produce
#' a set of information about the golf hole documented in that folder.  See the package README.md for details.
#'
#' This function looks for:
#' \itemize{
#' \item A Placemark with the name 'Tee' placed at the teeing ground.  Optionally, this placemark can have a description to indicate
#' out-of-bounds on the hole (e.g., OB=left for out-of-bounds on the left)
#' \item On par 4 and par 5 holes, paths drawn at 200, 250, and 300 yard distances from the tee, with the path being a single line drawn
#' to the width of the fairway at that point.  These paths should be named '200 width', '250 width', and '300 width', respectively, and
#' they must appear in the hole folder in that order.
#' \item Placemarks to indicate bunkers and water hazards, named 'Bunker' and 'Hazard' respectively
#' \item A Polygon around the green (putting surface), named 'Green'
#' }
#'
#' @export
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom rgdal readOGR
#' @importFrom sp CRS spTransform SpatialLines Lines Line SpatialPolygons Polygons Polygon coordinates
#' @importFrom rgeos gLength gCentroid gDistance gDifference gArea
#' @importFrom XML xmlParse getNodeSet
#' @importFrom tools file_path_sans_ext
#' @param layerName the name of the Folder in the KML file to indicate the hole.  Folder name must be "Hole X" where X is 1:18.
#' @param courseName the name of the golf course (if missing, same as KML file name, minus .kml)
#' @param elevationCacheFile the path to a file in which to store a cache of elevations, to avoid over-hitting the Google elevation API.  NULL (the default)
#' indicates not to use a cache.
#' @return a one-row tibble (data frame) with the information about the specified hole
extractHoleInfoFromKmlLayer <- function(layerName, kmlFile, courseName=NULL, elevationCacheFile=NULL) {
  
  warnOption <- getOption('warn')
  options(warn = -1)
  
  if (is.null(courseName)) {
    courseName <- file_path_sans_ext(basename(kmlFile))
  }

  xmlDoc <- xmlParse(kmlFile)
  
  if (length(getNodeSet(xmlDoc, paste0("//kml:Folder[kml:name='", layerName, "']//kml:Placemark[kml:name='Tee']"), c(kml='http://www.opengis.net/kml/2.2')))==0) {
    stop(paste0('No Tee placemark defined for ', layerName))
  }
  
  if (length(getNodeSet(xmlDoc, paste0("//kml:Folder[kml:name='", layerName, "']//kml:Placemark[kml:name='Green']"), c(kml='http://www.opengis.net/kml/2.2')))==0) {
    stop(paste0('No Green polygon defined for ', layerName))
  }
  
  kmlPoints <- readOGR(dsn=kmlFile, layer=layerName, require_geomType = 'wkbPoint', verbose=FALSE)
  kmlPoly <- readOGR(dsn=kmlFile, layer=layerName, require_geomType = 'wkbPolygon', verbose=FALSE)

  projLon <- kmlPoints@coords[1,1]
  projLat <- kmlPoints@coords[1,2]
  aeqd <- CRS(paste0('+proj=aeqd +lon_0=', projLon, ' +lat_0=', projLat))

  kmlPolyP <- spTransform(kmlPoly, aeqd)
  kmlPointsP <- spTransform(kmlPoints, aeqd)

  teeDf <- kmlPoints@data %>% filter(Name=='Tee')
  teeRowNumber <- as.integer(rownames(teeDf))
  ob <- gsub(x=teeDf$Description, pattern='OB=(.+)', replacement='\\1')
  ob <- ifelse(ob=='', 'none', ob)

  centerGreenCoordinates <- coordinates(kmlPoly)[1,]
  teeCoordinates <- kmlPoints@coords[teeRowNumber, 1:2]
  center250Coordinates <- NULL

  doglegAngle <- NA
  widths <- as.integer(c(NA, NA, NA))
  idx200Width <- 1
  idx250Width <- 2
  idx300Width <- 3
  par3Hole <- TRUE

  if (length(getNodeSet(xmlDoc, paste0("//kml:Folder[kml:name='", layerName, "']//kml:LineString"), c(kml='http://www.opengis.net/kml/2.2')))) {

    kmlLines <- readOGR(dsn=kmlFile, layer=layerName, require_geomType = 'wkbLineString', verbose=FALSE)
    
    df <- kmlLines@data
    df$id <- rownames(df)
    
    stopifnorows <- function(df, dfName) {
      if(nrow(df)==0) {
        stop(paste0(dfName, ' is missing...stopping execution'))
      }
      df
    }
    
    idx200Width <- df %>% filter(Name=='200 width') %>% stopifnorows('200width line') %>% .$id %>% as.integer() + 1
    idx250Width <- df %>% filter(Name=='250 width') %>% stopifnorows('250width line') %>% .$id %>% as.integer() + 1
    idx300Width <- df %>% filter(Name=='300 width') %>% stopifnorows('300width line') %>% .$id %>% as.integer() + 1
    
    kmlLinesP <- spTransform(kmlLines, aeqd)
    widths <- gLength(kmlLinesP, byid=TRUE) %>% metersToYards()
    midpoints <- gCentroid(kmlLines, byid=TRUE)
    center200Coordinates <- midpoints[idx200Width]@coords[1,]
    center250Coordinates <- midpoints[idx250Width]@coords[1,]
    
    doglegSpatialLines <- SpatialLines(
      list(
        Lines(list(Line(rbind(teeCoordinates, center200Coordinates))), ID='1'),
        Lines(list(Line(rbind(teeCoordinates, centerGreenCoordinates))), ID='2'),
        Lines(list(Line(rbind(center200Coordinates, centerGreenCoordinates))), ID='3')
      ), proj4string=kmlLines@proj4string
    )
    doglegSpatialLines <- spTransform(doglegSpatialLines, aeqd)
    doglegLineLengths <- gLength(doglegSpatialLines, byid=TRUE)
    doglegAngle <- 180*acos((doglegLineLengths[1]^2 + doglegLineLengths[2]^2 - doglegLineLengths[3]^2) / (2*doglegLineLengths[1]*doglegLineLengths[2]))/pi

    par3Hole <- FALSE

  }

  greenPolygon <- kmlPolyP@polygons[[1]]@Polygons[[1]] # assumes only one polygon per hole...
  greenArea <- greenPolygon@area

  dm <- head(greenPolygon@coords, -1) %>% dist() %>% as.matrix()
  extremeVertexIndices <- rownames(which(dm==max(dm), arr.ind=TRUE)) %>% as.integer()
  extremeVertices <- greenPolygon@coords[extremeVertexIndices,]
  extremeVerticesMidpoint <- c(sum(extremeVertices[,1])/2, sum(extremeVertices[,2])/2)

  minimumBoundingCircleArea <- NULL
  radius <- sqrt(abs(extremeVertices[1,1]-extremeVertices[2,1])^2 + abs(extremeVertices[1,2]-extremeVertices[2,2])^2)/2

  while(is.null(minimumBoundingCircleArea)) {
    # note that this does not guarantee a true minimum bounding circle, but is probably close enough...
    theta <- seq(0, 2*pi, length.out=3600)
    greenBoundingCircle <- matrix(c(extremeVerticesMidpoint[1] + radius*cos(theta), extremeVerticesMidpoint[2] + radius*sin(theta)), ncol=2)
    greenBoundingCirclePoly <- SpatialPolygons(list(Polygons(list(Polygon(list(greenBoundingCircle))), ID='1')), proj4string=kmlPolyP@proj4string)
    diff <- gDifference(kmlPolyP, greenBoundingCirclePoly)
    if (!is.null(diff)) {
      radius <- radius + .1
    } else {
      minimumBoundingCircleArea <- gArea(greenBoundingCirclePoly)
    }
  }

  greenLinearity <- minimumBoundingCircleArea / greenArea

  teeBunkerDistance <- gDistance(subset(kmlPointsP, Name=='Tee'), subset(kmlPointsP, Name=='Bunker'), byid=TRUE) %>% metersToYards() %>% .[,1]
  bunkersInPlayFromTee <- sum((teeBunkerDistance >= 200 & teeBunkerDistance <= 300) | (length(teeBunkerDistance) & par3Hole))

  teeWaterHazardDistance <- gDistance(subset(kmlPointsP, Name=='Tee'), subset(kmlPointsP, Name=='Hazard'), byid=TRUE) %>% metersToYards() %>% .[,1]
  waterHazardsInPlayFromTee <- sum((teeWaterHazardDistance >= 200 & teeWaterHazardDistance <= 300) | (length(teeWaterHazardDistance) & par3Hole))

  greenBunkerDistance <- gDistance(kmlPolyP, subset(kmlPointsP, Name=='Bunker'), byid=TRUE) %>% metersToYards() %>% .[,1]
  greensideBunkers <- sum(greenBunkerDistance <= 25)

  greenHazardDistance <- gDistance(kmlPolyP, subset(kmlPointsP, Name=='Hazard'), byid=TRUE) %>% metersToYards() %>% .[,1]
  greensideHazards <- sum(greenHazardDistance <= 25)

  hole <- as.integer(gsub(x=layerName, pattern='Hole ([0-9]+)', replacement='\\1'))
  
  teeElevation <- getElevationFromGoogle(teeCoordinates, elevationCacheFile) %>% metersToFeet()
  greenElevation <- getElevationFromGoogle(centerGreenCoordinates, elevationCacheFile) %>% metersToFeet()
  fairwayElevation <- as.double(NA)
  
  if (!is.null(center250Coordinates)) {
    fairwayElevation <- getElevationFromGoogle(center250Coordinates, elevationCacheFile) %>% metersToFeet()
  }
  
  options(warn = warnOption)

  tibble(CourseName=courseName, HoleNumber=hole, DoglegAngle=doglegAngle, GreenArea=squareMetersToSquareYards(greenArea), GreenLinearity=greenLinearity,
         Width200=widths[idx200Width], Width250=widths[idx250Width], Width300=widths[idx300Width],
         TeeElevation=teeElevation, GreenElevation=greenElevation, FairwayElevation=fairwayElevation,
         BunkersInPlayFromTee=bunkersInPlayFromTee, WaterHazardsInPlayFromTee=waterHazardsInPlayFromTee,
         GreensideBunkers=greensideBunkers, GreensideHazards=greensideHazards, OB=ob)

}

metersToFeet <- function(m) {
  metersToYards(m)*3
}

metersToYards <- function(m) {
  m*1.09631
}

squareMetersToSquareYards <- function(msq) {
  msq*1.19599
}

#' Read golf course (several holes) info from a KML file
#'
#' Extract information about all the holes on a group of golf courses, documented in the specified vector of KML files.
#' @importFrom purrr map2_df map_df
#' @importFrom rgdal ogrListLayers
#' @return a tibble with a row for each hole documented in the specified file
#' @param kmlFiles the KML file documenting the holes on the course (see documentation for extractHoleInfoFromKmlLayer on file conventions)
#' @param elevationCacheFile the path to a file in which to store a cache of elevations, to avoid over-hitting the Google elevation API
#' @export
extractCourseInfoFromKmlFiles <- function(kmlFiles, elevationCacheFile=NULL) {
  map_df(kmlFiles, function(kmlFile) {
    layerNames <- as.character(ogrListLayers(dsn=kmlFile))
    map_df(layerNames, extractHoleInfoFromKmlLayer, kmlFile, courseName=NULL, elevationCacheFile)
  })
}

getElevationAPIURL <- function(lat, long) {
  URLencode(paste0("https://maps.googleapis.com/maps/api/elevation/json?locations=", lat, ',', long))
}

#' @importFrom RCurl getURL
#' @importFrom jsonlite fromJSON
getElevationFromGoogle <- function(coords, elevationCacheFile) {
  
  if (DEBUG) writeLines(paste0('Invoking getElevationFromGoogle, coords=', paste0(coords, collapse=',')))
  
  latCoord <- coords[2]
  longCoord <- coords[1]
  
  cache <- get0('elevation-cache', envir=packageEnvironment)
  
  if (!is.null(elevationCacheFile)) {
    if (DEBUG) writeLines(paste0('Using elevation cache, file=', elevationCacheFile))
    if (file.exists(elevationCacheFile)) {
      if (is.null(cache)) {
        if (DEBUG) writeLines('Elevation cache not found in package environment, reading from file')
        cache <- readRDS(elevationCacheFile)
        assign('elevation-cache', cache, envir=packageEnvironment)
      } else {
        if (DEBUG) writeLines('Elevation cache found in package environment')
      }
      resultRow <- cache %>% filter(lat==latCoord & long==longCoord)
      if (nrow(resultRow)) {
        if (DEBUG) writeLines('Location found in elevation cache, returning without hitting Google API')
        return(resultRow$elevation)
      }
    } else {
      if (DEBUG) writeLines('No existing elevation cache file found, creating...')
      cache <- data.frame()
    }
  } else {
    if (DEBUG) writeLines('Not caching elevation data')
  }
  
  u <- getElevationAPIURL(latCoord, longCoord)
  if (DEBUG) writeLines(paste0('Hitting Google Elevation API, url=', u))
  doc <- getURL(u)
  x <- fromJSON(doc)
  ret <- x$results$elevation
  
  if (!is.null(elevationCacheFile)) {
    resultRow <- data.frame(lat=latCoord, long=longCoord, elevation=ret)
    cache <- bind_rows(cache, resultRow)
    if (DEBUG) writeLines(paste0('Updating elevation cache in file ', elevationCacheFile))
    saveRDS(cache, elevationCacheFile)
    assign('elevation-cache', cache, envir=packageEnvironment)
  }
  
  ret
  
}
