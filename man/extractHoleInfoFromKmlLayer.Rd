% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions.R
\name{extractHoleInfoFromKmlLayer}
\alias{extractHoleInfoFromKmlLayer}
\title{Read golf hole info from a KML layer}
\usage{
extractHoleInfoFromKmlLayer(layerName, kmlFile, courseName = NULL,
  elevationCacheFile = NULL)
}
\arguments{
\item{layerName}{the name of the Folder in the KML file to indicate the hole.  Folder name must be "Hole X" where X is 1:18.}

\item{courseName}{the name of the golf course (if missing, same as KML file name, minus .kml)}

\item{elevationCacheFile}{the path to a file in which to store a cache of elevations, to avoid over-hitting the Google elevation API.  NULL (the default)
indicates not to use a cache.}
}
\value{
a one-row tibble (data frame) with the information about the specified hole
}
\description{
Read a Folder structure from a KML file created in accordance with the conventions of this package, and produce
a set of information about the golf hole documented in that folder.  See the package README.md for details.
}
\details{
This function looks for:
\itemize{
\item A Placemark with the name 'Tee' placed at the teeing ground.  Optionally, this placemark can have a description to indicate
out-of-bounds on the hole (e.g., OB=left for out-of-bounds on the left)
\item On par 4 and par 5 holes, paths drawn at 200, 250, and 300 yard distances from the tee, with the path being a single line drawn
to the width of the fairway at that point.  These paths should be named '200 width', '250 width', and '300 width', respectively, and
they must appear in the hole folder in that order.
\item Placemarks to indicate bunkers and water hazards, named 'Bunker' and 'Hazard' respectively
\item A Polygon around the green (putting surface), named 'Green'
}
}

