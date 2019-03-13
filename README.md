# caRtegraph
R package for communicating with the Cartegraph API

### Author
[Geoffrey Arnold](mailto:geoffrey.arnold@pittsburghpa.gov)

## Installation
`devtools::install_github("CityofPittsburgh/caRtegraph")`

## What is Cartegraph?
[Cartegraph](https://www.cartegraph.com/) is an asset management software that is widely used by numerous governments.

## Usage

Every function begins with `cg*`. The packages' functions are mostly used to get spatial data from the API, but it can also be used to add and delete data as well as download primary attachments.

## Notes

This our first `R` package so please be nice when reporting issues.

If you like `sf`, sorry we've been working with Cartegraph for awhile, and tend to use the [leaflet](https://rstudio.github.io/leaflet/) package for visuals so `sp` was prioitized. I am looking to add functionality for that outside of simply transforming the data from `sp` objects, and once that is complete this package will be submitted to CRAN.
