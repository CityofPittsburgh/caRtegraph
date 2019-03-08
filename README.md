# caRtegraph
R package for communicating with the Cartegraph API

## Installation
# install.packages("devtools")
devtools::install_github("CityofPittsburgh/caRtegraph")

## What is Cartegraph?
[Cartegraph](https://www.cartegraph.com/) is an asset management software that is widely used by numerous governments.

## Usage

Every function begins with `cg*`. The packages' functions are mostly used to get spatial data from the API, but it can also be used to vageuly add and delete data as well, though those functions are far less built out and mostly require the user to create a tibble to pass as the JSON body.

## Notes

This our first `R` package so be nice in the issues please.

If you like `sf`, sorry we've been working with Cartegraph for awhile, and tend to use the [leaflet](https://rstudio.github.io/leaflet/) package for visuals so `sp` was prioitized. I am looking to add functionality for that outside of simply transforming the data from `sp` objects, and once that is complete this package will be submitted to CRAN.
