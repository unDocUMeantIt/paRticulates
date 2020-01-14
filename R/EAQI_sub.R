# Copyright 2020 Meik Michalke <meik.michalke@hhu.de>
#
# This file is part of the R package paRticulates.
#
# paRticulates is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# paRticulates is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with paRticulates.  If not, see <http://www.gnu.org/licenses/>.

#' Calcualte particulate matter sub-index of the European Air Quality Index (EAQI)
#'
#' The function returns the respective hourly EAQI grading.
#' 
#' The index has six levels, named "good" (best air quality), "fair", "moderate", "poor", "very poor",
#' and "extremely poor" (worst air quality).
#' Thresholds are different between PM10 and PM2.5 particles.
#' 
#' @param x A numeric vector of particulate matter measurements (µg/m³).
#' @param norm One value of either "PM10" or "PM2.5", setting the PM size of the given values of \code{x}.
#' @return If \code{raw=TRUE}, a numeric vector with the raw EAQI values. Otherwise a data frame with
#'    four columns: raw input data, EAQI level (factor), recommended color code (factor).
#'    The column names start with "raw_", "EAQI_", and "color_", and end with either "PM10" or
#'    "PM2_5", depending on the value of \code{norm}.
#' @export
#' @examples
#' EAQI_sub(55)

EAQI_sub <- function(
  x,
  norm="PM10"
){
  index_level <- switch(norm,
    "PM10"={
        sapply(
          x,
          function(x){
            if(x > 150){
              "extremely poor"
            } else if(x > 100){
              "very poor"
            } else if(x > 50){
              "poor"
            } else if(x > 40){
              "moderate"
            } else if(x > 20){
              "fair"
            } else {
              "good"
            }
          }
        )
      },
    "PM2.5"={
        sapply(
          x,
          function(x){
            if(x > 75){
              "extremely poor"
            } else if(x > 50){
              "very poor"
            } else if(x > 25){
              "poor"
            } else if(x > 20){
              "moderate"
            } else if(x > 10){
              "fair"
            } else {
              "good"
            }
          }
        )
      },
      stop(simpleError("'norm' must be either \"PM10\" or \"PM2.5\"!"))
  )

  col_EAQI <- c(
    "extremely poor"="#7d2181",
    "very poor"="#960032",
    "poor"="#ff5050",
    "moderate"="#f0e641",
    "fair"="#50ccaa",
    "good"="#50f0e6"
  )

  result <- data.frame(
    raw=x,
    EAQI_level=factor(index_level, levels=names(col_EAQI)),
    color=factor(col_EAQI[index_level], levels=col_EAQI)
  )
  colnames(result) <- paste0(colnames(result), "_", gsub("\\.", "_", norm))

  return(result)
}
