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

#' Calcualte particulate matter sub-index of the European Common Air Quality Index (CAQI)
#'
#' The function returns the respective hourly CAQI grading.
#' 
#' The index has five levels, named "very low" (best air quality), "low", "medium", "high", and "very high" (worst air quality).
#' Thresholds are different between PM10 and PM2.5 particles.
#' 
#' @param x A numeric vector of particulate matter measurements (µg/m³).
#' @param norm One value of either "PM10" or "PM2.5", setting the PM size of the given values of \code{x}.
#' @param raw Logical, if true returns only the CAQI raw values.
#' @return If \code{raw=TRUE}, a numeric vector with the raw CAQI values. Otherwise a data frame with
#'    four columns: raw input data, CAQI value, CAQI level (factor), recommended color code (factor).
#'    The column names start with "raw_", "CAQI_", "CAQI_level_", and "CAQI_color_", and end with either "PM10" or
#'    "PM2_5", depending on the value of \code{norm}.
#' @export
#' @examples
#' CAQI_sub(55)

CAQI_sub <- function(
  x,
  norm="PM10",
  raw=FALSE
){
  index_raw <- switch(norm,
    "PM10"={
        ceiling(
          sapply(
            x,
            function(x){
              if(x > 180){
                ((x - 180) * 25 / 90) + 100
              } else if(x > 90){
                ((x - 90) * 25 / 90) + 75
              } else if(x > 50){
                ((x - 50) * 25 / 40) + 50
              } else {
                x
              }
            }
          )
        )
      },
    "PM2.5"={
        ceiling(
          sapply(
            x,
            function(x){
              if(x > 110){
                ((x - 110) * 25 / 55) + 100
              } else if(x > 55){
                ((x - 55) * 25 / 55) + 75
              } else if(x > 30){
                (x - 30) + 50
              } else if(x > 15){
                ((x - 15) * 25 / 15) + 25
              } else {
                x * 25 / 15
              }
            }
          )
        )
      },
      stop(simpleError("'norm' must be either \"PM10\" or \"PM2.5\"!"))
  )

  if(isTRUE(raw)){
    return(index_raw)
  } else {
    # values are the lower end, i.e. 30 is considered "low"
    norm_CAQI <- c(
      "very high"=100,
      "high"=75,
      "medium"=50,
      "low"=25,
      "very low"=0
    )
    col_CAQI <- c(
      "very high"="#960018",
      "high"="#f29305",
      "medium"="#eec20b",
      "low"="#bbcf4c",
      "very low"="#79bc6a"
    )
    index_label <- sapply(
      index_raw,
      function(x){
        names(norm_CAQI)[min(which(x > norm_CAQI))]
      }
    )
    result <- data.frame(
      raw=x,
      CAQI=index_raw,
      CAQI_level=factor(index_label, levels=names(norm_CAQI)),
      CAQI_color=factor(col_CAQI[index_label], levels=col_CAQI)
    )
    colnames(result) <- paste0(colnames(result), "_", gsub("\\.", "_", norm))
    return(result)
  }
}
