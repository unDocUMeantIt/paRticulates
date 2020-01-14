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

#' Calcualte particulate matter sub-index of th EU Common Air Quality Index (CAQI)
#'
#' The function returns the respective hourly CAQI grading.
#' 
#' The index has five levels, named "very low" (best air quality), "low", "medium", "high", and "very high" (worst air quality).
#' Thresholds are different between PM10 and PM2.5 particles.
#' 
#' @param x A numeric vector of particular matter measurements (µg/m³).
#' @param norm One value of either "PM10" or "PM2.5", setting the PM size of the given values of \code{x}.
#' @return A named numeric vector.
#' @export
#' @examples
#' CAQI_sub(55)

CAQI_sub <- function(
  x,
  norm="PM10"
){
  if(x > 180){
    factor_PM10 <- ((x - 180) * 25 / 90) + 100
  } else if(x > 90){
    factor_PM10 <- ((x - 90) * 25 / 90) + 75
  } else if(x > 50){
    factor_PM10 <- ((x - 50) * 25 / 40) + 50
  } else {
    factor_PM10 <- x
  }

  if(x > 110){
    factor_PM2_5 <- ((x - 110) * 25 / 55) + 100
  } else if(x > 55){
    factor_PM2_5 <- ((x - 55) * 25 / 55) + 75
  } else if(x > 30){
    factor_PM2_5 <- (x - 30) + 50
  } else if(x > 15){
    factor_PM2_5 <- ((x - 15) * 25 / 15) + 25
  } else {
    factor_PM2_5 <- x * 25 / 15
  }

  # values are the lower end, i.e. 30 is considered "low"
  norm_CAQI <- c(
    "very high"=100,
    "high"=75,
    "medium"=50,
    "low"=25,
    "very low"=0
  )

  return(c(PM10=factor_PM10, PM2_5=factor_PM2_5))
}
