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

#' Calcualte particulate matter NowCast
#'
#' The function returns the weighted mean of exctly 12 given PM raw values, as defined by the United States Environmental Protection Agency (EPA)[1,2].
#' 
#' @param x A numeric vector of length 12, being the PM raw values of the past 12 hours, with the last being the most recent one.
#' @param norm One value of either "PM10" or "PM2.5", setting the PM size of the given values of \code{x}.
#' @param raw Logical, if true returns only the NowCast raw value.
#' @param latex_cellcolors Logical, if true the "NowCast_color_*" columns will show LaTeX \code{\\cellcolor} code instead
#'    of HTML colors. See \code{Colors} section of \code{\link[AQI_US_PM]{AQI_US_PM}}.
#' @return If \code{raw=TRUE}, a numeric vector with the raw NowCast values. Otherwise a data frame with
#'    four columns: NowCast raw value (weighted mean of input data), AQI raw value, AQI level (factor), recommended color code (factor).
#'    The column names start with "NowCast_raw_", "NowCast_AQI_", "NowCast_AQI_level_", and "NowCast_AQI_color_", and end with either "PM10" or
#'    "PM2_5", depending on the value of \code{norm}.
#' @references
#'    [1] \url{https://www.epa.gov/airnow/faq/Nowcast-formula.pptx}
#'
#'    [2] \url{https://en.wikipedia.org/wiki/NowCast_(air_quality_index)}
#' @export
#' @examples
#' NowCast_PM(c(50, 80, 75, 90, 82, 53, 64, 74, 21, 10, 16, 13))

NowCast_PM <- function(
  x,
  norm="PM10",
  raw=FALSE,
  latex_cellcolors=TRUE
){
  if(any(!is.numeric(x), length(x) != 12)){
    stop(simpleError("\"x\" must be a numeric vector of length 12."))
  } else {}
  wx <- min(x) / max(x)
  w <- ifelse(wx > .5, wx, .5)
  x <- rev(x)
  
  # here goes the NowCast
  i <- 1:12
  if(w == 1){
    nc <- mean(x)
  } else {
    nc <- sum(w^(i - 1) * x[i]) / sum(w^(i - 1))
  }

  if(isTRUE(raw)){  
    return(nc)
  } else {
    AQI_nc <- AQI_US_PM(nc, norm=norm, raw=FALSE, latex_cellcolors=latex_cellcolors)
    colnames(AQI_nc) <- paste0("NowCast_", gsub("AQI_US_", "AQI_", colnames(AQI_nc)))
    return(AQI_nc)
  }
}
