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

#' Calcualte particulate matter sub-index of the US Air Quality Index (AQI)
#'
#' The function returns the 24-hour AQI grading for given PM raw values.
#' 
#' The index has six levels, named "good" (best air quality), "moderate", "unhealthy for sensitive groups", "unhealthy", "very unhealthy", and "hazardous" (worst air quality).
#' Thresholds are different between PM10 and PM2.5 particles.
#' 
#' @note If there is no full 24-hours of data available, the AQI standard calculates a NowCast value from
#' the last 12 hours. This is currently not implemented.
#' 
#' @section Colors: The data frame contains rows to support coloring tables or plots, e.g. if you use the data in
#'    an RMarkdown document. If \code{latex_cellcolors=FALSE}, they provide a \code{<span>} with hexadecimal HTML color codes
#'    for each index value. Otherwise, the factor labels are in LaTeX format using the \code{\\cellcolor} command.
#'    This is useful, e.g., if combined with the \code{kableExtra::kable} function. To use the code out of the box,
#'    define the following colors in your preamble (example shows usage in a YAML header):
#'    
#' \preformatted{
#' header-includes:
#'    - \definecolor{AQIUSg}{HTML}{00e400}
#'    - \definecolor{AQIUSm}{HTML}{ffff00}
#'    - \definecolor{AQIUSus}{HTML}{ff7e00}
#'    - \definecolor{AQIUSu}{HTML}{ff0000}
#'    - \definecolor{AQIUSvu}{HTML}{99004c}
#'    - \definecolor{AQIUSh}{HTML}{7e0023}
#' }
#' 
#' @param x A numeric vector of particulate matter measurements (µg/m³).
#' @param norm One value of either "PM10" or "PM2.5", setting the PM size of the given values of \code{x}.
#' @param raw Logical, if true returns only the AQI_US raw values.
#' @param latex_cellcolors Logical, if true the "AQI_US_color_*" columns will show LaTeX \code{\\cellcolor} code instead
#'    of HTML colors. See \code{Colors} section.
#' @return If \code{raw=TRUE}, a numeric vector with the raw AQI_US values. Otherwise a data frame with
#'    four columns: raw input data, AQI_US value, AQI_US level (factor), recommended color code (factor).
#'    The column names start with "raw_", "AQI_US_", "AQI_US_level_", and "AQI_US_color_", and end with either "PM10" or
#'    "PM2_5", depending on the value of \code{norm}.
#' @export
#' @examples
#' AQI_US_sub(55)

AQI_US_sub <- function(
  x,
  norm="PM10",
  raw=FALSE,
  latex_cellcolors=TRUE
){
  AQI_func <- function(x, c_lo, c_hi, i_lo, i_hi){
    result <- ((i_hi - i_lo) / (c_hi - c_lo)) * (x - c_lo) + i_lo
  }
  index_raw <- switch(norm,
    "PM10"={
        ceiling(
          sapply(
            x,
            function(x){
              if(x > 504){
                AQI_func(x=x, c_lo=505, c_hi=604, i_lo=401, i_hi=500)
              } else if(x > 424){
                AQI_func(x=x, c_lo=425, c_hi=504, i_lo=301, i_hi=400)
              } else if(x > 354){
                AQI_func(x=x, c_lo=355, c_hi=424, i_lo=201, i_hi=300)
              } else if(x > 254){
                AQI_func(x=x, c_lo=255, c_hi=354, i_lo=151, i_hi=200)
              } else if(x > 154){
                AQI_func(x=x, c_lo=155, c_hi=254, i_lo=101, i_hi=150)
              } else if(x > 54){
                AQI_func(x=x, c_lo=55, c_hi=154, i_lo=51, i_hi=100)
              } else {
                AQI_func(x=x, c_lo=0, c_hi=54, i_lo=0, i_hi=50)
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
              if(x > 350.4){
                AQI_func(x=x, c_lo=350.5, c_hi=500.4, i_lo=401, i_hi=500)
              } else if(x > 250.4){
                AQI_func(x=x, c_lo=250.5, c_hi=350.4, i_lo=301, i_hi=400)
              } else if(x > 150.4){
                AQI_func(x=x, c_lo=150.5, c_hi=250.4, i_lo=201, i_hi=300)
              } else if(x > 55.4){
                AQI_func(x=x, c_lo=55.5, c_hi=150.4, i_lo=151, i_hi=200)
              } else if(x > 35.4){
                AQI_func(x=x, c_lo=35.5, c_hi=55.4, i_lo=101, i_hi=150)
              } else if(x > 12){
                AQI_func(x=x, c_lo=12.1, c_hi=35.4, i_lo=51, i_hi=100)
              } else {
                AQI_func(x=x, c_lo=0, c_hi=12, i_lo=0, i_hi=50)
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
    # values are the lower end, i.e. 30 is considered "good"
    norm_AQI_US <- c(
      "hazardous"=301,
      "very unhealthy"=201,
      "unhealthy"=151,
      "unhealthy for sensitive groups"=101,
      "moderate"=51,
      "good"=0
    )
    col_HTML_AQI_US <- c(
      "hazardous"="<div style=\"     background-color: #7e0023 !important; color: #ffffff !important;\" >&nbsp;hazardous&nbsp;</div>",
      "very unhealthy"="<div style=\"     background-color: #99004c !important;\" >&nbsp;very unhealthy&nbsp;</div>",
      "unhealthy"="<div style=\"     background-color: #ff0000 !important;\" >&nbsp;unhealthy&nbsp;</div>",
      "unhealthy for sensitive groups"="<div style=\"     background-color: #ff7e00 !important;\" >&nbsp;unhealthy for sensitive groups&nbsp;</div>",
      "moderate"="<div style=\"     background-color: #ffff00 !important;\" >&nbsp;moderate&nbsp;</div>",
      "good"="<div style=\"     background-color: #00e400 !important;\" >&nbsp;good&nbsp;</div>"
    )
    col_LaTeX_AQI_US <- c(
      "hazardous"="\\cellcolor{AQIUSh}\\textcolor{white}{hazardous}",
      "very unhealthy"="\\cellcolor{AQIUSvu}very unhealthy",
      "unhealthy"="\\cellcolor{AQIUSu}unhealthy",
      "unhealthy for sensitive groups"="\\cellcolor{AQIUSus}unhealthy for sensitive groups",
      "moderate"="\\cellcolor{AQIUSm}moderate",
      "good"="\\cellcolor{AQIUSg}good"
    )
    index_label <- sapply(
      index_raw,
      function(x){
        names(norm_AQI_US)[min(which(x > norm_AQI_US))]
      }
    )
    if(isTRUE(latex_cellcolors)){
      col_factor <- factor(col_LaTeX_AQI_US[index_label], levels=col_LaTeX_AQI_US)
    } else {
      col_factor <- factor(col_HTML_AQI_US[index_label], levels=col_HTML_AQI_US)
    }
    result <- data.frame(
      raw=x,
      AQI_US=index_raw,
      AQI_US_level=factor(index_label, levels=names(norm_AQI_US)),
      AQI_US_color=col_factor
    )
    rownames(result) <- NULL
    colnames(result) <- paste0(colnames(result), "_", gsub("\\.", "_", norm))
    return(result)
  }
}
