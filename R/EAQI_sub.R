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
#' @section Colors: The data frame contains rows to support coloring tables or plots, e.g. if you use the data in
#'    an RMarkdown document. If \code{latex_cellcolors=FALSE}, they provide a \code{<span>} with hexadecimal HTML color codes
#'    for each index value. Otherwise, the factor labels are in LaTeX format using the \code{\\cellcolor} command.
#'    This is useful, e.g., if combined with the \code{kableExtra::kable} function. To use the code out of the box,
#'    define the following colors in your preamble (example shows usage in a YAML header):
#'    
#' \preformatted{
#' header-includes:
#'    - \definecolor{EAQIep}{HTML}{7D2181}
#'    - \definecolor{EAQIvp}{HTML}{960032}
#'    - \definecolor{EAQIp}{HTML}{FF5050}
#'    - \definecolor{EAQIm}{HTML}{F0E641}
#'    - \definecolor{EAQIf}{HTML}{50CCAA}
#'    - \definecolor{EAQIg}{HTML}{50F0E6}
#' }
#'
#' @param x A numeric vector of particulate matter measurements (µg/m³).
#' @param norm One value of either "PM10" or "PM2.5", setting the PM size of the given values of \code{x}.
#' @param latex_cellcolors Logical, if true the "EAQI_color_*" columns will show LaTeX \code{\\cellcolor} code instead
#'    of HTML colors in a \code{<span>}. See \code{Colors} section.
#' @return If \code{raw=TRUE}, a numeric vector with the raw EAQI values. Otherwise a data frame with
#'    four columns: raw input data, EAQI level (factor), recommended color code (factor).
#'    The column names start with "raw_", "EAQI_level_", and "EAQI_color_", and end with either "PM10" or
#'    "PM2_5", depending on the value of \code{norm}.
#' @export
#' @examples
#' EAQI_sub(55)

EAQI_sub <- function(
  x,
  norm="PM10",
  latex_cellcolors=TRUE
){
  index_label <- switch(norm,
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

  col_HTML_EAQI <- c(
    "extremely poor"="<div style=\"     background-color: #7d2181 !important; color: #ffffff !important;\" >&nbsp;extremely poor&nbsp;</div>",
    "very poor"="<div style=\"     background-color: #960032 !important; color: #ffffff !important;\" >&nbsp;very poor&nbsp;</div>",
    "poor"="<div style=\"     background-color: #ff5050 !important;\" >&nbsp;poor&nbsp;</div>",
    "moderate"="<div style=\"     background-color: #f0e641 !important;\" >&nbsp;moderate&nbsp;</div>",
    "fair"="<div style=\"     background-color: #50ccaa !important;\" >&nbsp;fair&nbsp;</div>",
    "good"="<div style=\"     background-color: #50f0e6 !important;\" >&nbsp;good&nbsp;</div>"
  )
  col_LaTeX_EAQI <- c(
    "extremely poor"="\\cellcolor{EAQIep}\\textcolor{white}{extremely poor}",
    "very poor"="\\cellcolor{EAQIvp}\\textcolor{white}{very poor}",
    "poor"="\\cellcolor{EAQIp}poor",
    "moderate"="\\cellcolor{EAQIm}moderate",
    "fair"="\\cellcolor{EAQIf}fair",
    "good"="\\cellcolor{EAQIg}good"
  )
  if(isTRUE(latex_cellcolors)){
    col_factor <- factor(col_LaTeX_EAQI[index_label], levels=col_LaTeX_EAQI)
  } else {
    col_factor <- factor(col_HTML_EAQI[index_label], levels=col_HTML_EAQI)
  }

  result <- data.frame(
    raw=x,
    EAQI_level=factor(index_label, levels=names(col_HTML_EAQI)),
    EAQI_color=col_factor
  )
  colnames(result) <- paste0(colnames(result), "_", gsub("\\.", "_", norm))

  return(result)
}
