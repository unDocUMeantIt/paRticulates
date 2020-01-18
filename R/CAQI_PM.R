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
#' The function returns the respective hourly CAQI grading, as defined by the Common Information to European Air (CITEAIR and CITEAIR II) projects[1].
#' 
#' The index has five levels, named "very low" (best air quality), "low", "medium", "high", and "very high" (worst air quality).
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
#'    - \definecolor{CAQIvh}{HTML}{960018}
#'    - \definecolor{CAQIh}{HTML}{F29305}
#'    - \definecolor{CAQIm}{HTML}{EEC20B}
#'    - \definecolor{CAQIl}{HTML}{BBCF4C}
#'    - \definecolor{CAQIvl}{HTML}{79BC6A}
#' }
#' 
#' @param x A numeric vector of particulate matter measurements (µg/m³).
#' @param norm One value of either "PM10" or "PM2.5", setting the PM size of the given values of \code{x}.
#' @param raw Logical, if true returns only the CAQI raw values.
#' @param latex_cellcolors Logical, if true the "CAQI_color_*" columns will show LaTeX \code{\\cellcolor} code instead
#'    of HTML colors. See \code{Colors} section.
#' @return If \code{raw=TRUE}, a numeric vector with the raw CAQI values. Otherwise a data frame with
#'    four columns: raw input data, CAQI value, CAQI level (factor), recommended color code (factor).
#'    The column names start with "raw_", "CAQI_", "CAQI_level_", and "CAQI_color_", and end with either "PM10" or
#'    "PM2_5", depending on the value of \code{norm}.
#' @seealso
#'    \code{\link[EAQI_PM]{EAQI_PM}}, \code{\link[AQI_US_PM]{AQI_US_PM}}
#' @references
#'    [1] \url{https://www.airqualitynow.eu/about_indices_definition.php}
#' @export
#' @examples
#' CAQI_PM(55)

CAQI_PM <- function(
  x,
  norm="PM10",
  raw=FALSE,
  latex_cellcolors=TRUE
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
    col_HTML_CAQI <- c(
      "very high"="<div style=\"     background-color: #960018 !important; color: #ffffff !important;\" >&nbsp;very high&nbsp;</div>",
      "high"="<div style=\"     background-color: #f29305 !important;\" >&nbsp;high&nbsp;</div>",
      "medium"="<div style=\"     background-color: #eec20b !important;\" >&nbsp;medium&nbsp;</div>",
      "low"="<div style=\"     background-color: #bbcf4c !important;\" >&nbsp;low&nbsp;</div>",
      "very low"="<div style=\"     background-color: #79bc6a !important;\" >&nbsp;very low&nbsp;</div>"
    )
    col_LaTeX_CAQI <- c(
      "very high"="\\cellcolor{CAQIvh}\\textcolor{white}{very high}",
      "high"="\\cellcolor{CAQIh}high",
      "medium"="\\cellcolor{CAQIm}medium",
      "low"="\\cellcolor{CAQIl}low",
      "very low"="\\cellcolor{CAQIvl}very low"
    )
    index_label <- sapply(
      index_raw,
      function(x){
        names(norm_CAQI)[min(which(x > norm_CAQI))]
      }
    )
    if(isTRUE(latex_cellcolors)){
      col_factor <- factor(col_LaTeX_CAQI[index_label], levels=col_LaTeX_CAQI)
    } else {
      col_factor <- factor(col_HTML_CAQI[index_label], levels=col_HTML_CAQI)
    }
    result <- data.frame(
      raw=x,
      CAQI=index_raw,
      CAQI_level=factor(index_label, levels=names(norm_CAQI)),
      CAQI_color=col_factor
    )
    rownames(result) <- NULL
    colnames(result) <- paste0(colnames(result), "_", gsub("\\.", "_", norm))
    return(result)
  }
}
