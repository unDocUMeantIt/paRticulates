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

#' Get hourly means and air quality indices
#' 
#' The hour value is to be interpreted as the starting point of measurement until the next full hour.
#' this means, if you are looking for the past hour, you need to add 1 to the hour values.
#' 
#' @section Colors: The data frame contains rows to support coloring tables or plots, e.g. if you use the data in
#'    an RMarkdown document. If \code{latex_cellcolors=FALSE}, they simply provide hexadecimal HTML color codes
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
#'    - \definecolor{EAQIep}{HTML}{7D2181}
#'    - \definecolor{EAQIvp}{HTML}{960032}
#'    - \definecolor{EAQIp}{HTML}{FF5050}
#'    - \definecolor{EAQIm}{HTML}{F0E641}
#'    - \definecolor{EAQIf}{HTML}{50CCAA}
#'    - \definecolor{EAQIg}{HTML}{50F0E6}
#' }
#'
#' @param x An object of class \code{\link[paRticulates:airData-class]{airData}}.
#' @param time An optional vector with two elements, start and end time, either in character
#'    format supported by \code{\link[as.POSIXct]{as.POSIXct}}, or in POSIXct format.
#' @param calc Character vector of air quality indices to calculate.
#'    Currently, "CAQI" and "EAQI" are supported.
#' @param latex_cellcolors Logical, if true the "*_color_*" columns will show LaTeX \code{\\cellcolor} code instead
#'    of HTML colors. See \code{Colors} section.
#' @return A data frame as returned by \code{\link[paRticulates:layout_df]{layout_df}}, but reduced to hourly means.
#' @rdname aq_hourly-methods
#' @docType methods
#' @export
setGeneric("aq_hourly", function(x, time=c(), ...) standardGeneric("aq_hourly"))

#' @rdname aq_hourly-methods
#' @docType methods
#' @export
#' @aliases
#'    aq_hourly,-methods
#'    aq_hourly,airData-method
setMethod("aq_hourly",
  signature=signature(x="airData"),
  function(
    x,
    time=c(),
    calc=c("CAQI", "EAQI"),
    latex_cellcolors=TRUE
  ){
    raw_df <- layout_df(x, time=time)
    numeric_cols <- sapply(raw_df, is.numeric)
    raw_df[["time"]] <- as.factor(format(raw_df[["time"]], "%Y-%m-%d %H:00"))
    result <- as.data.frame(
      sapply(
        raw_df[, numeric_cols],
        function(x){
          as.vector(by(x, raw_df[["time"]], mean, na.rm=TRUE))
        }
      )
    )
    result[["time"]] <- unique(raw_df[["time"]])

    if("CAQI" %in% calc){
      if("PM10" %in% colnames(result)){
        CAQI10 <- CAQI_PM(result[["PM10"]], norm="PM10", latex_cellcolors=latex_cellcolors)
        result <- cbind(result, CAQI10[, !colnames(CAQI10) %in% "raw_PM10"])
      } else {}
      if("PM2_5" %in% colnames(result)){
        CAQI2_5 <- CAQI_PM(result[["PM2_5"]], norm="PM2.5", latex_cellcolors=latex_cellcolors)
        result <- cbind(result, CAQI2_5[, !colnames(CAQI2_5) %in% "raw_PM2_5"])
      } else {}
    } else {}

    if("EAQI" %in% calc){
      if("PM10" %in% colnames(result)){
        EAQI10 <- EAQI_PM(result[["PM10"]], norm="PM10", latex_cellcolors=latex_cellcolors)
        result <- cbind(result, EAQI10[, !colnames(EAQI10) %in% "raw_PM10"])
      } else {}
      if("PM2_5" %in% colnames(result)){
        EAQI2_5 <- EAQI_PM(result[["PM2_5"]], norm="PM2.5", latex_cellcolors=latex_cellcolors)
        result <- cbind(result, EAQI2_5[, !colnames(EAQI2_5) %in% "raw_PM2_5"])
      } else {}
    } else {}

    return(result)
  }
)
