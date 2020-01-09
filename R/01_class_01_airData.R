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

#' S4 Class airData
#' 
#' Used to provide a single object class for various sources of sensor data.
#' For instance, CSV data obtained from madavi.de or luftdaten.info has a
#' different format even if the same device was used.
#' 
#' @section Contructor function:
#' Should you need to manually generate objects of this class (which should rarely be
#' the case), the contructor function \code{airData(...)} can be used instead of
#' \code{new("airData", ...)}.
#'
#' @slot data List of data frames containing all raw data.
#' @slot layout Data frame with two columns, \code{obj}, \code{data}, and \code{time},
#'    and rows named after the type of data measured (e.g., \code{PM10}, \code{PM2_5},
#'    \code{temperature}, \code{humidity}). For each type, it defines in which
#'    data frame from the \code{data} slot the actual data can be found, and how the
#'    column with the time data is called.
#' @name airData,-class
#' @aliases airData-class
#' @import methods
#' @keywords classes
#' @export airData
#' @exportClass airData
#' @rdname airData-class

airData <- setClass("airData",
  representation=representation(
    data="list",
    layout="data.frame"
  ),
  prototype(
    data=list(),
    layout=data.frame(
      obj=character(),
      data=character(),
      time=character(),
      stringsAsFactors=FALSE
    )
  )
)

setValidity(
  "airData",
  function(object){
    data <- slot(object, "data")
    layout <- slot(object, "layout")
    
    if(length(data) > 0){
      if(!all(sapply(data, is.data.frame))){
        stop("Invalid object: All elements of \"data\" must be data frames!")
      }
    }
    ## TODO: check whether all columns in 'layout' do exist
    ## TODO: check if time codes are matching across data frames
  }
)
