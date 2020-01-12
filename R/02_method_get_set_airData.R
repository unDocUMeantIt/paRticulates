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

#' Getter and setter methods for airData objects
#' 
#' \itemize{
#'   \item{\code{air_data()}/\code{air_data()<-} }{Get/set the \code{data} slot.}
#'   \item{\code{air_layout()}/\code{air_layout()<-} }{Get/set the \code{layout} slot.}
#'   \item{\code{layout_df()} }{Returns a single data frame as described by the \code{layout} slot.}
#'   \item{\code{[}/\code{[[} }{Can be used as a shortcut to directly access the data as described by the \code{layout} slot.}
#' }
#' @param x An object of class \code{airData}.
#' @param value The new value to replace the current with.
#' @param i Defines the row selector (\code{[}) or the name to match (\code{[[}).
#' @param j Defines the column selector.
#' @param time An optional vector with two elements, start and end time, either in character
#'    format supported by \code{\link[as.POSIXct]{as.POSIXct}}, or in POSIXct format.
# @param tz A valid timezone as used by \code{\link[as.POSIXct]{as.POSIXct}}.
#' @param ... Additional arguments for the generics.
#' @rdname airData_get-methods
#' @docType methods
#' @export
# @examples
# \dontrun{
# 
# }
setGeneric("air_data", function(x, ...) standardGeneric("air_data"))
#' @rdname airData_get-methods
#' @docType methods
#' @export
#' @aliases
#'    air_data,-methods
#'    air_data,airData-method
setMethod("air_data",
  signature=signature(x="airData"),
  function(x){
    return(slot(x, name="data"))
  }
)

#' @rdname airData_get-methods
#' @docType methods
#' @export
setGeneric("air_data<-", function(x, value) standardGeneric("air_data<-"))
#' @rdname airData_get-methods
#' @docType methods
#' @export
#' @aliases
#'    air_data<-,-methods
#'    air_data<-,airData-method
setMethod("air_data<-",
  signature=signature(x="airData"),
  function(x, value){
    slot(x, name="data") <- value
    return(x)
  }
)


#' @rdname airData_get-methods
#' @docType methods
#' @export
setGeneric("air_layout", function(x, value) standardGeneric("air_layout"))
#' @rdname airData_get-methods
#' @docType methods
#' @export
#' @aliases
#'    air_layout,-methods
#'    air_layout,airData-method
setMethod("air_layout",
  signature=signature(x="airData"),
  function(x, value){
    return(slot(x, name="layout"))
  }
)

#' @rdname airData_get-methods
#' @docType methods
#' @export
setGeneric("air_layout<-", function(x, value) standardGeneric("air_layout<-"))
#' @rdname airData_get-methods
#' @docType methods
#' @export
#' @aliases
#'    air_layout<-,-methods
#'    air_layout<-,airData-method
setMethod("air_layout<-",
  signature=signature(x="airData"),
  function(x, value){
    slot(x, name="layout") <- value
    return(x)
  }
)


#' @rdname airData_get-methods
#' @docType methods
#' @export
setGeneric("layout_df", function(x, time=c(), ...) standardGeneric("layout_df"))
#' @rdname airData_get-methods
#' @docType methods
#' @export
#' @aliases
#'    layout_df,-methods
#'    layout_df,airData-method
setMethod("layout_df",
  signature=signature(x="airData"),
  function(x, time=c()){
    raw_data <- air_data(x)
    data_layout <- air_layout(x)
    data_available <- rownames(data_layout)
    result <- lapply(
      data_available,
      function(this_col){
        raw_data[[data_layout[this_col, "obj"]]][, data_layout[this_col, "data"]]
      }
    )
    names(result) <- data_available

    time_available <- unique(data_layout[, c("obj", "time")])
    # check for consistent time indices
    all_times <- unique(lapply(
      seq_along(time_available[["obj"]]),
      function(this_df){
        raw_data[[time_available[this_df, "obj"]]][, time_available[this_df, "time"]]
      }
    ))
    
    if(length(all_times) > 1){
      stop(simpleError("Time data is not consistent, it is currently not supported to combine such sensor data."))
    } else {
      result[["time"]] <- all_times[[1]]
    }
    
    result <- as.data.frame(result, stringsAsFactors=FALSE)
    if(isTRUE(length(time) == 2)){
      time_POSIX <- lapply(
        time,
        function(this_time){
          if(is.character(this_time)){
            return(as.POSIXct(this_time))
          } else if(inherits(this_time, "POSIXct")){
            return(this_time)
          } else {
            stop(simpleError("\"time\" must either be in POSIXct format or a character vector that can eb coerced into POSIXct!"))
          }
        }
      )
      result <- result[result[["time"]] >= time_POSIX[[1]] & result[["time"]] <= time_POSIX[[2]], ]
    } else {}
    return(result)
  }
)


#' @rdname airData_get-methods
#' @export
#' @docType methods
#' @aliases
#'    [,-methods
#'    [,airData,ANY,ANY-method
setMethod("[",
  signature=signature(x="airData"),
  function (x, i, j, time=c()){
    return(layout_df(x, time=time)[i, j])
  }
)

#' @rdname airData_get-methods
#' @export
#' @docType methods
#' @aliases
#'    [<-,-methods
#'    [<-,airData,ANY,ANY,ANY-method
setMethod("[<-",
  signature=signature(x="airData"),
  function (x, i, j, time=c(), value){
    layout_df(x, time=time)[i, j] <- value
    return(x)
  }
)

#' @rdname airData_get-methods
#' @export
#' @docType methods
#' @aliases
#'    [[,-methods
#'    [[,airData,ANY-method
setMethod("[[",
  signature=signature(x="airData"),
  function (x, i, time=c()){
    return(layout_df(x, time=time)[[i]])
  }
)

#' @rdname airData_get-methods
#' @export
#' @docType methods
#' @aliases
#'    [[<-,-methods
#'    [[<-,airData,ANY,ANY-method
setMethod("[[<-",
  signature=signature(x="airData"),
  function (x, i, time=c(), value){
    layout_df(x, time=time)[[i]] <- value
    return(x)
  }
)
