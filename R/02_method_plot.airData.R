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

#' Plot air quality data from an \code{airData} object
#' 
#' Plots objects of class \code{\link[paRticulates:airData-class]{airData}}.
#' 
#' @param x An object of class \code{\link[paRticulates:airData-class]{airData}}, like returned by read.madavi().
#' @param main Title of the plot.
#' @param start Start time in POSIXct format, or as a character string supported by \code{\link[as.POSIXct]{as.POSIXct}}. If missing all data is plotted.
#' @param end End time in POSIXct format, or as a character string supported by \code{\link[as.POSIXct]{as.POSIXct}}. If missing all data is plotted.
#' @param sub Subtitle
#' @param xlab A title for the x axis (time).
#' @param ylab1 A title for the upper y axis (humidity).
#' @param ylab2 A title for the lower y axis (fine dust).
#' @param zlab A title for the upper z axis (temperature).
#' @param legend Text for the plot legend describing humidity, temperature, PM10, PM2.5, humidity over 70\%, and particle concentration below 50 µg/m³.
#' @param colors Six color definitions used to plot humidity, temperature, PM10, PM2.5, humidity over 70\%, and particle concentration below 50 µg/m³, respectively.
#' @param ... Additional options applied to \code{plot} and \code{lines}.
#' @export
#' @keywords methods plot
#' @docType methods
#' @rdname plot-methods
#' @examples
#' \dontrun{
#' x <- read.madavi(file.path("~","fine_dust_data"))
#' plot(
#'   x,
#'   start="2019-12-10 09:00:00",
#'   end="2019-12-10 22:00:00"
#' )
#' }
setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

#' @export
#' @docType methods
#' @rdname plot-methods
#' @aliases plot,airData,missing-method
#' @include 01_class_01_airData.R
#' @import graphics
setMethod(
  "plot",
  signature(
    x="airData",
    y="missing"
  ),
  function(
    x,
    main="Particulates",
    start=NA,
    end=NA,
    sub=paste0("Date: "),
    xlab="Time of day",
    ylab1="Humidity",
    ylab2="Particulates (µg/m³)",
    zlab="Temperature",
    legend=c("Humid", "Temp", "PM10","PM2.5", "Humid > 70%", "< 50 µg/m³"),
    colors=c("blue", "red", "darkgreen", "purple", "#FFDDDD", "#DDFFDD"),
    ...
  ){

    data <- layout_df(x, time=c(start, end))

    par(
      mfrow=c(2,1),
      mar=c(0.8, 4.1, 4.1, 8.1),
      las=1
    )
    plot(
      humidity ~ time,
      data,
      col=colors[1],
      type="l",
      axes=FALSE,
      bty="n",
      main=main,
      xlab="",
      ylab=ylab1,
      lty="blank",
      ...
    )
    axis_points_humid <- pretty(range(data[["humidity"]]))
    axis(
      side=2,
      at=axis_points_humid,
      labels=paste0(axis_points_humid, "%"),
      cex.axis=0.8
    )
    if(max(axis_points_humid) > 70){
      rect(
        xleft=data[1,"time"],
        ybottom=70,
        xright=data[nrow(data),"time"],
        ytop=max(axis_points_humid),
        col=colors[5],
        border=NA
      )
    } else {}
    lines(
      humidity ~ time,
      data,
      col=colors[1],
      ...
    )
    par(new = TRUE)
    plot(
      temperature ~ time,
      data,
      col=colors[2],
      type="l",
      axes=FALSE,
      bty="n",
      xlab="",
      ylab="",
      ...
    )
    axis_points_temp <- pretty(range(data[["temperature"]]))
    axis(
      side=4,
      at=axis_points_temp,
      labels=paste0(axis_points_temp, "° C"),
      cex.axis=0.8
    )
    mtext(zlab, side=4, line=4, las=0)
    par(
      mar=c(5.1, 4.1, 0, 8.1),
      xpd=TRUE
    )
    plot(
      PM10 ~ time,
      data,
      ylim=range(0, data[["PM10"]], data[["PM2_5"]], na.rm=TRUE),
      type="l",
      bty="n",
      col=colors[3],
      sub=sub,
      xlab=xlab,
      ylab=ylab2,
      cex.axis=0.8,
      lty="blank", # don't draw yet, get rectangle with threshold first
      ...
    )
    rect(
      xleft=data[1,"time"],
      ybottom=0,
      xright=data[nrow(data),"time"],
      ytop=50,
      col=colors[6],
      border=NA
    )
    lines(
      PM10 ~ time,
      data,
      col=colors[3],
      ...
    )
    lines(
      PM2_5 ~ time,
      data,
      col=colors[4],
      ...
    )
    legend(
      "topright",
      fill=colors,
      border=NA,
      inset=c(-0.17, 0.3),
      legend=legend,
      bty="n"
    )
  }
)
