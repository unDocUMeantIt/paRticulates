#' Plot air quality data from an airData object
#' 
#' Plots objects of class airData.
#' 
#' @param data A data frame as returned by read.madavi().
#' @param main Title of the plot.
#' @param start Start time in POSIXct format, or as a character string in "\%Y-\%m-\%d \%H:\%M:\%S" format. If missing all data is plotted.
#' @param end End time in POSIXct format, or as a character string in "\%Y-\%m-\%d \%H:\%M:\%S" format. If missing all data is plotted.
#' @param sub Subtitle
#' @param xlab A title for the x axis (time).
#' @param ylab1 A title for the upper y axis (humidity).
#' @param ylab2 A title for the lower y axis (fine dust).
#' @param zlab A title for the upper z axis (temperature).
#' @param legend Text for the plot legend describing humidity, temperature, PM10, and PM2.5.
#' @param colors Four color definitions used to plot humidity, temperature, PM10, and PM2.5, respectively.
#' @export
#' @examples
#' \dontrun{
#' x <- read.madavi(file.path("~","fine_dust_data"))
#' plot.airData(
#'   x,
#'   start="2019-12-10 09:00:00",
#'   end="2019-12-10 22:00:00"
#' )
#' }

plot.airData <- function(
  data,
  main="Feinstaubkonzentration",
  start=NA,
  end=NA,
  sub=paste0("Datum: ", unique(format(data[["TimeLocal"]], "%d.%m.%Y"))),
  xlab="Tageszeit",
  ylab1="Luftfeuchtigkeit",
  ylab2="Feinstaub (µg/m³)",
  zlab="Temperatur",
  legend=c("LF", "Temp", "PM10","PM2.5"),
  colors=c("blue", "red", "darkgreen", "green")
){
  if(is.character(start)){
    start <- as.POSIXct(strptime(start, format="%Y-%m-%d %H:%M:%S"))
  } else {}
  if(is.character(end)){
    end <- as.POSIXct(strptime(end, format="%Y-%m-%d %H:%M:%S"))
  } else {}
  if(all(inherits(start, "POSIXct"), inherits(end, "POSIXct"))){
    data <- data[data[["TimeLocal"]] >= start & data[["TimeLocal"]] <= end, ]
  } else {}
  par(
    mfrow=c(2,1),
    mar=c(0.8, 4.1, 4.1, 8.1),
    las=1
  )
  plot(
    Humidity ~ TimeLocal,
    data,
    col=colors[1],
    type="l",
    axes=FALSE,
    bty="n",
    main=main,
    xlab="",
    ylab=ylab1
  )
  axis_points_humid <- pretty(range(data[["Humidity"]]))
  axis(
    side=2,
    at=axis_points_humid,
    labels=paste0(axis_points_humid, "%"),
    cex.axis=0.8
  )
  par(new = TRUE)
  plot(
    Temp ~ TimeLocal,
    data,
    col=colors[2],
    type="l",
    axes=FALSE,
    bty="n",
    xlab="",
    ylab=""
  )
  axis_points_temp <- pretty(range(data[["Temp"]]))
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
    SDS_P1 ~ TimeLocal,
    data,
    ylim=range(data[["SDS_P1"]], data[["SDS_P2"]], na.rm=TRUE),
    type="l",
    bty="n",
    col=colors[3],
    sub=sub,
    xlab=xlab,
    ylab=ylab2,
    cex.axis=0.8
  )
  lines(
    SDS_P2 ~ TimeLocal,
    data,
    col=colors[4]
  )
  legend(
    "topright",
    col=colors,
    inset=c(-0.17, 0.3),
    lty=1,
    legend=legend,
    bty="n"
  )
}
