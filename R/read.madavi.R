#' Import CSV data as provided by madavi.de
#' 
#' @param path Either a path to a single CSV file, or to a directory which is then being scanned for CSV files.
#' @param dropEmptyCols Logical, whether to drop all columns without any data.
#' @param tz Time zone, used to calculate local time from the UTC raw data.
#' @export
#' @examples
#' \dontrun{
#' # import single CSV file ~/fine_dust_data/data-esp8266-1234567-2019-12-10.csv
#' x <- read.madavi(file.path("~","fine_dust_data","data-esp8266-1234567-2019-12-10.csv"))
#'
#' # import all CSV files from directory ~/fine_dust_data
#' x <- read.madavi(file.path("~","fine_dust_data"))
#' }

read.madavi <- function(
  path,
  dropEmptyCols=TRUE,
  tz="Europe/Berlin"
){
  if(file_test("-d", path)){
    CSV_files <- list.files(path, pattern="*.[cC][sS][vV]", full.names=TRUE)
    # TODO: simplify this to avoid rbind()
    file_data_list <- lapply(
      CSV_files,
      function(this_file){
        return(read.madavi(
          path=this_file,
          dropEmptyCols=FALSE,
          tz=tz
        ))
      }
    )
    file_data <- file_data_list[[1]]
    if(length(file_data_list) > 1){
      for(this_df in file_data_list[-1]){
        file_data <- rbind(file_data, this_df)
      }
    } else {}
  } else if(file_test("-f", path)) {
    file_data <- read.csv2(
      file=path,
      dec='.',
      row.names=NULL,
      colClasses = c(
        "POSIXct",
        rep("numeric", 23)
      ),
      strip.white = TRUE,
      stringsAsFactors=FALSE
    )
  } else {
    stop(simpleError(paste0("Data can't be found:\n  ", path)))
  }
  if(isTRUE(dropEmptyCols)){
    no_data <- apply(file_data, 2, function(this_col) all(is.na(this_col)))
    file_data <- file_data[, !no_data]
  } else {}
  # convert UTC to local time
  file_data[["TimeLocal"]] <- as.POSIXct(format(file_data[["Time"]], tz="UTC", usetz=TRUE), tz=tz)
  return(file_data)
}
