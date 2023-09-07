## finds all .R and .r files within a folder and sources them
sourceFolder <- function(folder, recursive = FALSE, ...) 
{ 
  files <- list.files(folder, pattern = "[.][rR]$", 
                      full.names = TRUE, recursive = recursive)
  if (!length(files))
    stop(simpleError(sprintf('No R files in folder "%s"', folder)))
  src <- invisible(lapply(files, source, ...))
  message(sprintf('%s files sourced from folder "%s"', length(src), folder))
}
sourceFolder("c:\\Users\\jensk\\Work\\01_Projects\\210301bsu_DFG_RESIST\\01_Definition\\Lehre\\SEWAMM\\TrainTheTrainers_2022\\iha_software\\EflowStats\\R")

x <- data.frame(
  date = seq.Date(from = as.Date('1999-01-01'), to = as.Date('2003-12-31'), by = 'days'),
  discharge = seq.int(1,length(seq.Date(from = as.Date('1999-01-01'), to = as.Date('2003-12-31'), by = 'days')))
)

yearType <- 'water'
for (wy_month in 1:12){
  x_cut <- cut_dataToWaterYear(x, wy_month)
  cat("wyMonth =", wy_month, ":\n")
  print(as.data.frame(table(get_waterYear(x_cut$date, wy_month))))
  cat("----------------\n")
  if (length(validate_data(x_cut, yearType, wy_month))>1){
    cat("Data is valid\n\n\n")
  }else{
    cat("Data is NOT valid\n\n\n")
  }
}

