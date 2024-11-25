#' Read a File
#'
#' @param csv the name of the csv file to be read
#' @param dird the directory where the file is stored
#'
#' @return the last line (data frame)
#'
#' @examples
#' seed <- "SEEDLING.csv"
#' seed.df <- myread("SEEDLING.csv", "C:/Users/jaide/MATH4753/DATA/")
#'
#' @importFrom utils read.table
#'
#' @export
myread = function(csv, dird) {
  fl = paste(dird, csv, sep = "")
  read.table(fl, header = TRUE, sep = ",")
}
