% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myread.R
\name{myread}
\alias{myread}
\title{Read a File}
\usage{
myread(csv, dird)
}
\arguments{
\item{csv}{the name of the csv file to be read}

\item{dird}{the directory where the file is stored}
}
\value{
the last line (data frame)
}
\description{
Read a File
}
\examples{
seed <- "SEEDLING.csv"
seed.df <- myread("SEEDLING.csv", "C:/Users/jaide/MATH4753/DATA/")

}
