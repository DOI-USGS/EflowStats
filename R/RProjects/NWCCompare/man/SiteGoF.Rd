\name{SiteGoF}
\alias{SiteGoF}
\title{Function to calculate GoF statistics for given observed and modeled data sets}
\usage{
  SiteGoF(Gaged, Modeled)
}
\arguments{
  \item{Gaged}{data frame of daily flow data}

  \item{Modeled}{data frame of daily flow data}
}
\value{
  Output data frame of calculated statistics
}
\description{
  This function accepts observed and modeled data frames of
  daily flow data and returns a data frame of calculated
  GoF statistics
}
\examples{
load_data<-paste(system.file(package="NWCCompare"),"/data/qfiletempf.csv",sep="")
load_mod<-paste(system.file(package="NWCCompare"),"/data/qfiletempf.csv",sep="")
Gaged<-read.csv(load_data,stringsAsFactors=FALSE)
Modeled<-read.csv(load_mod,stringsAsFactors=FALSE)
SiteGoF(Gaged,Modeled)
}

