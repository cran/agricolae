\name{audpc.rel}
\alias{audpc.rel}
%- audpc.rel.
\title{ Calculates the relative AUDPC of a table }
\description{
  Area under the curve of the progress of the disease. The relative 
  AUDPC does not have units and is used when joint analysis
  of varieties is made in different environment where the dates
  and the intervals of evaluation change. The relative AUDPC is the value
  between the absolute AUDPC and the total area of evaluation multiplied 
  by the time interval of evaluation in percent.
}
\usage{
audpc.rel(evaluation, dates)
}

\arguments{
  \item{evaluation}{ Table of evaluations }
  \item{dates}{ Dates of evaluation }
}
\details{
  If evaluated from day 14 to day 28 is 14 days of evaluation, then the
  total area of evaluation will be: 100x14=1400
  If AUDPC = 1015 then AUDPC.rel = 1015/1400 = 0.725.
  
}
\value{

  \item{evaluation }{data frame}
  \item{dates }{a numeric vector}

}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{audpc.abs}} }

\examples{

library(agricolae)
# Example 1
dates<-c(14,21,28)
evaluation<-data.frame(E1=40,E2=80,E3=90)
# Audpc relative
relative<-audpc.rel(evaluation,dates)
print(relative)
rm(relative, evaluation,dates)
# Example 2
data(disease)
attach(disease)
dates<-c(1,2,3)
evaluation<-disease[,c(4,5,6)]
# Audpc relative
audpc<-audpc.rel(evaluation,dates)
# data for analysis.
data2<-data.frame(disease,relative=audpc)
# correlation between the yield and the AUDPC
attach(data2)
cor.vector(yield, relative, method="kendall")
rm(audpc, dates, data2)
}
\keyword{ manip }% at least one, from doc/KEYWORDS

