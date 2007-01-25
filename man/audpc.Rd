\name{audpc}
\alias{audpc}
%- audpc.
\title{ Calculating the absolute or relative value of the AUDPC }
\description{
   Area Under Disease Progress Curve. The AUDPC measures the disease throughout a 
   period. The AUDPC is the area that is determined by the sum of trapezes under 
   the curve.
}
\usage{
audpc(evaluation, dates, type = "absolute")
}

\arguments{
  \item{evaluation}{ Table of data of the evaluations: Data frame }
  \item{dates}{ Vector of dates corresponding to each evaluation }
  \item{type}{ relative, absolute }
}
\details{
     AUDPC. For the illustration one considers three evaluations
     (14, 21 and 28 days) and percentage of damage in the plant 40, 80 and 90
     (interval between dates of evaluation 7 days).  AUDPC = 1045.
     The evaluations can be at different interval.
}
\value{

  \item{evaluation }{data frame}
  \item{dates }{a numeric vector}
  \item{type }{text}
}
\references{ Campbell, C. L., L. V. Madden. (1990): Introduction to Plant Disease Epidemiology. John Wiley & Sons, New York City. }
\author{ Felipe de Mendiburu }

\examples{
library(agricolae)
# example 1
dates<-c(14,21,28)
evaluation<-data.frame(E1=40,E2=80,E3=90)
# It calculates audpc absolute
absolute<-audpc(evaluation,dates,type="absolute")
print(absolute)
rm(evaluation, dates, absolute)
# example 2
data(disease)
attach(disease)
dates<-c(1,2,3)
evaluation<-disease[,c(4,5,6)]
# It calculates audpc relative
indice<-audpc(evaluation, dates, type = "relative")
# datos para analisis
data2<-data.frame(disease, audpc=indice)
# Correlation between the yield and audpc
attach(data2)
correlation(yield, audpc, method="kendall")
}
\keyword{ manip }% at least one, from doc/KEYWORDS

