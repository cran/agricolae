\name{audpc.abs}
\alias{audpc.abs}
%- audpc.abs.
\title{ Calculates the absolute AUDPC of a table }
\description{
   Area under the curve of the progress of the disease. The AUDPC measures 
   the disease throughout a period. The AUDPC is the area
   that is determined by the sum of trapezes under the curve.
}
\usage{
audpc.abs(evaluation, dates)
}

\arguments{
  \item{evaluation}{ Table of data of the evaluations: Data frame
 }
  \item{dates}{ Vector of dates corresponding to each evaluation }
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
}
\references{ Campbell, C. L., L. V. Madden. (1990): Introduction to Plant Disease Epidemiology. John Wiley & Sons, New York City. }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{audpc.rel}} }

\examples{
library(agricolae)
dates<-c(14,21,28)
evaluation<-data.frame(E1=40,E2=80,E3=90)
# It calculates audpc absolute
audpc<-audpc.abs(evaluation,dates)
print(audpc)
rm(evaluation, dates, audpc)
# Another case
data(disease)
attach(disease)
dates<-c(1,2,3)
evaluation<-disease[,c(4,5,6)]
# It calculates audpc absolute
audpc<-audpc.abs(evaluation, dates)
# datos para analisis
data2<-data.frame(disease, audpc=audpc)
# Correlation between the yield and audpc
attach(data2)
cor.vector(yield, audpc, method="kendall")
}
\keyword{ manip }% at least one, from doc/KEYWORDS

