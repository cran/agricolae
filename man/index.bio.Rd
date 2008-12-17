\name{index.bio}
\alias{index.bio}
\title{ Biodiversity Index }
\description{
  Scientists use a formula called the biodiversity index to describe the amount of
species diversity in a given area.
}
\usage{
index.bio(data, method = c("Margalef", "Simpson.Dom", "Simpson.Div", "Berger.Parker", "McIntosh", "Shannon"), level=95, nboot=500, console=TRUE)
}
\arguments{
  \item{data}{ number of specimens }
  \item{method}{ Describe method bio-diversity }
  \item{level}{ Significant level }
  \item{nboot}{ size bootstrap }
  \item{console}{ output console TRUE}
}
\details{
  method bio-diversity.
  "Margalef"
  "Simpson.Dom"
  "Simpson.Div"
  "Berger.Parker"
  "McIntosh"
  "Shannon"
}
\value{
  \item{data }{ vector }
  \item{method }{ method bio-diversity }
  \item{level }{ value 0-100 percentage }
  \item{nboot }{ size 100, 500,... }
}
\references{Efron, B., Tibshirani, R. (1993) An Introduction to the Boostrap. Chapman and Hall/CRC }
\author{ Felipe de Mendiburu }
\examples{
library(agricolae)
data(paracsho)
# date 22-06-05 and treatment CON = application with insecticide
specimens <- paracsho[1:10,6]
output1 <- index.bio(specimens,method="Simpson.Div",level=95,nboot=200)
output2 <- index.bio(specimens,method="Shannon",level=95,nboot=200)
rbind(output1, output2)
}
\keyword{ univar }
