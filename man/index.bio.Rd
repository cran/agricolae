\name{index.bio}
\alias{index.bio}
%- index.bio.
\title{ Biodiversity Indices }
\description{
Scientists use a formula called the biodiversity index to describe the amount of
species diversity in a given area.

}
\usage{
index.bio(data, method = "Margalef", level = 95, Boot = 100)
}

\arguments{
  \item{data}{ number of specimens  }
  \item{method}{ Describe method bio-diversity }
  \item{level}{ Significant level }
  \item{Boot}{ size bootstrap }
}
\details{
method bio-diversity

"Margalef"
"Simpson.Dom"
"Simpson.Div"
"Berger.Parker"
"McIntosh"
"Shannon" 
 
}
\value{
  \item{data}{ vector }
  \item{method}{ method bio-diversity }
  \item{level}{ value 0-100 percentage }
  \item{Boot}{ size 50, 100,... }
}
\author{ Felipe de Mendiburu }

\examples{
library(agricolae)
data(paracsho)
# date 22-06-05 and treatment CON = application with insecticide
specimens <- paracsho[1:10,6]
index.bio(specimens,method="Simpson.Div",level=95,Boot=200)
index.bio(specimens,method="Shannon",level=95,Boot=200)
}
\keyword{ univar }% __ONLY ONE__ keyword per line
