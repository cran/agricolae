\name{strip.plot}
\alias{strip.plot}
%- Strip.plot.
\title{ Strip-Plot analysis }
\description{
  The analysis of variance of a strip-plot design is divided into 
  three parts: the horizontal-factor analysis, the vertical-factor
  analysis, and the interaction analysis.
}
\usage{
strip.plot(BLOCKS, COL, ROW, Y)
}

\arguments{
  \item{BLOCKS}{ replications }
  \item{COL}{ Factor column }
  \item{ROW}{ Factor row }
  \item{Y}{ Variable, response }
}
\details{
  The strip-plot design is specifically suited for a two-factor experiment
  in which the desired precision for measuring the interaction effects
  between the two factors is higher than that for measuring the main efect
  two factors
}
\value{
  \item{BLOCKS}{ vector, numeric }
  \item{COL}{ vector, numeric }
  \item{ROW}{ vector, numeric }
  \item{Y}{ vector, numeric }
}
\references{ Statistical procedures for agricultural research.
 Kwanchai A. Gomez, Arturo A. Gomez. Second Edition. 1984.
 }
\author{ Felipe de Mendiburu  }
\examples{
# Yield
library(agricolae)
data(huasahuasi)
attach(huasahuasi)
modelo<-strip.plot(Block, Clon, Treat, yield)
comparison<-LSD.group(yield,Clon,modelo$gl.a,modelo$Ea)
comparison<-LSD.group(yield,Treat,modelo$gl.b,modelo$Eb)
}
\keyword{ models }% at least one, from doc/KEYWORDS



