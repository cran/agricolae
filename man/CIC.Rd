\name{CIC}
\alias{CIC}
\docType{data}
\title{ Data for AMMI Analysis}
\description{
A study of the disease in the potato plant in the localities of Comas and
Oxapampa in Peru.
}
\usage{data(CIC)}
\format{
  A data frame with 732 observations of the following 7 variables.
  \describe{
    \item{\code{Environment}}{a factor with levels \code{Comas} \code{Oxapampa}}
    \item{\code{Order}}{a numeric vector}
    \item{\code{Genotype}}{a factor with levels \code{762616.258} \code{762619.251} \code{Atzimba} \code{CC2.1} \code{CC2.10} \code{CC2.100} \code{CC2.101} \code{CC2.102} \code{CC2.103} \code{CC2.104} \code{CC2.105} \code{CC2.106} \code{CC2.107} \code{CC2.108} \code{CC2.109} \code{CC2.11} \code{CC2.110} \code{CC2.111} \code{CC2.112} \code{CC2.113} \code{CC2.114} \code{CC2.115} \code{CC2.116} \code{CC2.117} \code{CC2.118} \code{CC2.119} \code{CC2.12} \code{CC2.120} \code{CC2.13} \code{CC2.14} \code{CC2.15} \code{CC2.16} \code{CC2.17} \code{CC2.18} \code{CC2.19} \code{CC2.2} \code{CC2.20} \code{CC2.21} \code{CC2.22} \code{CC2.23} \code{CC2.24} \code{CC2.25} \code{CC2.26} \code{CC2.27} \code{CC2.28} \code{CC2.29} \code{CC2.3} \code{CC2.30} \code{CC2.31} \code{CC2.32} \code{CC2.33} \code{CC2.34} \code{CC2.35} \code{CC2.36} \code{CC2.37} \code{CC2.38} \code{CC2.39} \code{CC2.4} \code{CC2.40} \code{CC2.41} \code{CC2.42} \code{CC2.43} \code{CC2.44} \code{CC2.45} \code{CC2.46} \code{CC2.47} \code{CC2.48} \code{CC2.49} \code{CC2.5} \code{CC2.50} \code{CC2.51} \code{CC2.52} \code{CC2.53} \code{CC2.54} \code{CC2.55} \code{CC2.56} \code{CC2.57} \code{CC2.58} \code{CC2.59} \code{CC2.6} \code{CC2.60} \code{CC2.61} \code{CC2.62} \code{CC2.63} \code{CC2.64} \code{CC2.65} \code{CC2.66} \code{CC2.67} \code{CC2.68} \code{CC2.69} \code{CC2.7} \code{CC2.70} \code{CC2.71} \code{CC2.72} \code{CC2.73} \code{CC2.74} \code{CC2.75} \code{CC2.76} \code{CC2.77} \code{CC2.78} \code{CC2.79} \code{CC2.8} \code{CC2.80} \code{CC2.81} \code{CC2.82} \code{CC2.83} \code{CC2.84} \code{CC2.85} \code{CC2.86} \code{CC2.87} \code{CC2.88} \code{CC2.89} \code{CC2.9} \code{CC2.90} \code{CC2.91} \code{CC2.92} \code{CC2.93} \code{CC2.94} \code{CC2.95} \code{CC2.96} \code{CC2.97} \code{CC2.98} \code{CC2.99} \code{Chata_Blanca} \code{Lbr_40} \code{Monserrate} \code{Yungay}}
    \item{\code{Rep}}{a numeric vector}
    \item{\code{Plants}}{a numeric vector}
    \item{\code{AUDPC}}{a numeric vector}
    \item{\code{Relative}}{a numeric vector}
  }
}

\source{
  Experimental field.
}
\references{
  International Potato Center. CIP - Lima Peru.
}
\examples{
library(agricolae)
data(CIC)
str(CIC)
}
\keyword{datasets}
