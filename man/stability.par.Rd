\name{stability.par}
\alias{stability.par}
%- stability.par.
\title{ Stability analysis. SHUKLA'S STABILITY VARIANCE AND KANG'S }
\description{
  This procedure calculates the stability variations as well as the statistics 
  of selection for the yield and the stability.
  The averages of the genotype through the different environment repetitions 
  are required for the calculations. The mean square error must be calculated
  from the joint variance analysis.
}
\usage{
stability.par(data,rep,MSerror,alpha=0.1,main=NULL,cova = FALSE,name.cov=NULL,file.cov=0)
}

\arguments{
  \item{data}{ matrix of averages, by rows the genotypes and columns the environment }
  \item{rep}{ Number of repetitions }
  \item{MSerror}{ Mean Square Error }
  \item{alpha}{ Label significant }
  \item{main}{ Title }
  \item{cova}{ Covariable }
  \item{name.cov}{ Name covariable }
  \item{file.cov}{ Data covariable }
}

\value{
  \item{data}{ Numeric }
  \item{rep}{ Constant numeric }
  \item{MSerror}{ Constant numeric }
  \item{alpha}{ Constant numeric }
  \item{main}{ Text }
  \item{cova}{ FALSE or TRUE }
  \item{name.cov}{ Text }
  \item{file.cov}{ Vector numeric }
}
\references{ Kang, M. S. 1993. Simultaneous selection for yield and
 stability: Consequences for growers. Agron. J. 85:754-757 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{stability.nonpar} } }

\examples{
library(agricolae)
# example 1
# Experimental data,
# replication rep= 4
# Mean square error, MSerror = 1.8
# 12 environment
# 13 genotype  = 1,2,3,.., 13
# yield averages of 13 genotypes in localities
V1 <- c(10.2, 8.8, 8.8, 9.3, 9.6, 7.2, 8.4, 9.6, 7.9, 10, 9.3, 8.0, 10.1)
V2 <- c(7, 7.8, 7.0, 6.9, 7, 8.3, 7.4, 6.5,	6.8, 7.9, 7.3, 6.8, 8.1)
V3 <- c(5.3, 4.4, 5.3, 4.4, 5.6, 4.6, 6.2, 6.0, 6.5, 5.3, 5.7, 4.4, 4.2)
V4 <- c(7.8, 5.9, 7.3, 5.9, 7.8, 6.3, 7.9, 7.5, 7.6, 5.4, 5.6, 7.8, 6.5)
V5 <- c(9, 9.2, 8.8, 10.6, 8.3, 9.3, 9.6, 8.8, 7.9, 9.1, 7.7, 9.5, 9.4)
V6 <- c(6.9, 7.7, 7.9, 7.9, 7, 8.9,	9.4, 7.9, 6.5, 7.2, 5.4, 6.2, 7.2)
V7 <- c(4.9, 2.5, 3.4, 2.5, 3,2.5, 3.6, 5.6,3.8, 3.9, 3.0, 3.0, 2.5)
V8 <- c(6.4, 6.4, 8.1, 7.2, 7.5, 6.6, 7.7, 7.6, 7.8, 7.5, 6.0, 7.2, 6.8)
V9 <- c(8.4, 6.1, 6.8, 6.1, 8.2, 6.9, 6.9, 9.1, 9.2, 7.7, 6.7, 7.8, 6.5)
V10 <-c(8.7, 9.4, 8.8, 7.9, 7.8, 7.8, 11.4, 9.9, 8.6, 8.5, 8.0, 8.3, 9.1)
V11 <-c(5.4, 5.2, 5.6, 4.6, 4.8, 5.7, 6.6, 6.8, 5.2, 4.8, 4.9, 5.4, 4.5)
V12 <-c(8.6, 8.0, 9.2, 8.1, 8.3, 8.9, 8.6, 9.6, 9.5, 7.7, 7.6, 8.3, 6.6)
data<-data.frame(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12)
rownames(data)<-LETTERS[1:13]
stability.par(data, rep=4, MSerror=1.8, alpha=0.1, main="Genotype")

#example 2 covariable. precipitation
precipitation<- c(1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100)
stability.par(data, rep=4, MSerror=1.8, alpha=0.1, main="Genotype",
 cova=TRUE, name.cov="Precipitation", file.cov=precipitation)

}
\keyword{ models }% at least one, from doc/KEYWORDS

