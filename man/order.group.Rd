\name{order.group}
\alias{order.group}
%- order.group.
\title{ Ordering the treatments according to the multiple comparison }
\description{
  This function allows us to compare the treatments averages or the adding of 
  their ranges with the minimal significant difference which can vary from one
  comparison to another one.
  This function is used by the HSD, LSD, Kruskal-Wallis, Friedman or Durbin 
  procedures.
}
\usage{
order.group(trt, means, N, MSerror, Tprob, std.err, parameter=1, snk=0, DFerror=NULL,alpha=NULL,sdtdif=NULL,vartau=NULL)
}

\arguments{
  \item{trt}{ Treatments }
  \item{means}{ Means of treatment }
  \item{N}{ Replications }
  \item{MSerror}{ Mean square error }
  \item{Tprob}{ minimum value for the comparison }
  \item{std.err}{ standard error }
  \item{parameter}{ Constante 1 (Sd), 0.5 (Sx) }
  \item{snk}{ Constante = 1 (Student Newman Keuls) }
  \item{DFerror}{ Degrees of freedom of the experimental error }
  \item{alpha}{ Level of risk for the test  } 
  \item{sdtdif}{ standar deviation of difference in BIB  } 
  \item{vartau}{ matrix var-cov in PBIB  } 
}
\value{
  \item{trt}{ Factor }
  \item{means}{ Numeric }
  \item{N}{ Numeric }
  \item{MSerror}{ Numeric }
  \item{Tprob}{ value between 0 and 1 }
  \item{std.err}{ Numeric }
  \item{parameter}{ Constant }
  \item{snk}{ Constant }
  \item{DFerror }{Numeric}
  \item{alpha }{Numeric}  
  \item{sdtdif}{Numeric}  
  \item{vartau}{ Numeric, matrix } 
}

\author{ Felipe de Mendiburu }

\seealso{\code{\link{order.stat} } }
\examples{
library(agricolae)
treatments <- c("A","B","C","D","E","F")
means<-c(20,40,35,72,49,58)
std.err<-c(1.2, 2, 1.5, 2.4, 1, 3.1)
replications <- c(4,4,3,4,3,3)
MSerror <- 55.8
value.t <- 2.1314
groups<-order.group(treatments,means,replications,MSerror,value.t,std.err)
groups
bar.err(groups,ylim=c(0,80))
}
\keyword{ manip }% at least one, from doc/KEYWORDS

