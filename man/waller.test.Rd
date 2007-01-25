\name{waller.test}
\alias{waller.test}
%- waller.group.
\title{ Multiple comparisons, Waller-Duncan }
\description{
  The Waller-Duncan k-ratio t test is performed on all main effect means
  in the MEANS statement. See the K-RATIO option for information on 
  controlling details of the test.
}
\usage{
waller.test(y, trt, DFerror, MSerror, Fc, K = 100, group=TRUE, main = NULL)
}
\arguments{
  \item{y}{ Variable response }
  \item{trt}{ Treatments }
  \item{DFerror}{ Degrees of freedom }
  \item{MSerror}{ Mean Square Error }
  \item{Fc}{ F Value }
  \item{K}{ K-RATIO }
  \item{group}{ TRUE or FALSE }
  \item{main}{ Title }
}
\details{

It is necessary first makes a analysis of variance.

K-RATIO (K): value specifies the Type 1/Type 2 error seriousness ratio for 
the Waller-Duncan test. Reasonable values for KRATIO are 50, 100, and 500,
which roughly correspond for the two-level case to ALPHA levels of 0.1, 0.05,
and 0.01. By default, the procedure uses the default value of 100. 
}
\value{

  \item{y }{Numeric}
  \item{trt }{factor}
  \item{DFerror }{Numeric}
  \item{MSerror }{Numeric}
  \item{Fc }{Numeric}
  \item{K }{Numeric}
  \item{group}{ Logic }  
  \item{main }{Text}
}
\references{ 
Waller, R. A. and Duncan, D. B. (1969).
A Bayes Rule for the Symmetric Multiple Comparison Problem,
Journal of the American Statistical Association 64, pages 1484-1504. 

Waller, R. A. and Kemp, K. E. (1976)
Computations of Bayesian t-Values for Multiple Comparisons,
Journal of Statistical Computation and Simulation, 75, pages 169-172.

Steel & Torry & Dickey. Third Edition 1997
Principles and procedures of statistics a biometrical approach
}
\author{ Felipe de Mendiburu}

\seealso{\code{\link{HSD.test}}, \code{\link{LSD.test}}, \code{\link{bar.err}},
 \code{\link{bar.group}} }

\examples{
library(agricolae)
data(sweetpotato)
attach(sweetpotato)
model<-aov(yield~virus)
df<-df.residual(model)
MSerror<-deviance(model)/df
Fc<-anova(model)[1,4]
comparison <- waller.test(yield, virus, df, MSerror, Fc, group=TRUE, 
main="Yield of sweetpotato. Dealt with different virus")
# std = F (default) is standard error
#startgraph
par(mfrow=c(2,2))
bar.err(comparison,std=TRUE,horiz=TRUE,xlim=c(0,45),density=4)
bar.err(comparison,std=TRUE,horiz=FALSE,ylim=c(0,45),density=8,col="blue")
bar.group(comparison,horiz=FALSE,ylim=c(0,45),density=8,col="red")
bar.group(comparison,horiz=TRUE,xlim=c(0,45),density=4,col="green")
#endgraph
}
\keyword{ htest }% at least one, from doc/KEYWORDS
