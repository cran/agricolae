\name{BIB.test}
\alias{BIB.test}

\title{ Finding the Variance Analysis of the Balanced Incomplete Block Design  }
\description{
  Analysis of variancia BIB and comparison mean adjusted. 
}
\usage{
BIB.test(block, trt, y, method = "lsd", alpha = 0.05, group = TRUE)
}

\arguments{
  \item{block}{ blocks }
  \item{trt}{ Treatment }
  \item{y}{ Response  }
  \item{method}{ Comparison treatments }
  \item{alpha}{ Significant test }
  \item{group}{logical }
}
\details{
Method of comparison treatment. 
lsd: least significant difference.
tukey: Honestly significant differente.
waller: test Waller-Duncan.
}
\value{
  \item{block}{ Vector }
  \item{trt}{ Vector }
  \item{y}{ numeric vector}
  \item{method}{ Character }
  \item{alpha}{ Numeric }
  \item{group}{ TRUE or FALSE }
}
\references{ Design of Experiments. Robert O. Kuehl. 2nd ed., Duxbury, 2000 
 Linear Estimation and Design of Experiments. D.D. Joshi. WILEY EASTERN LIMITED
 1987, New Delhi, India}

\author{ F. de Mendiburu }
\seealso{\code{\link{durbin.test}} }
\examples{
library(agricolae)
# Example Design of Experiments. Robert O. Kuehl. 2da. Edicion. 2001
run<-gl(10,3)
psi<-c(250,325,475,250,475,550,325,400,550,400,475,550,325,475,550,
250,400,475,250,325,400,250,400,550,250,325,550,325,400,475)
monovinyl<-c(16,18,32,19,46,45,26,39,61,21,35,55,19,47,48,20,33,31,13,13,34,21,
 30,52,24,10,50,24,31,37)
out<-BIB.test(run,psi,monovinyl,method="waller",group=FALSE)
out<-BIB.test(run,psi,monovinyl,method="waller",group=TRUE)
out<-BIB.test(run,psi,monovinyl,method="tukey",group=TRUE)
out<-BIB.test(run,psi,monovinyl,method="tukey",group=FALSE)
bar.err(out,density=4,ylim=c(0,60))
# Example linear estimation and design of experiments. D.D. Joshi. 1987
# Professor of Statistics, Institute of Social Sciences Agra, India
# 6 varieties of wheat crop in a BIB whit 10 blocks of 3 plots each.
y <-c(69,77,72,63,70,54,65,65,57,59,50,45,68,75,59,38,60,60,62,
 55,54,65,62,65,61,39,54,67,63,56)
varieties<-gl(6,5)
block <- c(1,2,3,4,5,1,2,6,7,8,1,3,6,9,10,2,4,7,9,10,3,5,7,8,9,4,5,6,8,10)
model<-BIB.test(block, varieties, y)

}
\keyword{ models }

