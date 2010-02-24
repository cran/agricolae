\name{DAU.test}
\alias{DAU.test}
%- DAU.test.
\title{ Finding the Variance Analysis of the Augmented block Design  }
\description{
  Analysis of variance Augmented block and comparison mean adjusted. 
}
\usage{
DAU.test(block, trt, y, method = c("lsd","tukey"))

}

\arguments{
  \item{block}{ blocks }
  \item{trt}{ Treatment }
  \item{y}{ Response  }
  \item{method}{ Comparison treatments }
}
\details{
Method of comparison treatment. 
lsd: Least significant difference.
tukey: Honestly significant differente.
}
\value{
  \item{block}{ Vector }
  \item{trt}{ Vector }
  \item{y}{ numeric vector}
  \item{method}{ Character }
}
\references{ Federer, W. T. (1956). Augmented (or hoonuiaku) designs. Hawaiian Planters, Record LV(2):191-208. }

\author{ F. de Mendiburu }
\seealso{\code{\link{BIB.test}},\code{\link{LSD.test}}, \code{\link{HSD.test}}, \code{\link{PBIB.test}} }
\examples{
library(agricolae)
block<-c(rep("I",7),rep("II",6),rep("III",7))
trt<-c("A","B","C","D","g","k","l","A","B","C","D","e","i","A","B","C","D","f","h","j")
yield<-c(83,77,78,78,70,75,74,79,81,81,91,79,78,92,79,87,81,89,96,82)
model<- DAU.test(block,trt,yield,method="lsd")
}
\keyword{ models }

