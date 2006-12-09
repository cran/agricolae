\name{wxyz}
\alias{wxyz}
%- wxyz.
\title{ It completes the matrix of information for cells with NA
 by means of a model }
\description{
 It generates a information matrix z=f(x, y). It uses a linear model
 to complete the cells without information
}
\usage{
wxyz(model, x, y, z)
}

\arguments{
  \item{model}{ obtained by 'lm' }
  \item{x}{ vector 'x' }
  \item{y}{ vector 'y' }
  \item{z}{ vector 'z' relation of 'x' e 'y' }
}

}
\value{

  \item{model}{ obtained by 'lm' }
  \item{x}{ Numeric }
  \item{y}{ Numeric }
  \item{z}{ Numeric }
}

\author{ Felipe de Mendiburu }

\seealso{\code{\link{gxyz}}, \code{\link{grid3d}}, \code{\link{grid3p} } }

\examples{
library(agricolae)
x<-c(1,3,5,2,3,1)
y<-c(2,5,4,3,2,3)
z<-c(4,10,NA,6,7,NA)
modelo<-lm(z~x+y)
zz<-wxyz(modelo,x,y,z)
# The response surface
x<-as.numeric(rownames(zz))
y<-as.numeric(colnames(zz))
#startgraph
persp(x,y,zz, cex=0.7,theta = -20, phi = 30,shade= 0.2,nticks = 6, col = 'green' ,
ticktype = 'detailed',xlab = 'X', ylab = 'Y', zlab = 'Response')
#endgraph
}
\keyword{ manip }% at least one, from doc/KEYWORDS

