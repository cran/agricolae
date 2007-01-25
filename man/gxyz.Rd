\name{gxyz}
\alias{gxyz}
%- gxyz.
\title{ The matrix zz of information is disturbed in three vectors x, y, z }
\description{
  From an information matrix, the function gxyz generates an object data.frame
  that contains the information matrix z=f(x, y) in columns."x" constitutes
  the name of the rows, "y" the name of the columns, and "z" the third column.
}
\usage{
gxyz(zz)
}

\arguments{
  \item{zz}{ The matrix zz of information }
}
\value{
  \item{zz }{matrix}
}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{wxyz}}, \code{\link{grid3p} } }
\examples{
library(agricolae)
x<-c(1,3,5,2,3,1)
y<-c(2,5,4,3,2,3)
z<-c(4,10,NA,6,7,NA)
# original data
cbind(x,y,z)
modelo<-lm(z~x+y)
# worked data
zz<-wxyz(modelo,x,y,z)
zz
# Use function gxyz
xyz<-gxyz(zz)
par(mfrow=c(2,2),cex=0.6)
# startgraph
# original data
plot(x,y,main="original data")
#endgraph
# startgraph
# worked data
plot(xyz$x,xyz$y,xlab="x",ylab="y",main="worked data\nwith model")
#endgraph
}
\keyword{ math }% at least one, from doc/KEYWORDS

