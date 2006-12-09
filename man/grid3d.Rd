\name{grid3d}
\alias{grid3d}
%- grid3d.
\title{ Interpolation of a information matrix }
\description{
   Generates a information matrix  Z=f(x,y) by a process of interpolation,
   use function interpolate() (library e1071).
}
\usage{
grid3d(zz, m, n, ...)
}

\arguments{
  \item{zz}{ matrix relation of "x" and "y" }
  \item{m}{ number of rows of new matrix }
  \item{n}{ number of columns of new matrix }
  \item{\dots}{ method=linear or constant }
}
\details{
  The origin data correspond to a vector "x" and to a vector "y" whose
  resultant it is a first "z", "z" will have pxq elements, whose value
  z[i, j ] = f(x[i], y[j ]).

  The function fxyz obtains a new data set.  A new vector "x" of "m"
  elements and a new vector "y" of "n" elements and the matrix "z"
  of mxn elements, those that will be obtained by interpolation.
}
\value{

  \item{zz}{ Numeric }
  \item{m}{ Numeric }
  \item{n}{ Numeric }

}

\author{ Felipe de Mendiburu }

\seealso{\code{\link{wxyz}}, \code{\link{gxyz}}, \code{\link{grid3p} } }

\examples{
library(e1071)
library(agricolae)
data(ralstonia)
arc<-c(4, 32, 24, 12, 8, 14, 28, 22, 14, 20, 6, 10, 24)
days <- c(2,15,29,43,58,73)
zz<-as.matrix(ralstonia)
dimnames(zz)<-list(arc,days)
fzz<-gxyz(zz)
x<-fzz[[1]]
y<-fzz[[2]]
z<-fzz[[3]]
modelo<-lm(z~x+y+x*y+I(x^2)+I(y^2) )
# it completes and it orders the information matrix z1 <- [x,y,z]
z1<-wxyz(modelo,x,y,z)
m<-40
n<-15
# It generates a new matrix mxn with but points by interpolation.
z2<-grid3d(z1,m,n)
# Surface of response
x2<-as.numeric(rownames(z2))
y2<-as.numeric(colnames(z2))
#startgraph
res<-persp(x2,y2,z2, cex=0.7,theta = 60, phi = 30,shade= 0.2,nticks = 6, col = "yellow" ,
ticktype = "detailed",xlab = "clay", ylab = "days", zlab = "Ralstonia")
mtext("Ralstonia solanacearum population",side=3,cex=0.9,font=4)
#endgraph
}
\keyword{ math }% at least one, from doc/KEYWORDS

