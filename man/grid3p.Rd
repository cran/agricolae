\name{grid3p}
\alias{grid3p}
%- grid3p.
\title{ Interpolation for nonequidistant points in matrix}
\description{
  Z=f(x,y) generates an information matrix by a process of interpolation for
  nonequidistant points. It uses function interpp of library akima.
}
\usage{
grid3p(x, y, z, m, n)
}

\arguments{
  \item{x}{ Vector independent}
  \item{y}{ Vector independent }
  \item{z}{ Vector dependent }
  \item{m}{ number of rows of the new matrix }
  \item{n}{ number of columns of the new matrix }
}
\details{
  The function fxyz obtains a new data set.  A new vector "x" of "m"
  elements and a new vector "y" of "n" elements and the matrix "z"
  of mxn elements, those that will be obtained by interpolation.
}
\value{

  \item{x }{Numeric}
  \item{y }{Numeric}
  \item{z }{Numeric}
  \item{m }{Numeric}
  \item{n }{Numeric}
}

\author{ Felipe de Mendiburu }

\seealso{\code{\link{wxyz}}}

\examples{
library(akima)
library(agricolae)
data(clay)
x<-clay$per.clay
y<-clay$days
z<-clay$ralstonia
model<- lm(z ~ x + y)
zo<-wxyz(model,x,y,z)
# it completes and it finds the average of points with equal coordinate.
b<-colnames(zo)
a<-rownames(zo)
x<-as.numeric(rep(a,length(b)))
y<-NULL
for(i in 1:length(b)) y<-c(y,rep(b[i],length(a)))
y<-as.numeric(y)
z<-as.numeric(zo)
m<-40
n<-20
# It generates a new matrix mxn with but points by interpolation.
z2<-grid3p(x,y,z,m,n)
# plot
x2<-as.numeric(dimnames(z2)[[1]])
y2<-as.numeric(dimnames(z2)[[2]])
res<-contour(x2,y2,z2, cex=0.7, col="blue",xlab="clay",ylab="days")
mtext("Ralstonia solanacearum population",side=3,cex=0.9,font=4)
#====================
# Using the function of interpolacion of irregular points. see interp() de "akima"

data(clay)
x<-clay$per.clay
y<-clay$days
z<-clay$ralstonia
zz <- interp(x,y,z,xo=seq(4,32,length=100),yo=seq(2,79,length=100),duplicate="mean")
#startgraph
image(zz$x,zz$y,zz$z,xlab = "clay", ylab = "day",frame=FALSE, col=topo.colors(8))
contour(zz$x,zz$y,zz$z, cex=0.7, col = "blue",add=TRUE,frame=FALSE)
mtext("Ralstonia solanacearum population\n",side=3,cex=0.9,font=4)
#endgraph
}
\keyword{ math }% at least one, from doc/KEYWORDS

