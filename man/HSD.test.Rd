\name{HSD.test}
\alias{HSD.test}
%- HSD.
\title{ Multiple comparisons: Tukey  }
\description{
  It makes multiple comparisons of treatments by means of Tukey.
  The level by alpha default is 0.05.
}
\usage{
HSD.test(y, trt, DFerror, MSerror, alpha = 0.05, group=TRUE, main = NULL,console=FALSE)
}

\arguments{
  \item{y}{ model(aov or lm) or answer of the experimental unit  }
  \item{trt}{ Constant( only y=model) or vector treatment applied to each experimental unit  }
  \item{DFerror}{ Degree free }
  \item{MSerror}{ Mean Square Error }
  \item{alpha}{ Significant level }
  \item{group}{ TRUE or FALSE }
  \item{main}{ Title }
  \item{console}{logical, print output }
}
\details{
  It is necessary first makes a analysis of variance.
}
\value{

  \item{y }{class (aov or lm) or vector numeric}
  \item{trt }{constant (only y=model) or vector alfanumeric}
  \item{DFerror }{Numeric}
  \item{MSerror }{Numeric}
  \item{alpha }{Numeric}
  \item{group}{ Logic}  
  \item{main }{Text}

}
\references{  
1. Principles and procedures of statistics a biometrical approach
   Steel & Torry & Dickey. Third Edition 1997
2. Multiple comparisons theory and methods. Departament of statistics 
   the Ohio State University. USA, 1996. Jason C. Hsu. Chapman Hall/CRC.
}

\author{ Felipe de Mendiburu }

\seealso{\code{\link{LSD.test}}, \code{\link{waller.test} }, \code{\link{SNK.test} },
\code{\link{duncan.test} } }

\examples{
library(agricolae)
data(sweetpotato)
model<-aov(yield~virus, data=sweetpotato)
out <- HSD.test(model,"virus", group=TRUE,console=TRUE,
main="Yield of sweetpotato\nDealt with different virus")
#stargraph
bar.group(out$groups,ylim=c(0,45),density=4,border="blue")
#endgraph
out<-HSD.test(model,"virus", group=FALSE)
means<-out$means

# Old version HSD.test()
attach(sweetpotato)
df<-df.residual(model)
MSerror<-deviance(model)/df
HSD.test(yield,virus,df,MSerror, group=TRUE,console=TRUE,
main="Yield of sweetpotato. Dealt with different virus")
detach(sweetpotato)
}
\keyword{ htest }% at least one, from doc/KEYWORDS

