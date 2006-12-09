\name{tapply.stat}
\alias{tapply.stat}
%- tapply.stat.
\title{ Statistics of data grouped by factors }
\description{
  The process consists of finding statistics of but of a variable,
  grouped or crossed by factors. The table must be organized
  by columns between variables and factors.
}
\usage{
tapply.stat(x, y, stat = "mean")
}

\arguments{
  \item{x}{data.frame factors  }
  \item{y}{data.frame variables  }
  \item{stat}{ Method }
}

\value{
  \item{x}{ Numeric  }
  \item{y}{ Numeric }
  \item{stat}{method = "mean", ...  }
}
\author{ Felipe de Mendiburu }

\examples{
library(agricolae)
# case of 1 single factor
data(sweetpotato)
tapply.stat(sweetpotato[,1],sweetpotato[,2],mean)
attach(sweetpotato)
tapply.stat(virus,yield,sd)
tapply.stat(virus,yield,function(x) max(x)-min(x))
tapply.stat(virus,yield,function(x) quantile(x,0.75,6)-quantile(x,0.25,6))
# other case
data(cotton)
attach(cotton)
tapply.stat(cotton[,c(1,3,4)],yield,mean)
tapply.stat(cotton[,c(1,4)],yield,max)
# Height of pijuayo
data(growth)
attach(growth)
tapply.stat(growth[,2:1], height,function(x) mean(x,na.rm=TRUE))
# trees
data(trees)
attach(trees)
w<-tapply(trees[,3],trees[,2],function(x) mean(x,na.rm=TRUE))
#startgraph
barplot(w,density=c(4,8,12,16),col=c(2,4,6,8),angle=c(0,45,90,145),las=2)
#endgraph
tapply.stat(species,diameter,function(x) mean(x,na.rm=TRUE))
}
\keyword{ univar }% at least one, from doc/KEYWORDS

