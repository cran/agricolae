\name{LSD.test}
\alias{LSD.test}
%- LSD.group.
\title{ Multiple comparisons, "Least significant difference" and Adjust P-values }
\description{
  Multiple comparisons of treatments by means of LSD and a grouping of treatments. 
  The level by alpha default is 0.05. Returns p-values adjusted using one of several methods
}
\usage{
LSD.test(y, trt, DFerror, MSerror, alpha = 0.05, p.adj=c("none","holm", 
"hochberg", "bonferroni", "BH", "BY", "fdr"), group=TRUE, main = NULL)
}

\arguments{
  \item{y}{ Answer of the experimental unit  }
  \item{trt}{ Treatment applied to each experimental unit  }
  \item{DFerror}{ Degrees of freedom of the experimental error }
  \item{MSerror}{ Means square error of the experimental  }
  \item{alpha}{ Level of risk for the test  }
  \item{p.adj}{ Method for adjusting p values (see p.adjust)  }  
  \item{group}{ TRUE or FALSE }
  \item{main}{ title of the study  }
}  
\details{
  For equal or different repetition.
  p.adj = "holm", "hochberg", "bonferroni", "BH", "BY", "fdr".
  see p.adjust()
  p-adj ="none" is t-student.
  p-adj ="hommel" is not applied in this test.
}
\value{
 
  \item{y }{Numeric}
  \item{trt }{alfanumeric}
  \item{DFerror }{Numeric}
  \item{MSerror }{Numeric}
  \item{alpha }{Numeric}
  \item{p.adj}{ text, see p.adjust  }  
  \item{group}{ Logic }
  \item{main }{Numeric}
}
\references{
     Steel, R.; Torri,J; Dickey, D.(1997)
     Principles and Procedures of Statistics
     A Biometrical Approach. pp178.
     }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{HSD.test}}, \code{\link{waller.test}}, \code{\link{bar.err}}, 
\code{\link{bar.group}} }

\examples{
library(agricolae)
data(sweetpotato)
attach(sweetpotato)
model<-aov(yield~virus)
df<-df.residual(model)
MSerror<-deviance(model)/df
comparison <- LSD.test(yield,virus,df,MSerror, p.adj="bonferroni", group=FALSE,
main="Yield of sweetpotato\ndealt with different virus")
#stargraph
bar.err(comparison,std=TRUE,ylim=c(0,45),density=4,border="blue")
#endgraph

}
\keyword{ htest }% at least one, from doc/KEYWORDS

