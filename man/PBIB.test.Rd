\name{PBIB.test}
\alias{PBIB.test}

\title{ Analysis of the Partially Balanced Incomplete Block Design  }
\description{
  Analysis of variance PBIB and comparison mean adjusted.
  Applied to resoluble designs: Lattices and alpha design.
}
\usage{
PBIB.test(block,trt,replication,y,k, method=c("REML","ML","VC"), 
test = c("lsd","tukey"), alpha=0.05, group=TRUE,console=FALSE)
}
\arguments{
  \item{block}{ blocks }
  \item{trt}{ Treatment }
  \item{replication}{ Replication }  
  \item{y}{ Response  }
  \item{k}{ Block size  }  
  \item{method}{ Estimation method: REML, ML and VC }
  \item{test}{ Comparison treatments }
  \item{alpha}{ Significant test }
  \item{group}{ TRUE or FALSE }
  \item{console}{logical, print output }
}
\details{
Method of comparison treatment. 
lsd: least significant difference.
tukey: Honestly significant differente.
Estimate: specifies the estimation method for the covariance parameters.
The REML is the default method. The REML specification performs residual (restricted) maximum likelihood, and 
The ML specification performs maximum likelihood, and 
the VC specifications apply only to variance component models. 
}
\value{
  \item{block}{ Vector, consecutive numbers by replication }
  \item{trt}{ Vector numeric or character }
  \item{replication}{ Vector }  
  \item{y}{ numeric vector}
  \item{k}{ numeric constant }    
  \item{method}{ Character: REML, ML and VC }
  \item{test}{ Character: comparison methods lsd and tukey}
  \item{alpha}{ Numeric }
  \item{group}{ Logic} 
}
\references{
1. Iterative Analysis of Generalizad Lattice Designs.
 E.R. Williams (1977) Austral J. Statistics 19(1) 39-42. 
 
2. Experimental design. Cochran and Cox. Second edition. 
Wiley Classics Library Edition published 1992
} 
\author{ F. de Mendiburu }
\seealso{\code{\link{BIB.test}}, \code{\link{design.alpha}} }
\examples{
require(agricolae)
require(MASS) # method = VC in PBIB.test
# if(require(lme4)) # method = REML or LM in PBIB.test
# alpha design 
Genotype<-paste("geno",1:30,sep="")
ntr<-length(Genotype)
r<-2
k<-3
s<-10
obs<-ntr*r
b <- s*r
book<-design.alpha(Genotype,k,r,seed=5)
book$book[,3]<- gl(20,3)
# dataset
y<-c(5,2,7,6,4,9,7,6,7,9,6,2,1,1,3,2,4,6,7,9,8,7,6,4,3,2,2,1,1,2,
     1,1,2,4,5,6,7,8,6,5,4,3,1,1,2,5,4,2,7,6,6,5,6,4,5,7,6,5,5,4)
dbook<-data.frame(book$book,yield=y)
rm(y,Genotype)
# analysis
attach(dbook)
if(require(lme4)) { # method = REML or LM in PBIB.test
model <- PBIB.test(block, Genotype, replication, yield, k=3, method="REML", group=TRUE)
}
detach(dbook)
model$method
model$parameters
head(model$comparison)
model$means
model$groups
bar.group(model$groups,ylim=c(0,9), density=20, las=2)
}
\keyword{ models }
