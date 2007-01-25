\name{AMMI}
\alias{AMMI}
%- AMMI.
\title{ AMMI Analysis}
\description{
Additive Main Effects and Multiplicative Interaction Models (AMMI) are widely used 
to analyze main effects and genotype by environment (GEN, ENV) interactions in 
multilocation variety trials. Furthermore, this function generates biplot, triplot 
graphs and analysis.
}
\usage{
AMMI(ENV, GEN, REP, Y, MSE = 0, number=TRUE,graph="biplot",...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ENV}{ Environment }
  \item{GEN}{ Genotype }
  \item{REP}{ Replication  }
  \item{Y}{ Response }
  \item{MSE}{ Mean Square Error }
  \item{number}{ TRUE or FALSE }
  \item{graph}{ "biplot" or "triplot" }
  \item{\dots}{ plot graphics parameters }
}
\details{
  additional biplot.
}
\value{

  \item{ENV }{Factor}
  \item{GEN }{Factor}
  \item{REP }{Numeric}
  \item{Y }{Numeric}
  \item{MSE }{Numeric}
  \item{number}{ TRUE or FALSE }
  \item{graph}{ "biplot" or "triplot" }
  \item{\dots}{ others parameters }
}
\references{ 
GGE Biplot Analysis: A graphical tool for breeder, geneticists, and agronomists.
Weikai Yan and Manjit S. Kang. www.crepress.com 2003,
Principles and procedures of statistics: a biometrical approach
Steel & Torry & Dickey. Third Edition 1997
 }
\author{ F. de Mendiburu }

\seealso{\code{\link{lineXtester}} }
\examples{
# Full replications
library(agricolae)
library(klaR)
# Example 1
data(ltrv)
#startgraph
# biplot
model<- AMMI(ltrv[,2], ltrv[,1], ltrv[,3], ltrv[,5],xlim=c(-3,3),ylim=c(-4,4),
graph="biplot")
model<- AMMI(ltrv[,2], ltrv[,1], ltrv[,3], ltrv[,5],xlim=c(-3,3),ylim=c(-4,4),
graph="biplot",number=FALSE)
# triplot
model<- AMMI(ltrv[,2], ltrv[,1], ltrv[,3], ltrv[,5],graph="triplot")
model<- AMMI(ltrv[,2], ltrv[,1], ltrv[,3], ltrv[,5],graph="triplot",number=FALSE)
#endgraph
# Example 2
data(CIC)
attach(CIC)
#startgraph
par(cex=0.6)
model<-AMMI(Environment, Genotype, Rep, Relative,xlim=c(-0.6,0.6),
ylim=c(-1.5e-8,1.5e-8))
#endgraph
pc<- princomp(model$genXenv, cor = FALSE)
pc$loadings
summary(pc)
model$biplot
# Example 3
# Only means. Mean square error is well-known.
data(sinRepAmmi)
attach(sinRepAmmi)
REP <- 3
MSerror <- 93.24224
#startgraph
model<-AMMI(ENV, GEN, REP, YLD, MSerror,xlim=c(-8,6),ylim=c(-6,6))
#endgraph
pc<- princomp(model$genXenv, cor = FALSE)
pc$loadings
summary(pc)
model$biplot
# Biplot with the one restored observed.
rm(REP)
bplot<-model$biplot[,1:4]
attach(bplot)
#startgraph
par(cex=0.8)
plot(YLD,CP1,cex=0.0,text(YLD,CP1,labels=row.names(bplot),col="blue"),
 main="AMMI BIPLOT",frame=TRUE)
media<-mean(YLD)
abline(h=0,v= media,lty=2,col="red")
amb<-subset(bplot,type=="ENV")
attach(amb)
s <- seq(length(YLD))
arrows(media, 0, 0.9*(YLD[s]-media)+media, 0.9*CP1[s], col= "brown",
lwd=1.8,length=0.1)
#endgraph

# Principal components by means of the covariance 
# It is to compare results with AMMI
#startgraph
par(cex=0.8)
pc<- princomp(model$genXenv, cor = FALSE)
biplot(pc,main="Interaction ENV x GEN")
abline(h=0,v= 0,lty=2,col="brown")
#endgraph
cova<-cov(model$genXenv)
values<-eigen(cova)
total<-sum(values$values)
round(values$values*100/total,2)
# [1] 64.81 18.58 13.50  3.11  0.00
}
\keyword{ models }% at least one, from doc/KEYWORDS


