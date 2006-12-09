\name{resampling.model}
\alias{resampling.model}
%- resampling.model.
\title{ Resampling for linear models }
\description{
  The process consists of finding the values of P-value by means of
  a re-sampling process, next to the values obtained by analysis of variance.
}
\usage{
resampling.model(k, data, model)
}

\arguments{
  \item{k}{ number of re-samplings }
  \item{data}{ data for the study of the model }
  \item{model}{ model in R }
}

\value{

  \item{k}{ constant numeric }
  \item{data}{ data frame }
  \item{model}{ model }
}

\author{ Felipe de Mendiburu }

\seealso{\code{\link{simulation.model} } }

\examples{
#example 1 Simple linear regression
library(agricolae)
data(clay)
model<-"ralstonia ~ days"
analysis<-resampling.model(200,clay,model)

#example 2 Analysis of variance: RCD
data(sweetpotato)
model<-"yield~virus"
analysis<-resampling.model(300,sweetpotato,model)

#example 3 Simple linear regression
data(Glycoalkaloids)
model<-"HPLC ~ spectrophotometer"
analysis<-resampling.model(100,Glycoalkaloids,model)

#example 4 Factorial in RCD

data(potato)
potato[,1]<-as.factor(potato[,1])
potato[,2]<-as.factor(potato[,2])
model<-"cutting~variety + date + variety:date"
analysis<-resampling.model(100,potato,model)

}
\keyword{ multivariate }% at least one, from doc/KEYWORDS

