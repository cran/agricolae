\name{simulation.model}
\alias{simulation.model}
%- simulation.model.
\title{ Simulation of the linear model under normality }
\description{
  This process consists of validating the variance analysis results 
  using a simulation process of the experiment.
  The validation consists of comparing the calculated values of each source
  of variation of the simulated data with respect to the calculated values
  of the original data. If in more than 50 percent of the cases they are higher
  than the real one, then it is considered favorable and the probability
  reported by the ANOVA is accepted, since the P-Value is the probability of
  (F > F.value).
}
\usage{
simulation.model(k, file, model, categorical = NULL)
}

\arguments{
  \item{k}{ Number of simulations. }
  \item{file}{ Data for the study of the model.}
  \item{model}{ Model in R.}
  \item{categorical}{ position of the columns of the data that correspond
   to categorical variables. }
}
\value{
  \item{k}{ constant numeric. }
  \item{file}{ data frame}
  \item{model}{ Model}
  \item{categorical}{ Numeric}
}  
\author{ Felipe de Mendiburu }

\seealso{\code{\link{resampling.model} } }

\examples{
library(agricolae)
#example 1
data(clay)
model<-"ralstonia ~ days"
simulation.model(200,clay,model)
#example 2
data(sweetpotato)
model<-"yield~virus"
simulation.model(300,sweetpotato,model,categorical=1)
#example 3
data(Glycoalkaloids)
model<-"HPLC ~ spectrophotometer"
simulation.model(100,Glycoalkaloids,model)
#example 4
data(potato)
model<-"cutting~date+variety"
simulation.model(200,potato,model,categorical=c(1,2,3))

}
\keyword{ models }% at least one, from doc/KEYWORDS

