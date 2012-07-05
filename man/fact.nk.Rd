\name{fact.nk}
\alias{fact.nk}
%- fact.nk.
\title{ Factorial design k-factors with n-levels in blocks }
\description{
  It generates a table randomized for the design of blocks in factorial \eqn{n^k}.
  "Random" uses the methods of number generation in R.
  The seed is by set.seed(seed, kinds).
}
\usage{
fact.nk(level, factors, blocks, seed = 0, kinds = "Super-Duper")
}

\arguments{
  \item{level}{ Number of levels }
  \item{factors}{ Number of factor}
  \item{blocks}{ Number of block }
  \item{seed}{ Seed to randomize }
  \item{kinds}{ method for to randomize}
}
\details{
  kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
  "Mersenne-Twister", "Knuth-TAOCP", "user-supplied",  "Knuth-TAOCP-2002",
  "default" )
}
\value{

  \item{level }{Numeric}
  \item{factors }{Numeric}
  \item{blocks }{Numeric}
  \item{seed }{Numeric}
  \item{kinds}{ details }
}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.crd}}, \code{\link{design.rcbd}}, \code{\link{design.lsd}}, 
\code{\link{random.ab}}, \code{\link{design.ab} } }

\examples{
library(agricolae)
level<- 2; factors<-3 ; blocks <- 4
fact.nk(level,factors,blocks)
# factorial 2^5 in 4 blocks
fact.nk(2,5,4)
# factorial two factors in 3 levels for 4 blocks
fact.nk(3,2,4,seed=100,kinds="Mersenne-Twister")
# factorial 4x4 = 4^2 in 3 blocks
fact.nk(4,2,3)

}
\keyword{ design }% at least one, from doc/KEYWORDS

