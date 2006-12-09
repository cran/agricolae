\name{ComasOxapampa}
\alias{ComasOxapampa}
\docType{data}
\title{ Data AUDPC Comas - Oxapampa}
\description{
  Measurement of AUDPC in the field of Comas and Oxapampa for
  resistance to Late Blight.
}
\usage{data(ComasOxapampa)}
\format{
  A data frame with 56 observations on the following 3 variables.
  \describe{
    \item{\code{CULTIVAR}}{a factor with levels \code{Amarilis-INIA} \code{Andinita} \code{Atahualpa} \code{Baseko} \code{BIRRIS} \code{C91-906-(Primavera)} \code{Canchan-INIA} \code{Chagllina-INIA} \code{Chamak} \code{Chata-Roja} \code{Costanera} \code{CRUZA-148} \code{Dheera} \code{Enfula} \code{FLS-5-(Raniag)} \code{Gikungu} \code{Heera} \code{ICA-Zipa} \code{Idiafrit} \code{IDIAP-92} \code{Ingabire} \code{INIA-301} \code{INIAP-FRIPAPA} \code{INIAPMargarita} \code{IRA-92} \code{Jubile} \code{Kigega} \code{Kinga} \code{Kinigi} \code{Kisoro} \code{Kori-INIA} \code{LB-(AWASH)} \code{LB-III-(PRECODEPA)} \code{LB-VII-(Chaposa)} \code{LBr-40} \code{LT-5} \code{Maria-Bonita-INIA} \code{Maria-Tambeña} \code{MariaHuanca} \code{MF-IxTPS-13(CIP-SURAJ)} \code{Muruta} \code{Muziranzara} \code{P-9} \code{Perricholi} \code{PIMPERNEL} \code{REICHE} \code{Rukinzo} \code{SantaAna} \code{Tacna} \code{Tahuaquea} \code{TIBISAY} \code{Tubira} \code{UNICA} \code{Victoria} \code{Yana} \code{Yayla-Kizi}}
    \item{\code{m.oxa}}{a numeric vector}
    \item{\code{m.comas}}{a numeric vector}
  }
}

\source{
  Experimental field.
}
\references{
  International Potato Center. CIP - Lima Peru.
}
\examples{
library(agricolae)
data(ComasOxapampa)
str(ComasOxapampa)
}
\keyword{datasets}
