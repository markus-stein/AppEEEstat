% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QuantAux.R
\name{QuantAux}
\alias{QuantAux}
\title{Criadora de intervalos de classe}
\usage{
QuantAux(y)
}
\arguments{
\item{y}{Vetor numérico}
}
\value{
Variável tipo fator
}
\description{
Cria intervalos de classe para vetores numericos
}
\examples{
var <- mtcars$wt
QuantAux(var)

}
