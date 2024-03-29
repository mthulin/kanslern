% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kanslern.R
\encoding{UTF-8}
\name{uka_table}
\alias{uka_table}
\title{Tabeller med svensk text och UKÄ:s färger}
\usage{
uka_table(df, ...)
}
\arguments{
\item{df}{En dataframe med 1-4 kolumner.}

\item{...}{Se \code{?ivo.table::ivo_table} för fler inställningar.}
}
\value{
En \code{flextable}.
}
\description{
Skapar en tabell via \code{ivo.table::ivo_table} med svensk text och UKÄ:s färger.
}
\examples{
# Exempeldata
example_data <- data.frame(Year = sample(2020:2023, 50, replace = TRUE),
A = sample(c("Type 1", "Type 2"), 50, replace = TRUE),
B = sample(c("Apples", "Oranges", "Bananas"), 50, replace = TRUE),
C = sample(c("Swedish", "Norwegian", "Chilean"), 50, replace = TRUE))

### 2-vägstabeller ###
data2 <- example_data |> dplyr::select(A, B)
data2_swap <- example_data |> dplyr::select(B, A)
uka_table(data2)
uka_table(data2_swap)
}
