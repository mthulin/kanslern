#' @title Tabeller med svensk text och UKÄ:s färger
#' @name uka_table
#' @description Skapar en tabell via \code{ivo.table::ivo_table} med svensk text och UKÄ:s färger.
#' @param df En dataframe med 1-4 kolumner.
#' @param ... Se \code{?ivo.table::ivo_table} för fler inställningar.
#' @return En \code{flextable}.
#' @encoding UTF-8
#' @examples # Exempeldata
#' example_data <- data.frame(Year = sample(2020:2023, 50, replace = TRUE),
#' A = sample(c("Type 1", "Type 2"), 50, replace = TRUE),
#' B = sample(c("Apples", "Oranges", "Bananas"), 50, replace = TRUE),
#' C = sample(c("Swedish", "Norwegian", "Chilean"), 50, replace = TRUE))
#'
#' ### 2-vägstabeller ###
#' data2 <- example_data |> dplyr::select(A, B)
#' data2_swap <- example_data |> dplyr::select(B, A)
uka_table <- function(df, ...) {
  ivo.table::ivo_table(df, missing_string = "(Saknas)", sums_string = "Summa", color = uka_farg("lila1"), font_name = "Arial", ...)
}
