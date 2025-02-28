#' Trouver l'élu le plus âgé
#'
#' @param df Un `data.frame` contenant les élus avec "Date.de.naissance".
#' @return Un `data.frame` avec l'élu le plus âgé.
#' @importFrom dplyr mutate slice select
#' @export
trouver_l_elu_le_plus_age <- function(df) {
  df <- dplyr::mutate(df, Date.de.naissance = as.Date(Date.de.naissance, format = "%d/%m/%Y"))
  elu_plus_age <- df |> dplyr::slice(which.min(Date.de.naissance)) |>
    dplyr::mutate(Age = as.integer(difftime(Sys.Date(), Date.de.naissance, units = "days") / 365.25)) |>
    dplyr::select(Nom.de.l.élu, Prénom.de.l.élu, Age)
  return(elu_plus_age)
}

#' Calculer la distribution des âges
#'
#' @param df Un `data.frame` contenant "Date.de.naissance".
#' @return Un vecteur des quantiles de l'âge.
#' @importFrom dplyr mutate
#' @export
calcul_distribution_age <- function(df) {
  df <- dplyr::mutate(df, Age = as.integer(difftime(Sys.Date(), as.Date(Date.de.naissance, format = "%d/%m/%Y"), units = "days") / 365.25))
  return(quantile(df$Age, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))
}
