#' Créer un objet de classe departement
#'
#' @param df Un `data.frame` contenant les données d'un département.
#' @return Un objet de classe "departement".
#' @export
creer_departement <- function(df) {
  structure(df, class = "departement")
}

#' Résumé d'un département
#'
#' @param x Un objet de classe "departement".
#' @return Un résumé des informations du département.
#' @export
summary.departement <- function(x) {
  summary(x)
}

#' Graphique des professions par département
#'
#' @param df Un objet de classe "departement".
#' @return Un graphique ggplot
#' @import ggplot2
#' @export
plot.departement <- function(df) {
  if (!inherits(df, "departement")) {
    stop("L'objet fourni n'est pas de classe 'departement'.")
  }

  df_profession <- df |>
    dplyr::count(Code.de.la.catégorie.socio.professionnelle, name = "nombre_elus") |>
    dplyr::arrange(desc(nombre_elus)) |>
    dplyr::slice_head(n = 10)

  ggplot(df_profession, aes(x = reorder(Code.de.la.catégorie.socio.professionnelle, nombre_elus), y = nombre_elus)) +
    geom_col(fill = "blue") +
    coord_flip() +
    labs(title = "Top 10 des professions des élus - Département",
         x = "Code Professionnel",
         y = "Nombre d’élus") +
    theme_minimal()
}
