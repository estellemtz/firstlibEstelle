#' Créer un objet de classe commune
#'
#' @param df Un `data.frame` contenant les données d'une commune.
#' @return Un objet de classe "commune".
#' @export
creer_commune <- function(df) {
  structure(df, class = "commune")
}

#' Résumé d'une commune
#'
#' @param x Un objet de classe "commune".
#' @return Un résumé des informations de la commune.
#' @export
summary.commune <- function(x) {
  summary(x)
}

#' Graphique des professions par commune
#'
#' @param df Un objet de classe "commune".
#' @return Un graphique ggplot
#' @import ggplot2
#' @export
plot.commune <- function(df) {
  if (!inherits(df, "commune")) {
    stop("L'objet fourni n'est pas de classe 'commune'.")
  }

  df_profession <- df |>
    dplyr::count(Code.de.la.catégorie.socio.professionnelle, name = "nombre_elus") |>
    dplyr::arrange(desc(nombre_elus))

  ggplot(df_profession, aes(x = reorder(Code.de.la.catégorie.socio.professionnelle, nombre_elus), y = nombre_elus)) +
    geom_col(fill = "pink") +
    coord_flip() +
    labs(title = "Nombre d’élus par code professionnel - Commune",
         x = "Code Professionnel",
         y = "Nombre d’élus") +
    theme_minimal()
}
