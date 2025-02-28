#' Graphique des professions des élus
#'
#' @param df Un `data.frame` contenant "Code.de.la.catégorie.socio.professionnelle".
#' @return Un graphique ggplot
#' @import ggplot2
#' @importFrom dplyr count arrange
#' @export
plot_code_professions <- function(df){
  df_profession <- df |>
    dplyr::count(Code.de.la.catégorie.socio.professionnelle, name = "nombre_elus") |>
    dplyr::arrange(desc(nombre_elus))

  ggplot(df_profession, aes(x = reorder(Code.de.la.catégorie.socio.professionnelle, nombre_elus), y = nombre_elus)) +
    geom_col(fill = "pink") +
    coord_flip() +
    labs(title = "Nombre d’élus par code professionnel",
         x = "Code Professionnel",
         y = "Nombre d’élus") +
    theme_minimal()
}
