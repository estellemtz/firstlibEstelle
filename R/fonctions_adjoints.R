#' Compter le nombre d'adjoints
#'
#' Cette fonction compte le nombre d'adjoints dans une liste d'élus.
#'
#' @param df Un `data.frame` contenant les élus avec une colonne "Libellé.de.la.fonction".
#' @return Un entier correspondant au nombre d'adjoints.
#' @importFrom stringr str_detect
#' @export
#' @examples
#' df <- data.frame(Libellé.de.la.fonction = c("Maire", "Adjoint", "Adjoint", "Conseiller"))
#' compter_nombre_d_adjoints(df)
compter_nombre_d_adjoints <- function(df) {

  schema <- c("Code.du.département", "Libellé.du.département",
              "Code.de.la.collectivité.à.statut.particulier",
              "Libellé.de.la.collectivité.à.statut.particulier",
              "Code.de.la.commune", "Libellé.de.la.commune",
              "Nom.de.l.élu", "Prénom.de.l.élu",
              "Code.sexe", "Date.de.naissance",
              "Code.de.la.catégorie.socio.professionnelle",
              "Libellé.de.la.catégorie.socio.professionnelle",
              "Date.de.début.du.mandat", "Libellé.de.la.fonction",
              "Date.de.début.de.la.fonction", "Code.nationalité")

  # Vérification des colonnes
  if (!identical(colnames(df), schema)) {
    stop("Les colonnes du DataFrame ne correspondent pas au schéma attendu.")
  }

  # Compter le nombre d'adjoints
  nb_adjoints <- sum(stringr::str_detect(df$Libellé.de.la.fonction, regex("adjoint", ignore_case = TRUE)))

  return(nb_adjoints)
}
