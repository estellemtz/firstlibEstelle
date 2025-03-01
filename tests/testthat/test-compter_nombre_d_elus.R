
test_that("compter_nombre_d_elus fonctionne correctement", {
  df_test <- data.frame(
    "Nom.de.l.elu" = c("Dupont", "Durand", "Martin"),
    "Prenom.de.l.elu" = c("Jean", "Paul", "Sophie"),
    "Date.de.naissance" = c("01/01/1980", "02/02/1990", "03/03/2000")
  )

  expect_equal(compter_nombre_d_elus(df_test), 3)
})

test_that("compter_nombre_d_elus renvoie une erreur si colonnes manquantes", {
  df_mauvais <- data.frame("Nom.de.l.elu" = c("Dupont"), "Prenom.de.l.elu" = c("Jean"))

  expect_error(compter_nombre_d_elus(df_mauvais))
})
