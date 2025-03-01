test_that("trouver_l_elu_le_plus_age fonctionne correctement", {
  df_test <- data.frame(
    "Nom.de.l.elu" = c("Dupont", "Durand", "Martin"),
    "Prenom.de.l.elu" = c("Jean", "Paul", "Sophie"),
    "Date.de.naissance" = c("01/01/1950", "02/02/1990", "03/03/2000")
  )

  result <- trouver_l_elu_le_plus_age(df_test)

  expect_equal(result$Nom.de.l.elu, "Dupont")
  expect_equal(result$Age, as.integer(difftime(Sys.Date(), as.Date("01/01/1950", format = "%d/%m/%Y"), units = "days") / 365.25))
})

test_that("trouver_l_elu_le_plus_age renvoie une erreur si colonne manquante", {
  df_mauvais <- data.frame("Nom.de.l.elu" = c("Dupont"), "Prenom.de.l.elu" = c("Jean"))

  expect_error(trouver_l_elu_le_plus_age(df_mauvais))
})
