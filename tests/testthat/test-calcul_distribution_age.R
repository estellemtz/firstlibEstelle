test_that("calcul_distribution_age fonctionne correctement", {
  df_test <- data.frame(
    "Date.de.naissance" = c("01/01/1980", "02/02/1990", "03/03/2000")
  )

  result <- calcul_distribution_age(df_test)
  expect_length(result, 5)
})

test_that("calcul_distribution_age renvoie une erreur si colonne manquante", {
  df_mauvais <- data.frame("Nom.de.l.elu" = c("Dupont"))

  expect_error(calcul_distribution_age(df_mauvais))
})
