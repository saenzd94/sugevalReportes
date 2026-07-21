test_that("las URL OData incluyen paréntesis y parámetros correctos", {
  expect_equal(
    sugevalReportes:::.odata_url("ObtenerAlgo"),
    paste0(sugevalReportes:::.SUGEVAL_BASE_URL, "/ObtenerAlgo()")
  )

  url <- sugevalReportes:::.odata_url(
    "ObtenerAlgo",
    list(
      Codigo = list(valor = "O'Brien", tipo = "string"),
      Fecha = list(valor = "2024-06-30", tipo = "date"),
      Estado = list(valor = NA, tipo = "null")
    )
  )
  expect_match(url, "Codigo='O''Brien'", fixed = TRUE)
  expect_match(url, "Fecha=2024-06-30", fixed = TRUE)
  expect_match(url, "Estado=null", fixed = TRUE)
})

test_that("los parámetros OData inválidos fallan antes de la red", {
  expect_error(
    sugevalReportes:::.odata_valor("no-es-fecha", "date"),
    "formato"
  )
  expect_error(sugevalReportes:::.odata_valor(1.5, "int"), "entero")
  expect_error(
    sugevalReportes:::.odata_url("método inválido"),
    "caracteres"
  )
})

test_that("el token recomendado tiene precedencia y Bearer se normaliza", {
  withr::local_envvar(c(
    SUGEVAL_TOKEN = "  Bearer token-nuevo  ",
    token_sugeval = "token-antiguo"
  ))
  expect_true(sugeval_token_configurado())
  expect_identical(sugevalReportes:::.sugeval_token(), "token-nuevo")
})

test_that("se conserva compatibilidad con token_sugeval", {
  withr::local_envvar(c(SUGEVAL_TOKEN = NA, token_sugeval = "legado"))
  expect_true(sugeval_token_configurado())
  expect_identical(sugevalReportes:::.sugeval_token(), "legado")

  withr::local_envvar(c(SUGEVAL_TOKEN = NA, token_sugeval = NA))
  expect_false(sugeval_token_configurado())
  expect_error(sugevalReportes:::.sugeval_token(), "No se encontró")
})

test_that("la extracción prioriza la tabla de datos", {
  respuesta <- list(
    Encabezado = list(FechaDeConsulta = "2026-01-01"),
    Catalogos = data.frame(Codigo = c("1", "2"), Nombre = c("A", "B"))
  )
  salida <- sugevalReportes:::.extraer_datos(respuesta)
  expect_s3_class(salida, "tbl_df")
  expect_equal(nrow(salida), 2)
  expect_named(salida, c("Codigo", "Nombre"))
})
