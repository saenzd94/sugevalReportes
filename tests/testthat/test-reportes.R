test_that("los nombres de métodos coinciden con el manual oficial", {
  metodo <- sugevalReportes:::.metodo_reporte(
    "safi", "balance", "reciente", "todos"
  )
  expect_identical(metodo, "ObtenerBalanceGeneralTodasSafisPorInfoReciente")

  metodo <- sugevalReportes:::.metodo_reporte(
    "fondo", "resultados_mensual", "rango", "uno"
  )
  expect_identical(
    metodo,
    "ObtenerEstadoResultadoMensualUnFondoPorRangoFechas"
  )
})

test_that("una consulta de fondo usa CodigoRegulado", {
  captura <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    .sugeval_get = function(metodo, params = list(), verbose = FALSE) {
      captura$metodo <- metodo
      captura$params <- params
      list(BalanceGeneral = data.frame(Cuenta = "1", Monto = 10))
    },
    .package = "sugevalReportes"
  )

  salida <- consultar_reporte(
    "fondo", "balance", reciente = TRUE, codigo = "F1"
  )
  expect_true("CodigoRegulado" %in% names(captura$params))
  expect_false("CodigoFondo" %in% names(captura$params))
  expect_identical(captura$params$CodigoRegulado$valor, "F1")
  expect_identical(salida$.tipo_entidad, "fondo")
  expect_identical(salida$.reporte, "balance")
})

test_that("los modos de consulta son mutuamente excluyentes", {
  expect_error(
    consultar_reporte("puesto", "balance"),
    "exactamente un modo"
  )
  expect_error(
    consultar_reporte(
      "puesto", "balance", fecha = "2024-01-01", reciente = TRUE
    ),
    "exactamente un modo"
  )
  expect_error(
    consultar_reporte("puesto", "balance", from = "2024-01-01"),
    "deben indicarse juntos"
  )
})

test_that("la consulta de bajo nivel limita el rango a seis meses", {
  expect_error(
    consultar_reporte(
      "puesto", "balance",
      from = "2024-01-01", to = "2024-07-01"
    ),
    "máximo de 6 meses"
  )
})
