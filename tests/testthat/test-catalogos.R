test_that("listar_fondos envía siempre CodigoRegulado", {
  captura <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    .sugeval_get = function(metodo, params = list(), verbose = FALSE) {
      captura$metodo <- metodo
      captura$params <- params
      list(Fondos = data.frame(Codigo = "F1", Estado = "Activo"))
    },
    .package = "sugevalReportes"
  )

  salida <- listar_fondos()
  expect_equal(nrow(salida), 1)
  expect_identical(captura$metodo, "ObtenerListadoTodosFondosDeSafi")
  expect_identical(captura$params$CodigoRegulado$valor, "")
  expect_identical(captura$params$CodigoRegulado$tipo, "string")
})

test_that("el filtro de fondos requiere una SAFI y normaliza el estado", {
  expect_error(listar_fondos(estado = "ACTIVO"), "codigo_safi")

  captura <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    .sugeval_get = function(metodo, params = list(), verbose = FALSE) {
      captura$metodo <- metodo
      captura$params <- params
      list(Fondos = data.frame())
    },
    .package = "sugevalReportes"
  )

  listar_fondos("S1", estado = "inactivo")
  expect_identical(
    captura$metodo,
    "ObtenerListadoTodosFondosDeSafiPorEstado"
  )
  expect_identical(captura$params$Estado$valor, "Inactivo")
})

test_that("puestos y SAFI validan sus estados", {
  expect_error(listar_puestos_bolsa("DESCONOCIDO"), "arg")
  expect_error(listar_safis("DESCONOCIDO"), "arg")
})
