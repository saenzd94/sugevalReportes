test_that("las ventanas respetan meses calendario y los fines de mes", {
  ventanas <- sugevalReportes:::.ventanas_fechas(
    "2024-01-31", "2024-08-31", meses = 6
  )
  expect_equal(ventanas[[1]], as.Date(c("2024-01-31", "2024-07-30")))
  expect_equal(ventanas[[2]], as.Date(c("2024-07-31", "2024-08-31")))
})

test_that("la interfaz de alto nivel divide rangos amplios", {
  llamadas <- list()
  testthat::local_mocked_bindings(
    consultar_reporte = function(tipo, reporte, fecha = NULL, from = NULL,
                                 to = NULL, reciente = FALSE, codigo = NULL,
                                 verbose = FALSE) {
      llamadas[[length(llamadas) + 1L]] <<- c(from, to)
      tibble::tibble(FechaCorte = as.Date(to), .tipo_entidad = tipo,
                     .reporte = reporte)
    },
    .package = "sugevalReportes"
  )

  salida <- obtener_reporte_sugeval(
    "balance", tipos = "puesto",
    from = "2024-01-01", to = "2024-12-31"
  )
  expect_length(llamadas, 2)
  expect_equal(llamadas[[1]], as.Date(c("2024-01-01", "2024-06-30")))
  expect_equal(llamadas[[2]], as.Date(c("2024-07-01", "2024-12-31")))
  expect_equal(nrow(salida), 2)
})

test_that("los errores parciales quedan registrados", {
  testthat::local_mocked_bindings(
    consultar_reporte = function(tipo, reporte, fecha = NULL, from = NULL,
                                 to = NULL, reciente = FALSE, codigo = NULL,
                                 verbose = FALSE) {
      if (identical(tipo, "safi")) stop("falla simulada")
      tibble::tibble(valor = 1, .tipo_entidad = tipo, .reporte = reporte)
    },
    .package = "sugevalReportes"
  )

  expect_warning(
    salida <- obtener_reporte_sugeval(
      "balance", tipos = c("puesto", "safi"), reciente = TRUE,
      intentar_continuar = TRUE
    ),
    "resultados parciales"
  )
  expect_equal(nrow(salida), 1)
  expect_length(attr(salida, "errores_sugeval"), 1)
})

test_that("si todo falla no se devuelve una tabla vacía silenciosa", {
  testthat::local_mocked_bindings(
    consultar_reporte = function(...) stop("falla simulada"),
    .package = "sugevalReportes"
  )
  expect_error(
    obtener_reporte_sugeval(
      "balance", tipos = "puesto", reciente = TRUE,
      intentar_continuar = TRUE
    ),
    "No se obtuvieron datos"
  )
})
