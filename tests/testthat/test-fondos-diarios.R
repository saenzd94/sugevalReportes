test_that("la información diaria por fecha usa el método y esquema oficiales", {
  captura <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    .sugeval_get = function(metodo, params = list(), verbose = FALSE) {
      captura$metodo <- metodo
      captura$params <- params
      list(
        Encabezado = list(
          FechaCorte = "2026-07-22",
          NotaInfoDiariaFISeriados = "Nota oficial"
        ),
        InformacionDiariaFondos = data.frame(
          Fecha = "2026-07-22",
          CodigoFondo = "F1",
          ActivoNeto = 250,
          NumeroValoresParticipacion = 100,
          ComisionRendimiento = NA
        )
      )
    },
    .package = "sugevalReportes"
  )

  salida <- consultar_informacion_diaria_fondos(fecha = "2026-07-22")
  expect_identical(
    captura$metodo, "ObtenerInformacionDiariaFondosPorFechaCorte"
  )
  expect_named(captura$params, "FechaCorte")
  expect_s3_class(salida$Fecha, "Date")
  expect_type(salida$ComisionRendimiento, "double")
  expect_equal(salida$ValorParticipacionCalculado, 2.5)
  expect_false(is.null(attr(salida, "encabezado_sugeval")))
})

test_that("la información diaria por rango valida y envía ambos límites", {
  captura <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    .sugeval_get = function(metodo, params = list(), verbose = FALSE) {
      captura$metodo <- metodo
      captura$params <- params
      list(InformacionDiariaFondos = data.frame())
    },
    .package = "sugevalReportes"
  )

  consultar_informacion_diaria_fondos(
    from = "2026-07-01", to = "2026-07-22"
  )
  expect_identical(
    captura$metodo, "ObtenerInformacionDiariaFondosPorRangoFechas"
  )
  expect_named(captura$params, c("FechaInicio", "FechaFinal"))

  expect_error(
    consultar_informacion_diaria_fondos(from = "2026-07-01"),
    "deben indicarse juntos"
  )
  expect_error(
    consultar_informacion_diaria_fondos(
      fecha = "2026-07-22", from = "2026-07-01", to = "2026-07-22"
    ),
    "exactamente un modo"
  )
  expect_error(
    consultar_informacion_diaria_fondos(
      from = "2026-01-01", to = "2026-07-01"
    ),
    "máximo de 6 meses"
  )
})

test_that("la información diaria se exporta a CSV y XLSX", {
  datos <- tibble::tibble(
    CodigoFondo = c("F1", "=2+2"),
    Fecha = as.Date(c("2026-07-21", "2026-07-22")),
    ActivoNeto = c(100, 110)
  )
  csv <- tempfile(fileext = ".csv")
  ruta <- exportar_informacion_diaria_fondos(csv, datos)
  contenido <- readBin(ruta, what = "raw", n = file.info(ruta)$size)
  expect_identical(
    as.integer(contenido[1:3]), c(0xEF, 0xBB, 0xBF) |> as.integer()
  )
  expect_match(rawToChar(contenido[-(1:3)]), "'=2\\+2")

  testthat::skip_if_not_installed("writexl")
  xlsx <- tempfile(fileext = ".xlsx")
  exportar_informacion_diaria_fondos(xlsx, datos)
  expect_identical(rawToChar(readBin(xlsx, "raw", n = 2L)), "PK")
})
