# Información diaria de fondos de inversión.

.COLUMNAS_NUMERICAS_FONDOS_DIARIOS <- c(
  "CodigoMoneda", "CuentasAbiertas", "CajaYBancos",
  "NumeroValoresParticipacion", "ActivoNeto",
  "RendimientoTotalUltimosTreintaDias",
  "RendimientoTotalUltimosDoceMeses",
  "RendimientoTotalLiquidacionDoceMeses",
  "ComisionActivoNeto", "ComisionRendimiento",
  "MontoSuscripciones", "MontoLiquidaciones"
)

.normalizar_informacion_diaria_fondos <- function(datos) {
  datos <- tibble::as_tibble(datos)
  if ("Fecha" %in% names(datos)) {
    valor <- as.character(datos$Fecha)
    datos$Fecha <- suppressWarnings(as.Date(substr(valor, 1L, 10L)))
  }

  for (columna in intersect(
    .COLUMNAS_NUMERICAS_FONDOS_DIARIOS, names(datos)
  )) {
    datos[[columna]] <- suppressWarnings(as.numeric(datos[[columna]]))
  }

  if (all(c("ActivoNeto", "NumeroValoresParticipacion") %in% names(datos))) {
    datos$ValorParticipacionCalculado <- ifelse(
      is.finite(datos$ActivoNeto) &
        is.finite(datos$NumeroValoresParticipacion) &
        datos$NumeroValoresParticipacion > 0,
      datos$ActivoNeto / datos$NumeroValoresParticipacion,
      NA_real_
    )
  }
  datos
}

#' Consultar información diaria de fondos de inversión
#'
#' Obtiene la tabla oficial de información diaria de fondos para una fecha de
#' corte o para un rango inclusivo no mayor de seis meses. El resultado incluye
#' identificadores, clasificación del fondo, moneda, cuentas abiertas, caja y
#' bancos, series, valores de participación, activo neto, rendimientos,
#' comisiones, suscripciones y liquidaciones.
#'
#' `ValorParticipacionCalculado` no es un campo enviado directamente por
#' SUGEVAL. Se calcula como `ActivoNeto / NumeroValoresParticipacion` cuando
#' ambos valores están disponibles y el denominador es positivo.
#'
#' @param fecha Fecha de corte. Es mutuamente excluyente con `from` y `to`.
#' @param from,to Límites inclusivos de un rango no mayor de seis meses.
#' @param verbose Si es `TRUE`, muestra la URL consultada sin revelar el token.
#' @return Un tibble. La columna `Fecha` se convierte a `Date`; las notas y el
#'   encabezado oficiales se conservan en el atributo `encabezado_sugeval`.
#' @export
#' @examples
#' \dontrun{
#' diarios <- consultar_informacion_diaria_fondos(fecha = "2026-07-22")
#' historico <- consultar_informacion_diaria_fondos(
#'   from = "2026-07-01", to = "2026-07-22"
#' )
#' }
consultar_informacion_diaria_fondos <- function(
    fecha = NULL, from = NULL, to = NULL, verbose = FALSE) {
  if (xor(is.null(from), is.null(to))) {
    stop("'from' y 'to' deben indicarse juntos.", call. = FALSE)
  }
  tiene_fecha <- !is.null(fecha)
  tiene_rango <- !is.null(from) && !is.null(to)
  if (sum(c(tiene_fecha, tiene_rango)) != 1L) {
    stop(
      "Indique exactamente un modo: 'fecha' o 'from' junto con 'to'.",
      call. = FALSE
    )
  }

  if (tiene_fecha) {
    metodo <- "ObtenerInformacionDiariaFondosPorFechaCorte"
    parametros <- list(
      FechaCorte = list(
        valor = .validar_fecha(fecha, "fecha"), tipo = "date"
      )
    )
  } else {
    fechas <- .validar_rango(from, to, max_meses = 6)
    metodo <- "ObtenerInformacionDiariaFondosPorRangoFechas"
    parametros <- list(
      FechaInicio = list(valor = fechas$from, tipo = "date"),
      FechaFinal = list(valor = fechas$to, tipo = "date")
    )
  }

  respuesta <- .sugeval_get(metodo, parametros, verbose = verbose)
  datos <- .extraer_datos(
    respuesta, preferidos = c("InformacionDiariaFondos", "value")
  )
  datos <- .normalizar_informacion_diaria_fondos(datos)
  attr(datos, "encabezado_sugeval") <- respuesta$Encabezado
  datos
}

#' Exportar información diaria de fondos a CSV o XLSX
#'
#' @param archivo Ruta de salida terminada en `.csv` o `.xlsx`.
#' @param datos Tabla obtenida con
#'   [consultar_informacion_diaria_fondos()].
#' @return Invisiblemente, la ruta absoluta del archivo creado.
#' @export
#' @examples
#' \dontrun{
#' diarios <- consultar_informacion_diaria_fondos(fecha = "2026-07-22")
#' exportar_informacion_diaria_fondos("fondos_diarios.xlsx", diarios)
#' }
exportar_informacion_diaria_fondos <- function(archivo, datos) {
  archivo <- .validar_texto_unico(archivo, "archivo")
  extension <- tolower(tools::file_ext(archivo))
  if (!extension %in% c("csv", "xlsx")) {
    stop("'archivo' debe terminar en .csv o .xlsx.", call. = FALSE)
  }
  if (!is.data.frame(datos)) {
    stop("'datos' debe ser un data.frame o tibble.", call. = FALSE)
  }

  directorio <- dirname(archivo)
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(directorio)) {
    stop("No fue posible crear el directorio de salida.", call. = FALSE)
  }

  salida <- .proteger_formulas_hoja(datos)
  if (identical(extension, "csv")) {
    temporal <- tempfile(fileext = ".csv")
    on.exit(unlink(temporal, force = TRUE), add = TRUE)
    utils::write.csv(
      salida, file = temporal, row.names = FALSE,
      na = "", fileEncoding = "UTF-8"
    )
    contenido <- readBin(temporal, what = "raw", n = file.info(temporal)$size)
    writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), contenido), archivo)
  } else {
    if (!requireNamespace("writexl", quietly = TRUE)) {
      stop(
        "Para exportar XLSX instale el paquete 'writexl': ",
        "install.packages('writexl').",
        call. = FALSE
      )
    }
    writexl::write_xlsx(list(InformacionDiariaFondos = salida), path = archivo)
  }

  invisible(normalizePath(archivo, winslash = "/", mustWork = TRUE))
}
