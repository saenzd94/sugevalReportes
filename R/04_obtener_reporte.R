# Orquestación de consultas por tipo de entidad y ventanas de fechas.

#' Dividir un rango de fechas en ventanas
#'
#' @param from,to Fechas inicial y final inclusivas.
#' @param meses Máximo de meses por ventana.
#' @return Lista de vectores `Date` de longitud dos.
#' @keywords internal
.ventanas_fechas <- function(from, to, meses = 6L) {
  fechas <- .validar_rango(from, to)
  if (length(meses) != 1L || is.na(meses) || !is.numeric(meses) ||
      meses <= 0 || meses != floor(meses)) {
    stop("'meses' debe ser un entero positivo.", call. = FALSE)
  }

  ventanas <- list()
  inicio <- fechas$from
  while (inicio <= fechas$to) {
    fin <- min(.sumar_meses(inicio, meses) - 1L, fechas$to)
    ventanas[[length(ventanas) + 1L]] <- c(inicio, fin)
    inicio <- fin + 1L
  }
  ventanas
}

#' Obtener estados financieros de SUGEVAL
#'
#' Consulta uno o varios tipos de entidad y consolida las respuestas. Cuando
#' se especifica un rango mayor de seis meses, lo divide en varias peticiones
#' compatibles con la restricción del servicio.
#'
#' @param reporte Uno de `"balance"`, `"cuentas_orden"`,
#'   `"resultados_acumulado"` o `"resultados_mensual"`.
#' @param tipos Uno o varios de `"puesto"`, `"safi"` y `"fondo"`.
#' @param fecha Fecha de corte opcional.
#' @param from,to Límites inclusivos de un rango de fechas.
#' @param reciente Si es `TRUE`, solicita la información más reciente.
#' @param codigo Código opcional de una entidad. Cuando se usa, `tipos` debe
#'   contener un único elemento.
#' @param intentar_continuar Si es `TRUE`, conserva resultados parciales cuando
#'   falla una petición. Los mensajes se guardan en el atributo
#'   `errores_sugeval`.
#' @param verbose Si es `TRUE`, muestra progreso y URLs.
#' @return Un tibble consolidado con columnas `.tipo_entidad` y `.reporte`.
#' @export
#' @examples
#' \dontrun{
#' obtener_reporte_sugeval("balance", reciente = TRUE)
#' obtener_reporte_sugeval(
#'   "resultados_mensual",
#'   tipos = "safi",
#'   from = "2024-01-01",
#'   to = "2024-12-31"
#' )
#' }
obtener_reporte_sugeval <- function(
    reporte,
    tipos = c("puesto", "safi", "fondo"),
    fecha = NULL,
    from = NULL,
    to = NULL,
    reciente = FALSE,
    codigo = NULL,
    intentar_continuar = FALSE,
    verbose = FALSE) {
  reporte <- .validar_reporte(reporte)
  tipos <- unique(match.arg(
    tipos, choices = names(.SUGEVAL_TIPO), several.ok = TRUE
  ))
  modo <- .resolver_modo_consulta(fecha, from, to, reciente)

  if (!length(tipos)) {
    stop("'tipos' debe contener al menos un tipo de entidad.", call. = FALSE)
  }
  if (!is.null(codigo) && length(tipos) != 1L) {
    stop("Si indica 'codigo', 'tipos' debe contener un \u00fanico tipo.",
         call. = FALSE)
  }
  if (length(intentar_continuar) != 1L || is.na(intentar_continuar) ||
      !is.logical(intentar_continuar)) {
    stop("'intentar_continuar' debe ser TRUE o FALSE.", call. = FALSE)
  }

  ventanas <- if (identical(modo, "rango")) {
    .ventanas_fechas(from, to)
  } else {
    list(NULL)
  }

  resultados <- list()
  errores <- character()

  for (tipo in tipos) {
    for (ventana in ventanas) {
      periodo <- if (is.null(ventana)) {
        if (identical(modo, "fecha")) paste("fecha", as.character(fecha)) else modo
      } else {
        paste(as.character(ventana), collapse = " a ")
      }
      etiqueta <- sprintf("%s/%s [%s]", reporte, tipo, periodo)

      datos <- tryCatch(
        switch(
          modo,
          fecha = consultar_reporte(
            tipo, reporte, fecha = fecha, codigo = codigo, verbose = verbose
          ),
          rango = consultar_reporte(
            tipo, reporte,
            from = ventana[[1L]], to = ventana[[2L]],
            codigo = codigo, verbose = verbose
          ),
          reciente = consultar_reporte(
            tipo, reporte, reciente = TRUE,
            codigo = codigo, verbose = verbose
          )
        ),
        error = function(error) {
          mensaje <- sprintf("[%s] %s", etiqueta, conditionMessage(error))
          if (!isTRUE(intentar_continuar)) {
            stop(mensaje, call. = FALSE)
          }
          errores <<- c(errores, mensaje)
          NULL
        }
      )

      if (!is.null(datos) && nrow(datos)) {
        resultados[[length(resultados) + 1L]] <- datos
      }
    }
  }

  if (!length(resultados)) {
    if (length(errores)) {
      stop(
        "No se obtuvieron datos. Errores de las consultas:\n",
        paste(errores, collapse = "\n"),
        call. = FALSE
      )
    }
    return(tibble::tibble())
  }

  salida <- dplyr::bind_rows(resultados)
  if (length(errores)) {
    attr(salida, "errores_sugeval") <- errores
    warning(
      sprintf(
        "Se devolvieron resultados parciales; fallaron %d consulta(s). ",
        length(errores)
      ),
      "Revise attr(resultado, 'errores_sugeval').",
      call. = FALSE
    )
  }
  salida
}
