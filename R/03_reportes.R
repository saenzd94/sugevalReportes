# Estados financieros de puestos, SAFI y fondos de inversión.

.SUGEVAL_TIPO <- list(
  puesto = list(todos = "TodosPuestos", uno = "UnPuesto"),
  safi = list(todos = "TodasSafis", uno = "UnaSafi"),
  fondo = list(todos = "TodosFondos", uno = "UnFondo")
)

.SUGEVAL_REPORTE <- list(
  balance = "BalanceGeneral",
  cuentas_orden = "CuentaOrden",
  resultados_acumulado = "EstadoResultadoAcumulado",
  resultados_mensual = "EstadoResultadoMensual"
)

.validar_tipo <- function(tipo) {
  match.arg(tipo, names(.SUGEVAL_TIPO))
}

.validar_reporte <- function(reporte) {
  match.arg(reporte, names(.SUGEVAL_REPORTE))
}

#' Construir el nombre de un método de estado financiero
#'
#' @param tipo Tipo de entidad.
#' @param reporte Tipo de estado financiero.
#' @param variante `"fecha"`, `"reciente"` o `"rango"`.
#' @param alcance `"todos"` o `"uno"`.
#' @return Nombre exacto del método OData.
#' @keywords internal
.metodo_reporte <- function(tipo, reporte, variante,
                            alcance = c("todos", "uno")) {
  tipo <- .validar_tipo(tipo)
  reporte <- .validar_reporte(reporte)
  variante <- match.arg(variante, c("fecha", "reciente", "rango"))
  alcance <- match.arg(alcance)

  variante_texto <- switch(
    variante,
    fecha = "PorFechaCorte",
    reciente = "PorInfoReciente",
    rango = "PorRangoFechas"
  )

  paste0(
    "Obtener",
    .SUGEVAL_REPORTE[[reporte]],
    .SUGEVAL_TIPO[[tipo]][[alcance]],
    variante_texto
  )
}

.resolver_modo_consulta <- function(fecha = NULL, from = NULL, to = NULL,
                                    reciente = FALSE) {
  if (length(reciente) != 1L || is.na(reciente) || !is.logical(reciente)) {
    stop("'reciente' debe ser TRUE o FALSE.", call. = FALSE)
  }
  if (xor(is.null(from), is.null(to))) {
    stop("'from' y 'to' deben indicarse juntos.", call. = FALSE)
  }

  tiene_fecha <- !is.null(fecha)
  tiene_rango <- !is.null(from) && !is.null(to)
  modos <- sum(c(tiene_fecha, tiene_rango, isTRUE(reciente)))
  if (modos != 1L) {
    stop(
      "Indique exactamente un modo: 'fecha', 'from' junto con 'to', o ",
      "reciente=TRUE.",
      call. = FALSE
    )
  }

  if (tiene_fecha) "fecha" else if (tiene_rango) "rango" else "reciente"
}

.sumar_meses <- function(fecha, meses) {
  fecha <- .validar_fecha(fecha, "fecha")
  meses <- as.integer(meses)
  if (length(meses) != 1L || is.na(meses)) {
    stop("'meses' debe ser un entero.", call. = FALSE)
  }

  anio <- as.integer(format(fecha, "%Y"))
  mes <- as.integer(format(fecha, "%m"))
  dia <- as.integer(format(fecha, "%d"))
  indice <- anio * 12L + (mes - 1L) + meses
  nuevo_anio <- indice %/% 12L
  nuevo_mes <- indice %% 12L + 1L
  primer_dia <- as.Date(sprintf("%04d-%02d-01", nuevo_anio, nuevo_mes))
  primer_dia_siguiente <- seq(
    primer_dia, by = "1 month", length.out = 2L
  )[[2L]]
  ultimo_dia <- as.integer(format(primer_dia_siguiente - 1, "%d"))

  primer_dia + min(dia, ultimo_dia) - 1L
}

.validar_rango <- function(from, to, max_meses = NULL) {
  from <- .validar_fecha(from, "from")
  to <- .validar_fecha(to, "to")
  if (from > to) {
    stop("'from' es posterior a 'to'.", call. = FALSE)
  }
  if (!is.null(max_meses) && to >= .sumar_meses(from, max_meses)) {
    stop(
      sprintf(
        "El rango excede el m\u00e1ximo de %s meses permitido por SUGEVAL.",
        max_meses
      ),
      call. = FALSE
    )
  }
  list(from = from, to = to)
}

#' Consultar un estado financiero de SUGEVAL
#'
#' Esta función de bajo nivel realiza una sola petición. Los rangos no pueden
#' superar seis meses. Para rangos mayores use
#' [obtener_reporte_sugeval()].
#'
#' @param tipo Uno de `"puesto"`, `"safi"` o `"fondo"`.
#' @param reporte Uno de `"balance"`, `"cuentas_orden"`,
#'   `"resultados_acumulado"` o `"resultados_mensual"`.
#' @param fecha Fecha de corte.
#' @param from,to Límites inclusivos de un rango no mayor de seis meses.
#' @param reciente Si es `TRUE`, solicita la información más reciente.
#' @param codigo Código opcional de una entidad. Para los fondos corresponde al
#'   código devuelto por [listar_fondos()].
#' @param verbose Si es `TRUE`, muestra la URL consultada.
#' @return Un tibble con columnas de trazabilidad `.tipo_entidad` y `.reporte`.
#' @export
#' @examples
#' \dontrun{
#' consultar_reporte("puesto", "balance", fecha = "2024-06-30")
#' consultar_reporte("fondo", "resultados_mensual", reciente = TRUE)
#' }
consultar_reporte <- function(tipo, reporte, fecha = NULL, from = NULL,
                              to = NULL, reciente = FALSE, codigo = NULL,
                              verbose = FALSE) {
  tipo <- .validar_tipo(tipo)
  reporte <- .validar_reporte(reporte)
  modo <- .resolver_modo_consulta(fecha, from, to, reciente)
  alcance <- if (is.null(codigo)) "todos" else "uno"

  if (!is.null(codigo)) {
    codigo <- .validar_texto_unico(codigo, "codigo")
  }

  parametros <- switch(
    modo,
    fecha = list(
      Fecha = list(valor = .validar_fecha(fecha, "fecha"), tipo = "date")
    ),
    rango = {
      fechas <- .validar_rango(from, to, max_meses = 6)
      list(
        FechaInicio = list(valor = fechas$from, tipo = "date"),
        FechaFinal = list(valor = fechas$to, tipo = "date")
      )
    },
    reciente = list()
  )

  if (identical(alcance, "uno")) {
    # El manual oficial usa CodigoRegulado también para un fondo individual.
    parametros <- c(
      list(CodigoRegulado = list(valor = codigo, tipo = "string")),
      parametros
    )
  }

  metodo <- .metodo_reporte(tipo, reporte, modo, alcance)
  respuesta <- .sugeval_get(metodo, parametros, verbose = verbose)
  datos <- .extraer_datos(respuesta)

  if (nrow(datos)) {
    datos$.tipo_entidad <- tipo
    datos$.reporte <- reporte
  }
  datos
}
