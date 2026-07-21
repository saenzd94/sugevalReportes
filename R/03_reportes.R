# ══════════════════════════════════════════════════════════════════════════════
#  03_reportes.R  —  Estados financieros de SUGEVAL (3 tipos de entidad)
#
#  SUGEVAL organiza la información en tres tipos de entidad, cada uno con sus
#  propios métodos:
#     • Puestos de Bolsa   (puesto)
#     • SAFIs              (safi)
#     • Fondos de Inversión (fondo)
#
#  Y tres estados financieros principales:
#     • Balance General            (balance)
#     • Cuentas de Orden           (cuentas_orden)
#     • Estado de Resultados Acum. (resultados_acumulado)
#     • Estado de Resultados Mens. (resultados_mensual)
#
#  Cada combinación (tipo × reporte) tiene tres variantes de consulta:
#     • PorFechaCorte(Fecha=...)                    — todas las entidades, 1 fecha
#     • PorInfoReciente()                           — última info disponible
#     • PorRangoFechas(FechaInicio=,FechaFinal=)    — rango (≤ 6 meses)
#
#  Los nombres de método siguen un patrón regular que se arma abajo. El rango
#  máximo es de 6 meses móviles para estados financieros; para rangos mayores
#  se itera por ventanas (ver obtener_reporte_sugeval con from/to amplios).
# ══════════════════════════════════════════════════════════════════════════════

# ── Fragmentos de nombre de método por tipo de entidad ────────────────────────
# Estructura de los nombres:  Obtener{REPORTE}Todos{TIPO_PLURAL}Por{VARIANTE}
#                             Obtener{REPORTE}Un{TIPO_SING}Por{VARIANTE}
.SUGEVAL_TIPO <- list(
  puesto = list(plural = "Puestos",     singular = "Puesto",  cod_param = "CodigoRegulado"),
  safi   = list(plural = "TodasSafis",  singular = "UnaSafi", cod_param = "CodigoRegulado",
                # Las SAFIs rompen el patrón "Todos{Plural}": usan "TodasSafis"
                # y "UnaSafi" directamente (sin el prefijo Todos/Un adicional).
                irregular = TRUE),
  fondo  = list(plural = "Fondos",      singular = "Fondo",   cod_param = "CodigoFondo")
)

# ── Fragmento de nombre por reporte ──────────────────────────────────────────
.SUGEVAL_REPORTE <- list(
  balance              = "BalanceGeneral",
  cuentas_orden        = "CuentaOrden",
  resultados_acumulado = "EstadoResultadoAcumulado",
  resultados_mensual   = "EstadoResultadoMensual"
)

#' Construye el nombre del método para una combinación tipo/reporte/variante
#'
#' @param tipo "puesto", "safi", "fondo".
#' @param reporte clave de .SUGEVAL_REPORTE.
#' @param variante "fecha", "reciente", "rango".
#' @param alcance "todos" o "uno".
#' @keywords internal
.metodo_reporte <- function(tipo, reporte, variante, alcance = "todos") {
  rep_frag <- .SUGEVAL_REPORTE[[reporte]]
  ti       <- .SUGEVAL_TIPO[[tipo]]
  if (is.null(rep_frag) || is.null(ti)) {
    stop("Combinación tipo/reporte no válida: ", tipo, " / ", reporte, call. = FALSE)
  }

  var_frag <- switch(variante,
    fecha    = "PorFechaCorte",
    reciente = "PorInfoReciente",
    rango    = "PorRangoFechas",
    stop("variante no válida: ", variante, call. = FALSE)
  )

  # Segmento de alcance (todas las entidades vs una)
  if (identical(alcance, "todos")) {
    if (isTRUE(ti$irregular)) {
      # SAFIs: Obtener{Reporte}TodasSafisPor{Variante}
      sprintf("Obtener%sTodasSafis%s", rep_frag, var_frag)
    } else {
      # Puestos/Fondos: Obtener{Reporte}Todos{Plural}Por{Variante}
      sprintf("Obtener%sTodos%s%s", rep_frag, ti$plural, var_frag)
    }
  } else {
    if (isTRUE(ti$irregular)) {
      sprintf("Obtener%sUnaSafi%s", rep_frag, var_frag)
    } else {
      sprintf("Obtener%sUn%s%s", rep_frag, ti$singular, var_frag)
    }
  }
}

#' Consulta un estado financiero de SUGEVAL para un tipo de entidad
#'
#' Nivel bajo: una sola llamada (una fecha de corte, o info reciente, o un
#' rango ≤ 6 meses). Para rangos amplios use obtener_reporte_sugeval().
#'
#' @param tipo "puesto", "safi", "fondo".
#' @param reporte "balance", "cuentas_orden", "resultados_acumulado",
#'   "resultados_mensual".
#' @param fecha fecha de corte (Date o "aaaa-MM-dd"). Si se da, usa PorFechaCorte.
#' @param from,to si ambos se dan, usa PorRangoFechas (≤ 6 meses).
#' @param reciente si TRUE (y sin fecha/rango), usa PorInfoReciente.
#' @param codigo opcional; si se indica, consulta UNA entidad (CodigoRegulado o
#'   CodigoFondo según el tipo). Si es NULL, consulta TODAS.
#' @param verbose imprime la URL.
#' @return data.frame (tibble) con los datos, o tibble vacío.
#' @export
consultar_reporte <- function(tipo, reporte,
                              fecha = NULL, from = NULL, to = NULL,
                              reciente = FALSE, codigo = NULL, verbose = FALSE) {
  ti <- .SUGEVAL_TIPO[[tipo]]
  if (is.null(ti)) stop("tipo no válido: ", tipo, call. = FALSE)

  alcance <- if (is.null(codigo)) "todos" else "uno"

  # Determinar variante y parámetros
  if (!is.null(fecha)) {
    variante <- "fecha"
    params <- list(Fecha = list(valor = fecha, tipo = "date"))
  } else if (!is.null(from) && !is.null(to)) {
    variante <- "rango"
    params <- list(FechaInicio = list(valor = from, tipo = "date"),
                   FechaFinal  = list(valor = to,   tipo = "date"))
  } else if (isTRUE(reciente)) {
    variante <- "reciente"
    params <- list()
  } else {
    stop("Indique 'fecha', o 'from' y 'to', o reciente=TRUE.", call. = FALSE)
  }

  # Si es una entidad, anteponer el parámetro de código
  if (alcance == "uno") {
    cod_param <- ti$cod_param
    params <- c(stats::setNames(list(list(valor = codigo, tipo = "string")), cod_param),
                params)
  }

  metodo <- .metodo_reporte(tipo, reporte, variante, alcance)
  parsed <- .sugeval_get(metodo, params, verbose = verbose)
  datos  <- .extraer_datos(parsed)

  # Anotar tipo de entidad y reporte para trazabilidad
  if (nrow(datos) > 0) {
    datos$.tipo_entidad <- tipo
    datos$.reporte      <- reporte
  }
  datos
}
