# ══════════════════════════════════════════════════════════════════════════════
#  04_obtener_reporte.R  —  Punto de entrada de alto nivel
#
#  obtener_reporte_sugeval() es la función principal del paquete. Orquesta:
#     • la iteración por ventanas de ≤ 6 meses (límite del web service para
#       estados financieros) cuando el rango solicitado es mayor;
#     • la consulta a uno o varios tipos de entidad (puesto/safi/fondo);
#     • la consolidación de resultados en un único data.frame.
#
#  Equivalente conceptual a obtener_reporte_sugef() del paquete de SUGEF, pero
#  adaptado al protocolo OData y a los tres tipos de entidad de SUGEVAL.
# ══════════════════════════════════════════════════════════════════════════════

#' Divide un rango de fechas en ventanas de a lo sumo `meses` meses
#' @keywords internal
.ventanas_fechas <- function(from, to, meses = 6) {
  from <- as.Date(from); to <- as.Date(to)
  if (from > to) stop("'from' es posterior a 'to'.", call. = FALSE)
  ventanas <- list()
  ini <- from
  while (ini <= to) {
    fin <- seq(ini, by = paste(meses, "months"), length.out = 2)[2] - 1
    if (fin > to) fin <- to
    ventanas[[length(ventanas) + 1]] <- c(ini, fin)
    ini <- fin + 1
  }
  ventanas
}

#' Obtener un estado financiero de SUGEVAL (alto nivel)
#'
#' @param reporte "balance", "cuentas_orden", "resultados_acumulado" o
#'   "resultados_mensual".
#' @param tipos vector con uno o varios de "puesto", "safi", "fondo".
#'   Por defecto los tres.
#' @param from,to rango de fechas ("aaaa-MM-dd"). Se itera automáticamente en
#'   ventanas de 6 meses. Si se omiten y reciente=TRUE, trae la info reciente.
#' @param reciente si TRUE y sin from/to, usa la información más reciente.
#' @param codigo opcional; código de una entidad concreta (según el tipo). Si
#'   se indica, `tipos` debe ser de longitud 1.
#' @param intentar_continuar si TRUE (por defecto), un error en un tipo/ventana
#'   no aborta todo: se registra y se continúa con el resto.
#' @param verbose imprime URLs y progreso.
#' @return data.frame consolidado (tibble) con columnas .tipo_entidad y .reporte.
#' @export
obtener_reporte_sugeval <- function(reporte,
                                    tipos = c("puesto", "safi", "fondo"),
                                    from = NULL, to = NULL,
                                    reciente = FALSE, codigo = NULL,
                                    intentar_continuar = TRUE, verbose = FALSE) {
  if (!reporte %in% names(.SUGEVAL_REPORTE)) {
    stop("reporte no válido: ", reporte, ". Opciones: ",
         paste(names(.SUGEVAL_REPORTE), collapse = ", "), call. = FALSE)
  }
  if (!is.null(codigo) && length(tipos) != 1) {
    stop("Si indica 'codigo', 'tipos' debe tener un único tipo.", call. = FALSE)
  }
  tipos <- match.arg(tipos, choices = c("puesto", "safi", "fondo"), several.ok = TRUE)

  # Definir las llamadas (por tipo × ventana)
  usar_rango <- !is.null(from) && !is.null(to)
  ventanas   <- if (usar_rango) .ventanas_fechas(from, to) else list(NULL)

  resultados <- list()
  errores    <- character(0)

  for (tp in tipos) {
    for (vt in ventanas) {
      etiqueta <- sprintf("%s/%s%s", reporte, tp,
                          if (is.null(vt)) "" else sprintf(" [%s a %s]", vt[1], vt[2]))
      datos <- tryCatch({
        if (usar_rango) {
          consultar_reporte(tp, reporte, from = vt[1], to = vt[2],
                            codigo = codigo, verbose = verbose)
        } else if (isTRUE(reciente)) {
          consultar_reporte(tp, reporte, reciente = TRUE,
                            codigo = codigo, verbose = verbose)
        } else {
          stop("Indique from/to o reciente=TRUE.", call. = FALSE)
        }
      }, error = function(e) {
        msg <- sprintf("[%s] %s", etiqueta, conditionMessage(e))
        if (isTRUE(intentar_continuar)) { warning(msg, call. = FALSE); errores[[length(errores)+1]] <<- msg; NULL }
        else stop(msg, call. = FALSE)
      })
      if (!is.null(datos) && nrow(datos) > 0) {
        resultados[[length(resultados) + 1]] <- datos
      }
    }
  }

  if (length(resultados) == 0) {
    if (length(errores) > 0) {
      warning("No se obtuvieron datos. Errores:\n", paste(errores, collapse = "\n"), call. = FALSE)
    }
    return(tibble::tibble())
  }

  # Consolidar (bind_rows es tolerante a columnas distintas entre tipos)
  out <- dplyr::bind_rows(resultados)
  if (length(errores) > 0 && isTRUE(verbose)) {
    message(sprintf("Completado con %d advertencia(s).", length(errores)))
  }
  out
}
