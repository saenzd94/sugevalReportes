# ══════════════════════════════════════════════════════════════════════════════
#  02_catalogos.R  —  Catálogos de SUGEVAL (listados de entidades y códigos)
#
#  El método ObtenerListadoCatalogoPorCodigo(CodigoCatalogo=N, Estado=...)
#  devuelve distintos catálogos según N. Con N=0 y Estado=null se obtiene el
#  listado de TODOS los catálogos disponibles (para descubrir qué códigos hay).
#
#  Catálogos con filtro por estado (de la Guía General):
#     4  — Listado Puestos Bolsa Por Estado   (ACTIVO / INACTIVO)
#     6  — Listado SAFIs por Estado           (ACTIVO / INACTIVO)
#     10 — Listado de Emisores Por Estado     (INSCRITO / DESINSCRITO)
#  (Los mismos catálogos suelen tener una versión sin "Por Estado" que lista
#   todo ignorando el parámetro estado.)
# ══════════════════════════════════════════════════════════════════════════════

#' Extrae el data.frame de resultados de la respuesta de un catálogo
#'
#' La respuesta trae un Título, un Encabezado (Disclaimer, FechaDeConsulta) y
#' el cuerpo con los datos. Esta función localiza el data.frame de datos con
#' robustez ante variaciones de nombres.
#' @keywords internal
.extraer_datos <- function(parsed) {
  if (is.data.frame(parsed)) return(tibble::as_tibble(parsed))
  if (is.list(parsed)) {
    # Buscar el primer elemento que sea data.frame con filas
    dfs <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, parsed)
    if (length(dfs) > 0) return(tibble::as_tibble(dfs[[1]]))
    # A veces el cuerpo está en un campo tipo "value" (convención OData)
    if (!is.null(parsed$value) && is.data.frame(parsed$value)) {
      return(tibble::as_tibble(parsed$value))
    }
  }
  tibble::tibble()
}

#' Listar los catálogos disponibles (para descubrir códigos)
#'
#' @param verbose imprime la URL.
#' @return data.frame con Codigo y Nombre de cada catálogo.
#' @export
listar_catalogos_sugeval <- function(verbose = FALSE) {
  parsed <- .sugeval_get(
    "ObtenerListadoCatalogoPorCodigo",
    list(CodigoCatalogo = list(valor = 0, tipo = "int"),
         Estado         = list(valor = NA, tipo = "null")),
    verbose = verbose
  )
  .extraer_datos(parsed)
}

#' Obtener un catálogo específico por código
#'
#' @param codigo código del catálogo (ver listar_catalogos_sugeval()).
#' @param estado opcional; para catálogos "Por Estado" (ej. "ACTIVO"). Si el
#'   catálogo no filtra por estado, se ignora. Si es NULL se envía null.
#' @param verbose imprime la URL.
#' @export
obtener_catalogo_sugeval <- function(codigo, estado = NULL, verbose = FALSE) {
  params <- list(CodigoCatalogo = list(valor = codigo, tipo = "int"))
  params$Estado <- if (is.null(estado)) list(valor = NA, tipo = "null")
                   else list(valor = estado, tipo = "string")
  parsed <- .sugeval_get("ObtenerListadoCatalogoPorCodigo", params, verbose = verbose)
  .extraer_datos(parsed)
}

#' Listar Puestos de Bolsa
#' @param estado "ACTIVO", "INACTIVO" o NULL (todos).
#' @export
listar_puestos_bolsa <- function(estado = NULL, verbose = FALSE) {
  # Catálogo 3 (todos) o 4 (por estado). Si se pide estado, usar 4.
  cod <- if (is.null(estado)) 3 else 4
  obtener_catalogo_sugeval(cod, estado = estado, verbose = verbose)
}

#' Listar SAFIs (sociedades administradoras de fondos)
#' @param estado "ACTIVO", "INACTIVO" o NULL (todos).
#' @export
listar_safis <- function(estado = NULL, verbose = FALSE) {
  # Catálogo 5 (todos) o 6 (por estado).
  cod <- if (is.null(estado)) 5 else 6
  obtener_catalogo_sugeval(cod, estado = estado, verbose = verbose)
}

#' Listar Fondos de Inversión de todas las SAFIs
#'
#' Usa el método dedicado ObtenerListadoTodosFondosDeSafis (no el catálogo
#' genérico), que devuelve el listado de fondos con su código de fondo y la
#' SAFI administradora.
#' @export
listar_fondos <- function(verbose = FALSE) {
  parsed <- .sugeval_get("ObtenerListadoTodosFondosDeSafis", list(), verbose = verbose)
  .extraer_datos(parsed)
}
