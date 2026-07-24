# Catálogos y listados de entidades de SUGEVAL.

.colectar_data_frames <- function(x, ruta = "raiz") {
  if (is.data.frame(x)) {
    return(stats::setNames(list(x), ruta))
  }
  if (!is.list(x) || !length(x)) {
    return(list())
  }

  salida <- list()
  nombres <- names(x)
  for (indice in seq_along(x)) {
    nombre <- if (!is.null(nombres) && nzchar(nombres[[indice]])) {
      nombres[[indice]]
    } else {
      as.character(indice)
    }
    salida <- c(
      salida,
      .colectar_data_frames(x[[indice]], paste(ruta, nombre, sep = "$"))
    )
  }
  salida
}

#' Extraer la tabla principal de una respuesta de SUGEVAL
#'
#' @param parsed Objeto ya interpretado desde JSON.
#' @param preferidos Nombres de campos que se deben priorizar.
#' @return Un tibble, posiblemente vacío.
#' @keywords internal
.extraer_datos <- function(
    parsed,
    preferidos = c(
      "Catalogos", "Fondos", "InformacionDiariaFondos",
      "BalanceGeneral", "CuentasOrden",
      "CuentaOrden", "EstadosResultados", "EstadoResultadoAcumulado",
      "EstadoResultadoMensual", "value"
    )) {
  if (is.data.frame(parsed)) {
    return(tibble::as_tibble(parsed))
  }
  if (!is.list(parsed) || !length(parsed)) {
    return(tibble::tibble())
  }

  for (nombre in preferidos) {
    if (!is.null(parsed[[nombre]]) && is.data.frame(parsed[[nombre]])) {
      return(tibble::as_tibble(parsed[[nombre]]))
    }
  }

  tablas <- .colectar_data_frames(parsed)
  if (!length(tablas)) {
    return(tibble::tibble())
  }

  filas <- vapply(tablas, nrow, integer(1))
  tibble::as_tibble(tablas[[which.max(filas)]])
}

.normalizar_estado_entidad <- function(estado) {
  if (is.null(estado)) {
    return(NULL)
  }
  estado <- toupper(.validar_texto_unico(estado, "estado"))
  match.arg(estado, c("ACTIVO", "INACTIVO"))
}

.normalizar_estado_fondo <- function(estado) {
  estado <- .normalizar_estado_entidad(estado)
  if (identical(estado, "ACTIVO")) "Activo" else "Inactivo"
}

#' Listar los catálogos disponibles en SUGEVAL
#'
#' Consulta el catálogo cero, que describe los códigos y nombres de los demás
#' catálogos publicados por el servicio.
#'
#' @param verbose Si es `TRUE`, muestra la URL consultada.
#' @return Un tibble con los catálogos disponibles.
#' @export
#' @examples
#' \dontrun{
#' listar_catalogos_sugeval()
#' }
listar_catalogos_sugeval <- function(verbose = FALSE) {
  respuesta <- .sugeval_get(
    "ObtenerListadoCatalogoPorCodigo",
    list(
      CodigoCatalogo = list(valor = 0, tipo = "int"),
      Estado = list(valor = NA, tipo = "null")
    ),
    verbose = verbose
  )
  .extraer_datos(respuesta, "Catalogos")
}

#' Obtener un catálogo específico de SUGEVAL
#'
#' @param codigo Código entero del catálogo. Consulte
#'   [listar_catalogos_sugeval()] para descubrir los disponibles.
#' @param estado Filtro opcional de estado. Si es `NULL`, se envía el literal
#'   OData `null`.
#' @param verbose Si es `TRUE`, muestra la URL consultada.
#' @return Un tibble con los elementos del catálogo.
#' @export
#' @examples
#' \dontrun{
#' obtener_catalogo_sugeval(3)
#' obtener_catalogo_sugeval(4, estado = "ACTIVO")
#' }
obtener_catalogo_sugeval <- function(codigo, estado = NULL, verbose = FALSE) {
  # Valida antes de realizar la petición y conserva el valor entero.
  codigo_formateado <- .odata_valor(codigo, "int")
  codigo <- as.numeric(codigo_formateado)

  if (!is.null(estado)) {
    estado <- .validar_texto_unico(estado, "estado")
  }
  parametros <- list(
    CodigoCatalogo = list(valor = codigo, tipo = "int"),
    Estado = if (is.null(estado)) {
      list(valor = NA, tipo = "null")
    } else {
      list(valor = estado, tipo = "string")
    }
  )

  respuesta <- .sugeval_get(
    "ObtenerListadoCatalogoPorCodigo", parametros, verbose = verbose
  )
  .extraer_datos(respuesta, "Catalogos")
}

#' Listar puestos de bolsa
#'
#' @param estado `"ACTIVO"`, `"INACTIVO"` o `NULL` para no filtrar.
#' @param verbose Si es `TRUE`, muestra la URL consultada.
#' @return Un tibble con los puestos de bolsa.
#' @export
#' @examples
#' \dontrun{
#' listar_puestos_bolsa("ACTIVO")
#' }
listar_puestos_bolsa <- function(estado = NULL, verbose = FALSE) {
  estado <- .normalizar_estado_entidad(estado)
  codigo <- if (is.null(estado)) 3 else 4
  obtener_catalogo_sugeval(codigo, estado = estado, verbose = verbose)
}

#' Listar sociedades administradoras de fondos de inversión
#'
#' @param estado `"ACTIVO"`, `"INACTIVO"` o `NULL` para no filtrar.
#' @param verbose Si es `TRUE`, muestra la URL consultada.
#' @return Un tibble con las SAFI.
#' @export
#' @examples
#' \dontrun{
#' listar_safis("ACTIVO")
#' }
listar_safis <- function(estado = NULL, verbose = FALSE) {
  estado <- .normalizar_estado_entidad(estado)
  codigo <- if (is.null(estado)) 5 else 6
  obtener_catalogo_sugeval(codigo, estado = estado, verbose = verbose)
}

#' Listar fondos de inversión
#'
#' @param codigo_safi Código opcional de una SAFI, obtenido del catálogo 5.
#'   Si se omite, se consultan los fondos de todas las SAFI.
#' @param estado Filtro opcional `"ACTIVO"` o `"INACTIVO"`. SUGEVAL exige
#'   `codigo_safi` cuando se usa este filtro.
#' @param verbose Si es `TRUE`, muestra la URL consultada.
#' @return Un tibble con los fondos de inversión.
#' @export
#' @examples
#' \dontrun{
#' listar_fondos()
#' listar_fondos(codigo_safi = "S0001")
#' listar_fondos(codigo_safi = "S0001", estado = "ACTIVO")
#' }
listar_fondos <- function(codigo_safi = NULL, estado = NULL,
                          verbose = FALSE) {
  if (is.null(codigo_safi)) {
    codigo_safi <- ""
  } else {
    codigo_safi <- .validar_texto_unico(codigo_safi, "codigo_safi")
  }

  if (!is.null(estado) && !nzchar(codigo_safi)) {
    stop("SUGEVAL exige 'codigo_safi' cuando se filtra por 'estado'.",
         call. = FALSE)
  }

  parametros <- list(
    CodigoRegulado = list(valor = codigo_safi, tipo = "string")
  )
  metodo <- "ObtenerListadoTodosFondosDeSafi"

  if (!is.null(estado)) {
    metodo <- "ObtenerListadoTodosFondosDeSafiPorEstado"
    parametros$Estado <- list(
      valor = .normalizar_estado_fondo(estado), tipo = "string"
    )
  }

  respuesta <- .sugeval_get(metodo, parametros, verbose = verbose)
  .extraer_datos(respuesta, "Fondos")
}
