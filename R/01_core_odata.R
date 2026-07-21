# ══════════════════════════════════════════════════════════════════════════════
#  01_core_odata.R  —  Núcleo de acceso al Web Service de SUGEVAL (OData)
#
#  SUGEVAL publica sus datos mediante un RESTful API con protocolo OData (Open
#  Data Protocol). A diferencia de SUGEF (endpoints REST tradicionales), aquí
#  los parámetros van DENTRO de la URL, entre paréntesis, con una sintaxis
#  particular según el tipo de dato:
#     • Date   → sin comillas:   Fecha=2024-06-30
#     • String → comilla simple: CodigoRegulado='G0001'
#     • Int    → sin comillas:   CodigoCatalogo=0
#     • null   → literal null:   Estado=null
#
#  Autenticación (según la Guía General SWE): el token va en el encabezado
#  Authorization con el valor "Bearer <token>". El token se lee de la variable
#  de entorno token_sugeval (guárdalo en .Renviron, nunca en el código).
#
#  URL base: https://serviciosexternos.sugeval.fi.cr/ServiciosWebExternos/odata/
# ══════════════════════════════════════════════════════════════════════════════

.SUGEVAL_BASE_URL <- "https://serviciosexternos.sugeval.fi.cr/ServiciosWebExternos/odata"

# Tiempo mínimo entre peticiones (segundos). El servicio rechaza peticiones
# demasiado seguidas ("Los intervalos entre peticiones superan el mínimo de
# tiempo permitido"). El valor exacto no se publica; se usa un margen prudente
# de 1 segundo por defecto, ajustable. Si recibes ese error, súbelo.
.SUGEVAL_MIN_INTERVALO <- 1.0

# Marca de tiempo de la última petición (para respetar el intervalo mínimo).
.sugeval_ultima_peticion <- local({
  t <- 0
  list(get = function() t, set = function(v) t <<- v)
})

#' Obtener el token de SUGEVAL desde el entorno
#'
#' Lee la variable de entorno token_sugeval. Debe estar en .Renviron como:
#'   token_sugeval=eyJhbGciOi...   (el JWT completo, con sus dos puntos)
#' @keywords internal
.sugeval_token <- function() {
  tok <- Sys.getenv("token_sugeval")
  if (!nzchar(tok)) {
    stop("No se encontró el token. Define token_sugeval en .Renviron ",
         "(o con Sys.setenv(token_sugeval='...')) y reinicia la sesión de R.",
         call. = FALSE)
  }
  tok
}

#' Formatea un valor para una URL OData según su tipo
#'
#' @param x valor a formatear.
#' @param tipo "date", "string", "int" o "null".
#' @keywords internal
.odata_valor <- function(x, tipo = c("string", "date", "int", "null")) {
  tipo <- match.arg(tipo)
  if (tipo == "null" || is.null(x) || (length(x) == 1 && is.na(x))) return("null")
  switch(tipo,
    date   = format(as.Date(x), "%Y-%m-%d"),   # sin comillas
    int    = as.character(as.integer(x)),      # sin comillas
    string = sprintf("'%s'", x)                # comilla simple
  )
}

#' Construye la URL completa de un método OData con sus parámetros
#'
#' @param metodo nombre del método (ej. "ObtenerBalanceGeneralTodosPuestosPorFechaCorte").
#' @param params lista nombrada; cada elemento es list(valor=, tipo=).
#'   Ej: list(Fecha = list(valor = "2024-06-30", tipo = "date")).
#' @keywords internal
.odata_url <- function(metodo, params = list()) {
  if (length(params) == 0) {
    return(sprintf("%s/%s", .SUGEVAL_BASE_URL, metodo))
  }
  partes <- vapply(names(params), function(nm) {
    p <- params[[nm]]
    sprintf("%s=%s", nm, .odata_valor(p$valor, p$tipo))
  }, character(1))
  sprintf("%s/%s(%s)", .SUGEVAL_BASE_URL, metodo, paste(partes, collapse = ","))
}

#' Realiza una petición GET al Web Service de SUGEVAL
#'
#' Respeta el intervalo mínimo entre peticiones, adjunta el token en el
#' encabezado Authorization: Bearer, y parsea el JSON de respuesta.
#'
#' @param metodo nombre del método OData.
#' @param params lista de parámetros (ver .odata_url).
#' @param verbose imprime la URL consultada.
#' @return lista parseada del JSON (o error informativo).
#' @keywords internal
.sugeval_get <- function(metodo, params = list(), verbose = FALSE) {
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Se requieren los paquetes 'httr' y 'jsonlite'.", call. = FALSE)
  }

  # Respetar el intervalo mínimo entre peticiones.
  ahora   <- as.numeric(Sys.time())
  transc  <- ahora - .sugeval_ultima_peticion$get()
  if (transc < .SUGEVAL_MIN_INTERVALO) {
    Sys.sleep(.SUGEVAL_MIN_INTERVALO - transc)
  }

  url <- .odata_url(metodo, params)
  # Resolver el token ANTES del tryCatch de red, para que la ausencia de token
  # produzca su propio mensaje claro y no quede envuelta como "error de red".
  token <- .sugeval_token()
  if (isTRUE(verbose)) message("[SUGEVAL] GET ", url)

  resp <- tryCatch(
    httr::GET(
      url,
      httr::add_headers(Authorization = paste("Bearer", token)),
      httr::accept_json()
    ),
    error = function(e) {
      stop(sprintf("Error de red al consultar %s: %s", metodo, conditionMessage(e)),
           call. = FALSE)
    }
  )
  .sugeval_ultima_peticion$set(as.numeric(Sys.time()))

  status <- httr::status_code(resp)
  cuerpo <- httr::content(resp, as = "text", encoding = "UTF-8")

  if (status == 401 || status == 403) {
    stop("Autenticación rechazada (", status, "). El token puede ser inválido, ",
         "haber vencido o estar bloqueado. Revisa token_sugeval y su vigencia.",
         call. = FALSE)
  }
  if (status == 400) {
    stop("Petición inválida (400) en ", metodo, ". Revisa el formato de los ",
         "parámetros (fechas aaaa-MM-dd, rango ≤ 6 meses). Detalle: ",
         substr(cuerpo, 1, 300), call. = FALSE)
  }
  if (status >= 500) {
    stop("Error interno del servicio (", status, ") en ", metodo, ". Detalle: ",
         substr(cuerpo, 1, 300), call. = FALSE)
  }
  if (status != 200) {
    stop("Respuesta inesperada (", status, ") en ", metodo, ": ",
         substr(cuerpo, 1, 300), call. = FALSE)
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(cuerpo, simplifyVector = TRUE, simplifyDataFrame = TRUE),
    error = function(e) {
      stop("No se pudo parsear el JSON de ", metodo, ": ", conditionMessage(e),
           call. = FALSE)
    }
  )

  # El mensaje de regla de validación viene con status 200 pero contiene el
  # texto de excepción; detectarlo para avisar claramente.
  if (is.character(parsed) && any(grepl("reglas de validaci", parsed, ignore.case = TRUE))) {
    stop("SUGEVAL rechazó la consulta: ", paste(parsed, collapse = " "), call. = FALSE)
  }

  parsed
}
