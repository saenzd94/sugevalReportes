# Núcleo de acceso a los Servicios Web Externos de SUGEVAL.

.SUGEVAL_BASE_URL <- paste0(
  "https://serviciosexternos.sugeval.fi.cr/",
  "ServiciosWebExternos/odata"
)

.SUGEVAL_TOKEN_VARS <- c("SUGEVAL_TOKEN", "token_sugeval")
.SUGEVAL_MIN_INTERVALO <- 1
.SUGEVAL_TIMEOUT <- 60

# Marca monotónica de la última petición realizada en esta sesión de R.
.sugeval_ultima_peticion <- local({
  instante <- -Inf
  list(
    get = function() instante,
    set = function(valor) instante <<- valor,
    reset = function() instante <<- -Inf
  )
})

.sugeval_segundos <- function() {
  unname(proc.time()[["elapsed"]])
}

.sugeval_opcion_numerica <- function(nombre, valor_predeterminado,
                                     minimo = 0) {
  valor <- getOption(nombre, valor_predeterminado)
  if (length(valor) != 1L || is.na(valor) || !is.numeric(valor) ||
      !is.finite(valor) || valor < minimo) {
    stop(
      sprintf(
        "La opci\u00f3n '%s' debe ser un n\u00famero finito mayor o igual que %s.",
        nombre, minimo
      ),
      call. = FALSE
    )
  }
  as.numeric(valor)
}

#' Comprobar si hay un token de SUGEVAL configurado
#'
#' Busca primero `SUGEVAL_TOKEN` y, por compatibilidad con versiones previas,
#' también `token_sugeval`. La función no muestra ni devuelve el token.
#'
#' @return Un valor lógico de longitud uno.
#' @export
#' @examples
#' sugeval_token_configurado()
sugeval_token_configurado <- function() {
  any(vapply(
    .SUGEVAL_TOKEN_VARS,
    function(nombre) nzchar(trimws(Sys.getenv(nombre, unset = ""))),
    logical(1)
  ))
}

.sugeval_token <- function() {
  for (nombre in .SUGEVAL_TOKEN_VARS) {
    token <- trimws(Sys.getenv(nombre, unset = ""))
    if (nzchar(token)) {
      # Evita enviar accidentalmente "Bearer Bearer ...".
      token <- sub("^Bearer[[:space:]]+", "", token, ignore.case = TRUE)
      if (nzchar(token)) {
        return(token)
      }
    }
  }

  stop(
    "No se encontr\u00f3 el token de SUGEVAL. Defina SUGEVAL_TOKEN en ",
    "~/.Renviron (tambi\u00e9n se acepta token_sugeval por compatibilidad) y ",
    "reinicie la sesi\u00f3n de R.",
    call. = FALSE
  )
}

.validar_texto_unico <- function(x, nombre, permitir_vacio = FALSE) {
  if (length(x) != 1L || is.na(x) || !is.character(x)) {
    stop(sprintf("'%s' debe ser un texto de longitud uno.", nombre),
         call. = FALSE)
  }
  x <- trimws(x)
  if (!permitir_vacio && !nzchar(x)) {
    stop(sprintf("'%s' no puede estar vac\u00edo.", nombre), call. = FALSE)
  }
  x
}

.validar_fecha <- function(x, nombre) {
  if (length(x) != 1L || is.na(x)) {
    stop(sprintf("'%s' debe contener una sola fecha v\u00e1lida.", nombre),
         call. = FALSE)
  }
  fecha <- tryCatch(
    suppressWarnings(as.Date(x)),
    error = function(error) as.Date(NA)
  )
  if (is.na(fecha)) {
    stop(sprintf("'%s' debe tener formato aaaa-MM-dd o ser de clase Date.",
                 nombre), call. = FALSE)
  }
  fecha
}

#' Formatear un valor según la sintaxis OData de SUGEVAL
#'
#' @param x Valor escalar.
#' @param tipo Uno de `"string"`, `"date"`, `"int"` o `"null"`.
#' @return Texto listo para incorporarse a la llamada OData.
#' @keywords internal
.odata_valor <- function(x, tipo = c("string", "date", "int", "null")) {
  tipo <- match.arg(tipo)

  if (identical(tipo, "null") || is.null(x) ||
      (length(x) == 1L && is.na(x))) {
    return("null")
  }
  if (length(x) != 1L) {
    stop("Los par\u00e1metros OData deben ser escalares.", call. = FALSE)
  }

  switch(
    tipo,
    date = format(.validar_fecha(x, "valor"), "%Y-%m-%d"),
    int = {
      numero <- suppressWarnings(as.numeric(x))
      if (is.na(numero) || !is.finite(numero) || numero != floor(numero)) {
        stop("El valor OData de tipo 'int' debe ser un entero.",
             call. = FALSE)
      }
      format(numero, scientific = FALSE, trim = TRUE)
    },
    string = {
      texto <- .validar_texto_unico(
        as.character(x), "valor", permitir_vacio = TRUE
      )
      # En OData las comillas simples dentro de una cadena se duplican.
      sprintf("'%s'", gsub("'", "''", texto, fixed = TRUE))
    }
  )
}

#' Construir la URL de una función OData
#'
#' @param metodo Nombre de la función publicada por SUGEVAL.
#' @param params Lista nombrada de especificaciones `valor` y `tipo`.
#' @return URL completa.
#' @keywords internal
.odata_url <- function(metodo, params = list()) {
  metodo <- .validar_texto_unico(metodo, "metodo")
  if (!grepl("^[A-Za-z][A-Za-z0-9]*$", metodo)) {
    stop("'metodo' contiene caracteres no permitidos.", call. = FALSE)
  }
  if (!is.list(params)) {
    stop("'params' debe ser una lista.", call. = FALSE)
  }

  base_url <- sub(
    "/+$", "",
    .validar_texto_unico(
      getOption("sugevalReportes.base_url", .SUGEVAL_BASE_URL),
      "sugevalReportes.base_url"
    )
  )

  if (!length(params)) {
    return(sprintf("%s/%s()", base_url, metodo))
  }
  if (is.null(names(params)) || any(!nzchar(names(params))) ||
      anyDuplicated(names(params))) {
    stop("'params' debe tener nombres \u00fanicos y no vac\u00edos.", call. = FALSE)
  }

  partes <- vapply(names(params), function(nombre) {
    parametro <- params[[nombre]]
    if (!is.list(parametro) ||
        !all(c("valor", "tipo") %in% names(parametro))) {
      stop(
        sprintf("El par\u00e1metro '%s' debe definir 'valor' y 'tipo'.", nombre),
        call. = FALSE
      )
    }
    sprintf(
      "%s=%s", nombre,
      .odata_valor(parametro$valor, parametro$tipo)
    )
  }, character(1))

  sprintf("%s/%s(%s)", base_url, metodo, paste(partes, collapse = ","))
}

.sugeval_respetar_intervalo <- function() {
  minimo <- .sugeval_opcion_numerica(
    "sugevalReportes.min_intervalo", .SUGEVAL_MIN_INTERVALO
  )
  transcurrido <- .sugeval_segundos() - .sugeval_ultima_peticion$get()
  if (transcurrido < minimo) {
    Sys.sleep(minimo - transcurrido)
  }
}

.sugeval_detalle <- function(cuerpo, maximo = 500L) {
  texto <- gsub("[\r\n\t]+", " ", cuerpo)
  texto <- gsub("[[:space:]]+", " ", texto)
  texto <- trimws(texto)
  if (!nzchar(texto)) {
    return("sin detalle adicional")
  }
  substr(texto, 1L, maximo)
}

#' Realizar una petición GET al servicio de SUGEVAL
#'
#' @param metodo Nombre de la función OData.
#' @param params Lista de parámetros para [`.odata_url()`].
#' @param verbose Si es `TRUE`, muestra la URL consultada (nunca el token).
#' @return Respuesta JSON convertida a objetos de R.
#' @keywords internal
.sugeval_get <- function(metodo, params = list(), verbose = FALSE) {
  if (length(verbose) != 1L || is.na(verbose) || !is.logical(verbose)) {
    stop("'verbose' debe ser TRUE o FALSE.", call. = FALSE)
  }

  url <- .odata_url(metodo, params)
  token <- .sugeval_token()
  timeout <- .sugeval_opcion_numerica(
    "sugevalReportes.timeout", .SUGEVAL_TIMEOUT, minimo = 1
  )

  .sugeval_respetar_intervalo()
  if (isTRUE(verbose)) {
    message("[SUGEVAL] GET ", url)
  }

  respuesta <- tryCatch(
    httr::GET(
      url,
      httr::add_headers(Authorization = paste("Bearer", token)),
      httr::accept_json(),
      httr::user_agent("sugevalReportes/0.2.0"),
      httr::timeout(timeout)
    ),
    error = function(error) {
      stop(
        sprintf(
          "Error de red al consultar %s: %s",
          metodo, conditionMessage(error)
        ),
        call. = FALSE
      )
    },
    finally = {
      .sugeval_ultima_peticion$set(.sugeval_segundos())
    }
  )

  estado <- httr::status_code(respuesta)
  cuerpo <- httr::content(respuesta, as = "text", encoding = "UTF-8")
  detalle <- .sugeval_detalle(cuerpo)

  if (estado %in% c(401L, 403L)) {
    stop(
      "Autenticaci\u00f3n rechazada (", estado, "). El token puede ser inv\u00e1lido, ",
      "haber vencido o estar bloqueado.",
      call. = FALSE
    )
  }
  if (estado == 404L) {
    stop(
      "SUGEVAL no encontr\u00f3 el m\u00e9todo solicitado (404): ", metodo,
      ". Verifique que el servicio conserve este endpoint.",
      call. = FALSE
    )
  }
  if (estado == 429L) {
    stop(
      "SUGEVAL limit\u00f3 temporalmente las peticiones (429). Aumente la opci\u00f3n ",
      "sugevalReportes.min_intervalo e intente de nuevo.",
      call. = FALSE
    )
  }
  if (estado == 400L) {
    if (grepl("intervalos entre peticiones", cuerpo, ignore.case = TRUE)) {
      stop(
        "SUGEVAL rechaz\u00f3 la consulta por frecuencia excesiva. Aumente la ",
        "opci\u00f3n sugevalReportes.min_intervalo. Detalle: ", detalle,
        call. = FALSE
      )
    }
    stop(
      "Petici\u00f3n inv\u00e1lida (400) en ", metodo,
      ". Revise par\u00e1metros, fechas y el rango m\u00e1ximo de seis meses. Detalle: ",
      detalle,
      call. = FALSE
    )
  }
  if (estado >= 500L) {
    stop(
      "Error interno del servicio (", estado, ") en ", metodo,
      ". Detalle: ", detalle,
      call. = FALSE
    )
  }
  if (estado < 200L || estado >= 300L) {
    stop(
      "Respuesta inesperada (", estado, ") en ", metodo,
      ". Detalle: ", detalle,
      call. = FALSE
    )
  }
  if (estado == 204L || !nzchar(trimws(cuerpo))) {
    return(list())
  }

  if (grepl("reglas de validaci", cuerpo, ignore.case = TRUE)) {
    stop("SUGEVAL rechaz\u00f3 la consulta: ", detalle, call. = FALSE)
  }

  tryCatch(
    jsonlite::fromJSON(
      cuerpo,
      simplifyVector = TRUE,
      simplifyDataFrame = TRUE,
      flatten = FALSE
    ),
    error = function(error) {
      stop(
        "No se pudo interpretar el JSON de ", metodo, ": ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )
}
