#' sugevalReportes: cliente para los Servicios Web Externos de SUGEVAL
#'
#' El paquete ofrece funciones para consultar catálogos, listar puestos de
#' bolsa, SAFI y fondos de inversión, y descargar estados financieros mediante
#' los métodos OData publicados por SUGEVAL.
#'
#' @section Configuración:
#' Defina el token en la variable de entorno `SUGEVAL_TOKEN`. Por
#' compatibilidad también se reconoce `token_sugeval`.
#'
#' @section Opciones:
#' - `sugevalReportes.min_intervalo`: segundos mínimos entre peticiones (1 por
#'   defecto).
#' - `sugevalReportes.timeout`: tiempo máximo de una petición en segundos (60
#'   por defecto).
#'
#' @keywords internal
"_PACKAGE"
