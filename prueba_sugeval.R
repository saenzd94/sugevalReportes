# ══════════════════════════════════════════════════════════════════════════════
#  prueba_sugeval.R  —  Prueba de humo del cliente SUGEVAL con token real
#
#  Ejecútalo con tu token ya configurado en .Renviron (token_sugeval). Verifica
#  los tres tipos de entidad (Puestos de Bolsa, SAFIs, Fondos) para detectar
#  cruces o inconvenientes en la extracción, como pediste.
#
#  Uso:
#    setwd("ruta/donde/está/sugevalReportes")
#    source("prueba_sugeval.R")
# ══════════════════════════════════════════════════════════════════════════════

suppressMessages({
  library(httr); library(jsonlite); library(dplyr); library(tibble)
})

# Cargar el paquete (sin instalar) o vía library() si ya lo instalaste
if (!exists("obtener_reporte_sugeval")) {
  for (f in sort(list.files("R", full.names = TRUE))) source(f)
}

sep <- function(t) cat("\n", strrep("=", 70), "\n", t, "\n", strrep("=", 70), "\n", sep = "")

# ── 0. Verificar token ────────────────────────────────────────────────────────
if (!nzchar(Sys.getenv("token_sugeval"))) {
  stop("Configura token_sugeval en .Renviron antes de correr esta prueba.")
}
cat("Token detectado (primeros 15 chars):",
    substr(Sys.getenv("token_sugeval"), 1, 15), "...\n")

# ── 1. Catálogos ──────────────────────────────────────────────────────────────
sep("1. CATÁLOGOS")
catalogos <- tryCatch(listar_catalogos_sugeval(verbose = TRUE),
                      error = function(e) { cat("✗", conditionMessage(e), "\n"); NULL })
if (!is.null(catalogos)) {
  cat("✓ Catálogos disponibles:", nrow(catalogos), "\n")
  print(utils::head(catalogos, 20))
}

# ── 2. Listados por tipo de entidad ───────────────────────────────────────────
sep("2. LISTADOS DE ENTIDADES POR TIPO")
listar_seguro <- function(nombre, fn) {
  cat("\n→", nombre, "\n")
  r <- tryCatch(fn(), error = function(e) { cat("  ✗", conditionMessage(e), "\n"); NULL })
  if (!is.null(r)) {
    cat("  ✓", nrow(r), "registros | columnas:", paste(names(r), collapse = ", "), "\n")
    print(utils::head(r, 5))
  }
  r
}
puestos <- listar_seguro("Puestos de Bolsa (ACTIVO)", function() listar_puestos_bolsa("ACTIVO"))
safis   <- listar_seguro("SAFIs (ACTIVO)",            function() listar_safis("ACTIVO"))
fondos  <- listar_seguro("Fondos de Inversión",       function() listar_fondos())

# ── 3. Balance reciente de los tres tipos ─────────────────────────────────────
sep("3. BALANCE GENERAL RECIENTE (3 TIPOS)")
probar_reporte_tipo <- function(tipo) {
  cat("\n→ Balance", tipo, "\n")
  r <- tryCatch(
    obtener_reporte_sugeval("balance", tipos = tipo, reciente = TRUE, verbose = TRUE),
    error = function(e) { cat("  ✗", conditionMessage(e), "\n"); NULL }
  )
  if (!is.null(r) && nrow(r) > 0) {
    cat("  ✓", nrow(r), "filas |", dplyr::n_distinct(r[[1]]), "valores en 1ª col\n")
    cat("  Columnas:", paste(names(r), collapse = ", "), "\n")
  } else {
    cat("  ⚠ sin datos\n")
  }
  r
}
bal_p <- probar_reporte_tipo("puesto")
bal_s <- probar_reporte_tipo("safi")
bal_f <- probar_reporte_tipo("fondo")

# ── 4. Los tres juntos (verificar que no hay cruces) ──────────────────────────
sep("4. LOS TRES TIPOS JUNTOS")
bal_todos <- tryCatch(
  obtener_reporte_sugeval("balance", tipos = c("puesto","safi","fondo"), reciente = TRUE),
  error = function(e) { cat("✗", conditionMessage(e), "\n"); NULL }
)
if (!is.null(bal_todos) && nrow(bal_todos) > 0) {
  cat("✓ Consolidado:", nrow(bal_todos), "filas\n")
  cat("Distribución por tipo de entidad:\n")
  print(table(bal_todos$.tipo_entidad))
  cat("\nColumnas del consolidado:\n")
  print(names(bal_todos))
}

sep("FIN DE LA PRUEBA")
cat("Comparte esta salida (sin el token) para afinar el parser según la\n")
cat("estructura real de columnas que devuelva cada tipo de entidad.\n")
