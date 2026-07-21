# Prueba manual contra el servicio real de SUGEVAL.
#
# Después de instalar el paquete:
# source(system.file("examples", "prueba_sugeval.R",
#                    package = "sugevalReportes"))

library(sugevalReportes)

separador <- function(titulo) {
  cat("\n", strrep("=", 70), "\n", titulo, "\n",
      strrep("=", 70), "\n", sep = "")
}

if (!sugeval_token_configurado()) {
  stop("Configure SUGEVAL_TOKEN en ~/.Renviron antes de ejecutar la prueba.")
}
cat("Token configurado: sí (su valor no se mostrará).\n")

separador("1. CATÁLOGOS")
catalogos <- listar_catalogos_sugeval(verbose = TRUE)
cat("Catálogos disponibles:", nrow(catalogos), "\n")
print(utils::head(catalogos, 20))

separador("2. ENTIDADES")
puestos <- listar_puestos_bolsa("ACTIVO")
safis <- listar_safis("ACTIVO")
fondos <- listar_fondos()
cat("Puestos activos:", nrow(puestos), "\n")
cat("SAFI activas:", nrow(safis), "\n")
cat("Fondos:", nrow(fondos), "\n")

separador("3. BALANCE RECIENTE")
balance <- obtener_reporte_sugeval(
  "balance",
  tipos = c("puesto", "safi", "fondo"),
  reciente = TRUE,
  verbose = TRUE
)
cat("Filas:", nrow(balance), "\n")
if (nrow(balance)) {
  print(table(balance$.tipo_entidad))
  print(names(balance))
}

separador("PRUEBA COMPLETADA")
