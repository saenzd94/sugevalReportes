# sugevalReportes

Cliente en R para consultar los
[Servicios Web Externos de SUGEVAL](https://www.sugeval.fi.cr/serviciosytramites/servicios-web)
mediante OData. El paquete cubre catálogos, puestos de bolsa, sociedades
administradoras de fondos de inversión (SAFI), fondos de inversión y cuatro
estados financieros:

* balance general;
* cuentas de orden;
* estado de resultados acumulado;
* estado de resultados mensual.

El cliente administra el encabezado de autenticación, valida los parámetros,
respeta una pausa mínima entre peticiones y divide rangos extensos en ventanas
de hasta seis meses.

> Este proyecto no es un producto oficial de SUGEVAL. Los métodos y límites
> proceden del
> [manual técnico oficial](https://www.sugeval.fi.cr/serviciosytramites/servicioweb/Manual%20Tecnico%20SWE.pdf).

## Requisitos

* R 4.1 o posterior.
* Acceso HTTPS a `serviciosexternos.sugeval.fi.cr`.
* Un token vigente emitido por SUGEVAL.

Las dependencias de R se instalan automáticamente.

## Instalación

### Desde GitHub

```r
install.packages("remotes")
remotes::install_github("saenzd94/sugevalReportes", dependencies = TRUE)
```

### Desde una carpeta o ZIP descargado

Si descargó y descomprimió el repositorio:

```r
install.packages("remotes")
remotes::install_local(
  "C:/ruta/a/sugevalReportes",
  dependencies = TRUE,
  upgrade = "never"
)
```

En desarrollo también puede usar:

```r
devtools::load_all("C:/ruta/a/sugevalReportes")
```

No se recomienda cargar los archivos de `R/` con `source()`: instalar o cargar
el paquete garantiza que el espacio de nombres y las dependencias se manejen
correctamente.

## Configurar el token

Guarde el token en `~/.Renviron` con el nombre `SUGEVAL_TOKEN`. Desde R puede
abrir ese archivo con:

```r
file.edit("~/.Renviron")
```

Agregue una línea como esta, sin comillas y sin escribir la palabra `Bearer`:

```text
SUGEVAL_TOKEN=pegue_aqui_el_token_completo
```

Reinicie R y compruebe la configuración sin revelar el valor:

```r
library(sugevalReportes)
sugeval_token_configurado()
```

Por compatibilidad también se reconoce la variable antigua `token_sugeval`.
No guarde `.Renviron` dentro del repositorio ni comparta el token. El archivo
`.gitignore` incluido ayuda a evitar una publicación accidental.

## Uso

### Catálogos y entidades

```r
library(sugevalReportes)

# Descubrir códigos de catálogo.
catalogos <- listar_catalogos_sugeval()

# Puestos y SAFI.
puestos <- listar_puestos_bolsa(estado = "ACTIVO")
safis <- listar_safis(estado = "ACTIVO")

# Todos los fondos.
fondos <- listar_fondos()

# Fondos de una SAFI y, opcionalmente, por estado.
fondos_safi <- listar_fondos(codigo_safi = "CODIGO_SAFI")
fondos_activos <- listar_fondos(
  codigo_safi = "CODIGO_SAFI",
  estado = "ACTIVO"
)
```

### Estados financieros

Los valores válidos de `reporte` son `"balance"`, `"cuentas_orden"`,
`"resultados_acumulado"` y `"resultados_mensual"`. Los tipos de entidad son
`"puesto"`, `"safi"` y `"fondo"`.

```r
# Último balance disponible de los tres tipos de entidad.
balance_reciente <- obtener_reporte_sugeval(
  "balance",
  tipos = c("puesto", "safi", "fondo"),
  reciente = TRUE
)

# Una fecha de corte.
balance_fecha <- obtener_reporte_sugeval(
  "balance",
  tipos = "puesto",
  fecha = "2024-06-30"
)

# Un rango amplio; se divide automáticamente en ventanas de seis meses.
resultados <- obtener_reporte_sugeval(
  "resultados_mensual",
  tipos = "safi",
  from = "2023-01-01",
  to = "2024-12-31"
)

# Un fondo concreto. El código proviene de listar_fondos().
balance_fondo <- obtener_reporte_sugeval(
  "balance",
  tipos = "fondo",
  codigo = "CODIGO_FONDO",
  reciente = TRUE
)
```

Las respuestas no vacías incluyen `.tipo_entidad` y `.reporte` para conservar
la trazabilidad después de consolidar resultados.

### Una sola petición

`consultar_reporte()` es la interfaz de bajo nivel. Un rango no puede superar
seis meses:

```r
una_consulta <- consultar_reporte(
  tipo = "puesto",
  reporte = "balance",
  from = "2024-01-01",
  to = "2024-06-30"
)
```

Debe elegirse exactamente un modo: `fecha`, `from` junto con `to`, o
`reciente = TRUE`.

## Control de peticiones y errores

La pausa predeterminada es de un segundo y el tiempo máximo de una petición es
de 60 segundos. Puede ajustarlos para la sesión actual:

```r
options(
  sugevalReportes.min_intervalo = 1.5,
  sugevalReportes.timeout = 90
)
```

SUGEVAL no publica el valor exacto del intervalo mínimo y puede modificarlo.
Si el servicio rechaza peticiones muy seguidas, aumente
`sugevalReportes.min_intervalo`.

Por defecto, `obtener_reporte_sugeval()` se detiene ante un error. Para obtener
resultados parciales en una descarga extensa:

```r
resultado <- obtener_reporte_sugeval(
  "balance",
  reciente = TRUE,
  intentar_continuar = TRUE
)

attr(resultado, "errores_sugeval")
```

Si todas las peticiones fallan, la función siempre produce un error en lugar
de devolver silenciosamente una tabla vacía.

## Desarrollo y comprobación

```r
devtools::document()
devtools::test()
devtools::check()
```

Las pruebas automatizadas no consumen el servicio ni necesitan token. La
prueba manual instalada sí consulta SUGEVAL:

```r
source(system.file(
  "examples", "prueba_sugeval.R",
  package = "sugevalReportes"
))
```

## Seguridad y límites del servicio

* No comparta el token ni haga consultas simultáneas con el mismo token.
* El histórico contable indicado por SUGEVAL comienza en 2004.
* El rango máximo de una petición contable es de seis meses móviles.
* Los datos pueden cambiar por reenvíos de las entidades reguladas.
* Un error 401 o 403 suele indicar que el token venció, fue rechazado o quedó
  bloqueado.

## Licencia

MIT © 2026 Diego Sáenz C.
