# sugevalReportes

Cliente en R para los **Servicios Web Externos de SUGEVAL** (Costa Rica),
publicados mediante protocolo **OData**. Análogo a `sugefReportes`, pero
adaptado a SUGEVAL: autenticación por token Bearer, parámetros en formato
OData, e iteración automática por ventanas de 6 meses.

## Instalación

Coloca la carpeta del paquete y instálala localmente:

```r
# install.packages("remotes")
remotes::install_local("sugevalReportes")
# o, en desarrollo, cargar sin instalar:
#   for (f in list.files("sugevalReportes/R", full.names = TRUE)) source(f)
```

Dependencias: `httr`, `jsonlite`, `dplyr`, `tibble`.

## Token de acceso

El token de SUGEVAL (el JWT que recibiste por correo) debe estar en la variable
de entorno `token_sugeval`. Guárdalo en tu archivo `.Renviron`:

```
token_sugeval=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ...firma
```

- Pega el **JWT completo**, con sus dos puntos (`.`) que separan los tres
  bloques (`header.payload.signature`). No lo pongas entre comillas.
- Reinicia la sesión de R tras editar `.Renviron` (o usa
  `readRenviron("~/.Renviron")`).
- **No subas `.Renviron` a GitHub** (agrégalo a `.gitignore`).
- El token vence: SUGEVAL te avisa por correo 30/15/5/3/1 días antes. Si las
  consultas fallan con error 401, regenera el token en el Autenticador.

## Uso

### Listar entidades (catálogos)

```r
library(sugevalReportes)

# Descubrir todos los catálogos disponibles y sus códigos
catalogos <- listar_catalogos_sugeval()

# Listados por tipo de entidad
puestos <- listar_puestos_bolsa(estado = "ACTIVO")   # o INACTIVO, o NULL (todos)
safis   <- listar_safis(estado = "ACTIVO")
fondos  <- listar_fondos()
```

### Estados financieros

Reportes disponibles: `"balance"`, `"cuentas_orden"`,
`"resultados_acumulado"`, `"resultados_mensual"`.
Tipos de entidad: `"puesto"`, `"safi"`, `"fondo"`.

```r
# Balance de los TRES tipos de entidad, info más reciente
bal <- obtener_reporte_sugeval("balance", tipos = c("puesto","safi","fondo"),
                               reciente = TRUE)

# Balance de puestos de bolsa en un rango (se itera en ventanas de 6 meses)
bal_puestos <- obtener_reporte_sugeval("balance", tipos = "puesto",
                                       from = "2023-01-01", to = "2024-12-31")

# Estado de resultados mensual de una SAFI concreta
res_safi <- obtener_reporte_sugeval("resultados_mensual", tipos = "safi",
                                    codigo = "SU0001",
                                    from = "2024-01-01", to = "2024-06-30")
```

Cada fila del resultado trae dos columnas de trazabilidad: `.tipo_entidad`
(puesto/safi/fondo) y `.reporte`.

### Consulta de bajo nivel (una sola llamada)

```r
# Una fecha de corte, todas las entidades de un tipo
consultar_reporte("puesto", "balance", fecha = "2024-06-30")

# Una entidad concreta, rango ≤ 6 meses
consultar_reporte("fondo", "balance", codigo = "F0001",
                  from = "2024-01-01", to = "2024-06-30")
```

## Notas del web service

- **Rango máximo:** 6 meses por consulta de estados financieros. La función de
  alto nivel divide rangos mayores automáticamente.
- **Intervalo entre peticiones:** el servicio rechaza llamadas demasiado
  seguidas. El paquete espera `~1 s` entre peticiones (ajustable en
  `.SUGEVAL_MIN_INTERVALO`). Si ves el error "intervalos entre peticiones",
  súbelo.
- **Histórico:** catálogos contables desde 2004; Resumen Financiero de SAFIs y
  UDEs desde 2008.
- **No compartas el token:** consultas simultáneas con el mismo token pueden
  hacer que SUGEVAL lo bloquee automáticamente.

## Estructura del paquete

```
sugevalReportes/
├── DESCRIPTION
├── NAMESPACE
├── README.md
└── R/
    ├── 01_core_odata.R      # autenticación, formato OData, petición GET
    ├── 02_catalogos.R       # catálogos y listados de entidades
    ├── 03_reportes.R        # métodos de estados financieros por tipo
    └── 04_obtener_reporte.R # orquestación de alto nivel (ventanas 6 meses)
```
