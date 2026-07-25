# Componentes visuales reutilizables del reporte navreport.

PALETA_SUGEVAL <- list(
  primario = "#0F1C35",
  acento = "#0D9488",
  azul = "#2563EB",
  oro = "#C9A227",
  morado = "#7C3AED",
  rojo = "#DC2626",
  firebrick = "#B22222",
  promedio = "#475569",
  verde = "#059669",
  serie = c(
    "#0D9488", "#2563EB", "#C9A227", "#7C3AED", "#DC2626",
    "#059669", "#0891B2", "#C2410C", "#4338CA", "#65A30D"
  )
)

TIPOS_ORDEN <- c("puesto", "safi", "fondo")
TIPOS_ETIQUETA <- c(
  puesto = "Puestos de bolsa",
  safi = "SAFI",
  fondo = "Fondos de inversión"
)

categorias_hc <- function(x) {
  unname(as.character(x))
}

.promedio_historico_hc <- function(valores, sufijo = "") {
  valores <- limpiar_num(valores)
  promedio <- if (all(is.na(valores))) NA_real_ else mean(valores, na.rm = TRUE)
  if (is.na(promedio) || !is.finite(promedio)) return(list())
  list(list(
    value = promedio,
    color = PALETA_SUGEVAL$promedio,
    width = 2,
    dashStyle = "ShortDash",
    zIndex = 4,
    label = list(
      text = paste0(
        "Promedio histórico: ", fmt_num(promedio, 2L), sufijo
      ),
      align = "right",
      style = list(
        color = PALETA_SUGEVAL$promedio,
        fontWeight = "600"
      )
    )
  ))
}

.media_movil_hc <- function(x, ventana) {
  x <- limpiar_num(x)
  if (!length(x)) return(x)
  moving_fun(
    x, as.integer(ventana) - 1L,
    function(bloque) {
      if (all(is.na(bloque))) NA_real_ else mean(bloque, na.rm = TRUE)
    }
  )
}

.agregar_medias_moviles_hc <- function(
    hc, fechas, valores, ventanas = c(3L, 6L, 12L),
    sufijo_ventana = "m", sufijo_valor = "") {
  colores <- c(
    PALETA_SUGEVAL$azul,
    PALETA_SUGEVAL$morado,
    PALETA_SUGEVAL$firebrick
  )
  trazos <- c("Dot", "Dash", "LongDashDot")
  for (indice in seq_along(ventanas)) {
    ventana <- as.integer(ventanas[[indice]])
    media <- .media_movil_hc(valores, ventana)
    if (all(is.na(media))) next
    hc <- highcharter::hc_add_series(
      hc,
      name = paste0("Media móvil ", ventana, sufijo_ventana),
      data = serie_stock(fechas, media),
      color = colores[[(indice - 1L) %% length(colores) + 1L]],
      dashStyle = trazos[[(indice - 1L) %% length(trazos) + 1L]],
      lineWidth = if (indice == length(ventanas)) 2.4 else 1.7,
      marker = list(enabled = FALSE),
      tooltip = list(valueSuffix = sufijo_valor, valueDecimals = 2)
    )
  }
  hc
}

hc_stock_limpio <- function(hc, selected = 5L) {
  hc %>%
    highcharter::hc_rangeSelector(
      enabled = TRUE,
      selected = selected,
      inputEnabled = TRUE,
      allButtonsEnabled = TRUE,
      buttons = list(
        list(type = "month", count = 3, text = "3m"),
        list(type = "month", count = 6, text = "6m"),
        list(type = "ytd", text = "YTD"),
        list(type = "year", count = 1, text = "1a"),
        list(type = "year", count = 3, text = "3a"),
        list(type = "all", text = "Todo")
      )
    ) %>%
    highcharter::hc_xAxis(
      type = "datetime", ordinal = TRUE,
      dateTimeLabelFormats = list(
        day = "%e %b", week = "%e %b", month = "%b %Y", year = "%Y"
      )
    ) %>%
    highcharter::hc_navigator(enabled = FALSE) %>%
    highcharter::hc_scrollbar(enabled = FALSE)
}

hc_legend_isolate <- function(hc) {
  evento <- htmlwidgets::JS(
    "function () { var keep=this.index, ss=this.chart.series;",
    "for(var i=0;i<ss.length;i++){if(ss[i].index!==keep){",
    "ss[i].visible?ss[i].hide():ss[i].show();}} return false;}"
  )
  opciones <- hc$x$hc_opts$plotOptions %||% list()
  opciones$series <- opciones$series %||% list()
  opciones$series$events <- opciones$series$events %||% list()
  opciones$series$events$legendItemClick <- evento
  hc$x$hc_opts$plotOptions <- opciones
  hc
}

hc_vacio <- function(titulo, subtitulo = "Sin datos disponibles para esta vista") {
  highcharter::highchart() %>%
    highcharter::hc_title(text = titulo) %>%
    highcharter::hc_subtitle(text = subtitulo) %>%
    highcharter::hc_legend(enabled = FALSE)
}

hc_evolucion_tipos <- function(panel_tipo, variable, titulo,
                               factor = 1000, sufijo = " M") {
  datos <- panel_tipo %>%
    dplyr::filter(!is.na(.data[[variable]]), TIPO %in% TIPOS_ORDEN)
  if (!nrow(datos)) return(hc_vacio(titulo))
  univariado <- dplyr::n_distinct(datos$TIPO) == 1L
  serie_unica <- datos %>% dplyr::arrange(FECHA)
  valores_unicos <- serie_unica[[variable]] / factor

  hc <- highcharter::highchart(type = "stock") %>%
    hc_stock_limpio() %>%
    highcharter::hc_chart(type = "line") %>%
    highcharter::hc_title(text = titulo) %>%
    highcharter::hc_subtitle(text = if (univariado) {
      "Serie observada, promedio histórico y medias móviles de 3, 6 y 12 meses"
    } else {
      NULL
    }) %>%
    highcharter::hc_yAxis(
      title = list(text = "Millones de colones"),
      plotLines = if (univariado) {
        .promedio_historico_hc(valores_unicos, sufijo)
      } else {
        list()
      }
    )
  for (indice in seq_along(TIPOS_ORDEN)) {
    tipo <- TIPOS_ORDEN[[indice]]
    parte <- datos %>% dplyr::filter(TIPO == tipo) %>% dplyr::arrange(FECHA)
    if (!nrow(parte)) next
    hc <- highcharter::hc_add_series(
      hc,
      name = TIPOS_ETIQUETA[[tipo]],
      data = serie_stock(parte$FECHA, parte[[variable]] / factor),
      color = PALETA_SUGEVAL$serie[[indice]],
      tooltip = list(valueSuffix = sufijo, valueDecimals = 1)
    )
  }
  if (univariado) {
    hc <- .agregar_medias_moviles_hc(
      hc, serie_unica$FECHA, valores_unicos,
      ventanas = c(3L, 6L, 12L), sufijo_ventana = "m",
      sufijo_valor = sufijo
    )
  }
  hc %>%
    highcharter::hc_tooltip(shared = TRUE) %>%
    hc_legend_isolate()
}

hc_participacion_tipo <- function(panel_actual, tipo, titulo = NULL) {
  datos <- panel_actual %>%
    dplyr::filter(TIPO == tipo, !is.na(PARTICIPACION), Activo > 0) %>%
    dplyr::arrange(dplyr::desc(PARTICIPACION))
  titulo <- titulo %||% paste("Participación por activo —", TIPOS_ETIQUETA[[tipo]])
  if (!nrow(datos)) return(hc_vacio(titulo))

  highcharter::highchart() %>%
    highcharter::hc_chart(type = "pie") %>%
    highcharter::hc_title(text = titulo) %>%
    highcharter::hc_add_series(
      name = "Participación",
      data = lapply(seq_len(nrow(datos)), function(indice) {
        list(
          name = nombre_corto(datos$NOMBRE[[indice]], 46L),
          y = round(100 * datos$PARTICIPACION[[indice]], 2)
        )
      })
    ) %>%
    highcharter::hc_colors(PALETA_SUGEVAL$serie) %>%
    highcharter::hc_tooltip(valueSuffix = "%", valueDecimals = 2)
}

hc_ranking_tipo <- function(panel_actual, tipo, n = 15L,
                            factor = 1000, titulo = NULL) {
  datos <- panel_actual %>%
    dplyr::filter(TIPO == tipo, !is.na(Activo), Activo > 0) %>%
    dplyr::arrange(dplyr::desc(Activo)) %>%
    dplyr::slice_head(n = n) %>%
    dplyr::arrange(Activo)
  titulo <- titulo %||% paste("Principales entidades —", TIPOS_ETIQUETA[[tipo]])
  if (!nrow(datos)) return(hc_vacio(titulo))

  highcharter::highchart() %>%
    highcharter::hc_chart(type = "bar") %>%
    highcharter::hc_title(text = titulo) %>%
    highcharter::hc_xAxis(
      categories = categorias_hc(nombre_corto(datos$NOMBRE, 52L))
    ) %>%
    highcharter::hc_yAxis(title = list(text = "Millones de colones")) %>%
    highcharter::hc_add_series(
      name = "Activo",
      data = round(datos$Activo / factor, 1),
      colorByPoint = TRUE
    ) %>%
    highcharter::hc_colors(PALETA_SUGEVAL$serie) %>%
    highcharter::hc_legend(enabled = FALSE) %>%
    highcharter::hc_tooltip(valueSuffix = " M", valueDecimals = 1)
}

hc_estructura_actual <- function(panel_actual, factor = 1000) {
  datos <- panel_actual %>%
    dplyr::group_by(TIPO) %>%
    dplyr::summarise(
      Pasivo = suma_segura(Pasivo),
      Patrimonio = suma_segura(Patrimonio),
      .groups = "drop"
    ) %>%
    dplyr::mutate(ETIQUETA = unname(TIPOS_ETIQUETA[TIPO])) %>%
    dplyr::arrange(factor(TIPO, levels = TIPOS_ORDEN))
  if (!nrow(datos)) return(hc_vacio("Estructura financiera por tipo"))

  highcharter::highchart() %>%
    highcharter::hc_chart(type = "column") %>%
    highcharter::hc_title(text = "Estructura financiera por tipo") %>%
    highcharter::hc_xAxis(categories = categorias_hc(datos$ETIQUETA)) %>%
    highcharter::hc_yAxis(title = list(text = "Millones de colones")) %>%
    highcharter::hc_plotOptions(column = list(stacking = "normal")) %>%
    highcharter::hc_add_series(
      name = "Pasivo", data = round(datos$Pasivo / factor, 1),
      color = PALETA_SUGEVAL$azul
    ) %>%
    highcharter::hc_add_series(
      name = "Patrimonio", data = round(datos$Patrimonio / factor, 1),
      color = PALETA_SUGEVAL$acento
    ) %>%
    highcharter::hc_tooltip(shared = TRUE, valueSuffix = " M")
}

hc_ratio_tipos <- function(panel_tipo, variable, titulo) {
  datos <- panel_tipo %>% dplyr::filter(!is.na(.data[[variable]]))
  if (!nrow(datos)) return(hc_vacio(titulo))
  univariado <- dplyr::n_distinct(datos$TIPO) == 1L
  serie_unica <- datos %>% dplyr::arrange(FECHA)
  valores_unicos <- 100 * serie_unica[[variable]]
  hc <- highcharter::highchart(type = "stock") %>%
    hc_stock_limpio() %>%
    highcharter::hc_chart(type = "line") %>%
    highcharter::hc_title(text = titulo) %>%
    highcharter::hc_subtitle(text = if (univariado) {
      "Serie observada, promedio histórico y medias móviles de 3, 6 y 12 meses"
    } else {
      NULL
    }) %>%
    highcharter::hc_yAxis(
      title = list(text = "%"),
      plotLines = if (univariado) {
        .promedio_historico_hc(valores_unicos, "%")
      } else {
        list()
      }
    )
  for (indice in seq_along(TIPOS_ORDEN)) {
    tipo <- TIPOS_ORDEN[[indice]]
    parte <- datos %>% dplyr::filter(TIPO == tipo) %>% dplyr::arrange(FECHA)
    if (!nrow(parte)) next
    hc <- highcharter::hc_add_series(
      hc,
      name = TIPOS_ETIQUETA[[tipo]],
      data = serie_stock(parte$FECHA, 100 * parte[[variable]]),
      color = PALETA_SUGEVAL$serie[[indice]],
      tooltip = list(valueSuffix = "%", valueDecimals = 2)
    )
  }
  if (univariado) {
    hc <- .agregar_medias_moviles_hc(
      hc, serie_unica$FECHA, valores_unicos,
      ventanas = c(3L, 6L, 12L), sufijo_ventana = "m",
      sufijo_valor = "%"
    )
  }
  hc %>% highcharter::hc_tooltip(shared = TRUE) %>% hc_legend_isolate()
}

hc_concentracion <- function(concentracion) {
  datos <- concentracion %>%
    dplyr::mutate(ETIQUETA = unname(TIPOS_ETIQUETA[TIPO])) %>%
    dplyr::arrange(factor(TIPO, levels = TIPOS_ORDEN))
  if (!nrow(datos)) return(hc_vacio("Concentración por tipo de entidad"))
  highcharter::highchart() %>%
    highcharter::hc_chart(type = "bar") %>%
    highcharter::hc_title(text = "Índice Herfindahl-Hirschman por activo") %>%
    highcharter::hc_xAxis(categories = categorias_hc(datos$ETIQUETA)) %>%
    highcharter::hc_yAxis(title = list(text = "HHI (0–10.000)"), max = 10000) %>%
    highcharter::hc_add_series(
      name = "HHI", data = round(datos$HHI, 0), colorByPoint = TRUE
    ) %>%
    highcharter::hc_colors(PALETA_SUGEVAL$serie) %>%
    highcharter::hc_legend(enabled = FALSE)
}

hc_entidad <- function(panel_entidad, id, variable, titulo,
                       factor = 1000, porcentaje = FALSE) {
  datos <- panel_entidad %>%
    dplyr::filter(ID == id, !is.na(.data[[variable]])) %>%
    dplyr::arrange(FECHA)
  if (!nrow(datos)) return(hc_vacio(titulo))
  valores <- datos[[variable]]
  eje <- "Millones de colones"
  sufijo <- " M"
  if (isTRUE(porcentaje)) {
    valores <- 100 * valores
    factor <- 1
    eje <- "%"
    sufijo <- "%"
  }
  valores_grafico <- valores / factor
  grafico <- highcharter::highchart(type = "stock") %>%
    hc_stock_limpio() %>%
    highcharter::hc_chart(type = "line") %>%
    highcharter::hc_title(text = titulo) %>%
    highcharter::hc_subtitle(
      text = "Serie observada, promedio histórico y medias móviles de 3, 6 y 12 meses"
    ) %>%
    highcharter::hc_yAxis(
      title = list(text = eje),
      plotLines = .promedio_historico_hc(valores_grafico, sufijo)
    ) %>%
    highcharter::hc_add_series(
      name = "Serie observada",
      data = serie_stock(datos$FECHA, valores_grafico),
      color = PALETA_SUGEVAL$acento,
      tooltip = list(valueSuffix = sufijo, valueDecimals = 2)
    )
  grafico <- .agregar_medias_moviles_hc(
    grafico, datos$FECHA, valores_grafico,
    ventanas = c(3L, 6L, 12L), sufijo_ventana = "m",
    sufijo_valor = sufijo
  )
  grafico %>%
    highcharter::hc_tooltip(shared = TRUE) %>%
    hc_legend_isolate()
}

.valores_validos_tabla <- function(x) {
  x <- trimws(as.character(x))
  x[
    !is.na(x) & nzchar(x) &
      !toupper(x) %in% c("N/D", "NA", "N.A.", "-", "—")
  ]
}

.es_columna_fecha_tabla <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return(TRUE)
  if (!is.character(x) && !is.factor(x)) return(FALSE)
  valores <- .valores_validos_tabla(x)
  length(valores) > 0L &&
    mean(grepl("^\\d{2}/\\d{2}/\\d{4}$", valores)) >= 0.8
}

.es_columna_numerica_tabla <- function(x) {
  if (is.numeric(x) || is.integer(x)) return(TRUE)
  if (!is.character(x) && !is.factor(x)) return(FALSE)
  valores <- .valores_validos_tabla(x)
  if (!length(valores)) return(FALSE)
  patron <- paste0(
    "^[[:space:]]*[+\\-]?(₡|\\$|€)?[[:space:]]*",
    "[0-9][0-9\\.[:space:]]*(,[0-9]+)?[[:space:]]*",
    "(%|M|miles|\\(miles\\))?[[:space:]]*$"
  )
  mean(grepl(patron, valores, ignore.case = TRUE)) >= 0.8
}

.render_orden_numerico_tabla <- htmlwidgets::JS(
  "function(data, type) {",
  "  if (type !== 'sort' && type !== 'type') return data;",
  "  if (data === null || data === undefined) return null;",
  "  if (typeof data === 'number') return data;",
  "  var s = String(data).replace(/<[^>]*>/g, '').replace(/\\u00a0/g, ' ').trim();",
  "  if (!s || /^(N\\/D|NA|N\\.A\\.|-|—)$/i.test(s)) return null;",
  "  var negativo = /^\\s*\\(.*\\)\\s*$/.test(s);",
  "  s = s.replace(/\\s/g, '').replace(/[^0-9,.+\\-]/g, '');",
  "  if (s.indexOf(',') >= 0) {",
  "    s = s.replace(/\\./g, '').replace(',', '.');",
  "  } else if (/^[+\\-]?\\d{1,3}(\\.\\d{3})+$/.test(s)) {",
  "    s = s.replace(/\\./g, '');",
  "  }",
  "  var numero = Number(s.replace('+', ''));",
  "  if (!isFinite(numero)) return null;",
  "  return negativo ? -numero : numero;",
  "}"
)

.render_orden_fecha_tabla <- htmlwidgets::JS(
  "function(data, type) {",
  "  if (type !== 'sort' && type !== 'type') return data;",
  "  if (data === null || data === undefined) return null;",
  "  var s = String(data).replace(/<[^>]*>/g, '').trim();",
  "  var m = s.match(/^(\\d{2})\\/(\\d{2})\\/(\\d{4})$/);",
  "  if (!m) return null;",
  "  return Date.UTC(Number(m[3]), Number(m[2]) - 1, Number(m[1]));",
  "}"
)

.diferir_widget_navreport <- function(widget, tipo = "datatables") {
  etiquetas <- htmltools::as.tags(widget)
  es_script_widget <- function(etiqueta) {
    inherits(etiqueta, "shiny.tag") &&
      identical(etiqueta$name, "script") &&
      identical(etiqueta$attribs$type, "application/json") &&
      !is.null(etiqueta$attribs[["data-for"]])
  }
  scripts <- which(vapply(etiquetas, es_script_widget, logical(1)))
  if (!length(scripts)) {
    stop("No se encontró el script de inicialización del widget.")
  }
  for (indice in scripts) {
    etiquetas[[indice]]$attribs$type <- "text/nr-deferred"
    etiquetas[[indice]]$attribs[["data-type"]] <- tipo
  }
  etiquetas
}

inicializador_dt_navreport <- function() {
  htmltools::tagList(
    htmltools::tags$style(htmltools::HTML(paste(
      ".dataTables_wrapper .dataTables_scrollHead table.dataTable,",
      ".dataTables_wrapper .dataTables_scrollBody > table.dataTable {",
      "  margin-left: 0 !important;",
      "  margin-right: 0 !important;",
      "}",
      sep = "\n"
    ))),
    htmltools::tags$script(htmltools::HTML(paste(
      "(function () {",
      "  function aplicarParcheDT() {",
      "    if (!window.NR || window.NR.__sugevalDTPatch) return !!window.NR;",
      "    var original = window.NR.initWidgetsIn;",
      "    window.NR.initWidgetsIn = function (container) {",
      "      if (container) {",
      "        var scripts = container.querySelectorAll(",
      "          'script[type=\"text/nr-deferred\"][data-type=\"datatables\"]'",
      "        );",
      "        scripts.forEach(function (script) {",
      "          var id = script.getAttribute('data-for');",
      "          var widget = id ? document.getElementById(id) : null;",
      "          if (widget) widget.classList.remove('html-widget-static-bound');",
      "        });",
      "      }",
      "      return original.call(window.NR, container);",
      "    };",
      "    window.NR.__sugevalDTPatch = true;",
      "    return true;",
      "  }",
      "  if (!aplicarParcheDT()) {",
      "    document.addEventListener('DOMContentLoaded', aplicarParcheDT, {once:true});",
      "  }",
      "})();",
      sep = "\n"
    )))
  )
}

tabla_html <- function(datos, ..., filas_pagina = 15L) {
  datos <- as.data.frame(datos, stringsAsFactors = FALSE)
  if (!requireNamespace("DT", quietly = TRUE)) {
    return(htmltools::HTML(
      knitr::kable(
        datos, format = "html", row.names = FALSE, escape = TRUE, ...
      )
    ))
  }

  fechas <- unname(which(vapply(
    datos, .es_columna_fecha_tabla, logical(1)
  )))
  numericas <- unname(setdiff(
    which(vapply(datos, .es_columna_numerica_tabla, logical(1))),
    fechas
  ))
  definiciones <- unname(c(
    lapply(numericas, function(indice) {
      list(
        targets = indice - 1L,
        type = "num",
        render = .render_orden_numerico_tabla
      )
    }),
    lapply(fechas, function(indice) {
      list(
        targets = indice - 1L,
        type = "num",
        render = .render_orden_fecha_tabla
      )
    })
  ))

  widget <- DT::datatable(
    datos,
    rownames = FALSE,
    filter = "top",
    escape = TRUE,
    selection = "none",
    class = "stripe hover compact",
    width = "100%",
    options = list(
      pageLength = as.integer(filas_pagina),
      lengthMenu = c(10, 15, 25, 50, 100),
      ordering = TRUE,
      order = list(),
      scrollX = TRUE,
      autoWidth = TRUE,
      searchHighlight = TRUE,
      columnDefs = definiciones,
      language = list(
        search = "Buscar:",
        lengthMenu = "Mostrar _MENU_ filas",
        info = "Filas _START_ a _END_ de _TOTAL_",
        infoEmpty = "Sin filas disponibles",
        zeroRecords = "No se encontraron coincidencias",
        paginate = list(
          first = "Primera", last = "Última",
          `next` = "Siguiente", previous = "Anterior"
        )
      )
    )
  )
  .diferir_widget_navreport(widget)
}
