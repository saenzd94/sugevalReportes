# sugevalReportes 0.4.0

* Se añadió `consultar_informacion_diaria_fondos()` para consultar una fecha
  de corte o un rango de hasta seis meses con los métodos oficiales de
  información diaria de fondos de SUGEVAL.
* La respuesta conserva las notas oficiales y añade
  `ValorParticipacionCalculado`, identificado expresamente como el cociente
  entre activo neto y número de valores de participación.
* Se añadió `exportar_informacion_diaria_fondos()` para crear archivos CSV
  UTF-8 y XLSX a partir de la tabla consultada.

# sugevalReportes 0.3.0

* Se añadió `listar_emisiones_vigentes()` para consultar las emisiones locales
  vigentes inscritas en el RNVI.
* Se añadió `exportar_emisiones_vigentes()` para crear archivos CSV UTF-8 y
  XLSX, con protección frente a fórmulas de hoja de cálculo.
* Se conservaron el encabezado y la fecha de consulta oficiales como atributos
  de la tabla de emisiones.

# sugevalReportes 0.2.0

* Se corrigieron los métodos OData sin parámetros para incluir `()`.
* Se corrigió el endpoint `ObtenerListadoTodosFondosDeSafi` y su parámetro
  obligatorio `CodigoRegulado=''` al listar todos los fondos.
* Las consultas de un fondo ahora usan `CodigoRegulado`, como establece el
  manual técnico oficial de SUGEVAL.
* Se añadieron validaciones de fechas, modos de consulta, estados y rangos.
* Se añadió soporte preferente para `SUGEVAL_TOKEN`, manteniendo
  `token_sugeval` por compatibilidad.
* Se incorporaron documentación roxygen2, pruebas automatizadas, licencia,
  archivos de desarrollo y un flujo local de R CMD check para GitHub Actions.
