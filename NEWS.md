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
