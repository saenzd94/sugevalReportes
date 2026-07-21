# Contribuir a sugevalReportes

## Preparar el entorno

Abra `sugevalReportes.Rproj` y ejecute:

```r
install.packages(c("devtools", "testthat", "roxygen2", "rcmdcheck"))
devtools::document()
devtools::test()
devtools::check()
```

Las pruebas automatizadas no usan la red ni requieren un token. Las pruebas
manuales contra SUGEVAL sí requieren `SUGEVAL_TOKEN`; nunca incluya el token en
el código, capturas, incidencias o commits.

## Antes de proponer cambios

1. Verifique los métodos y parámetros en el manual técnico oficial vigente.
2. Añada o actualice las pruebas correspondientes.
3. Ejecute `devtools::document()`, `devtools::test()` y `devtools::check()`.
4. Confirme que `.Renviron`, archivos con tokens y salidas descargadas no estén
   incluidos en Git.
