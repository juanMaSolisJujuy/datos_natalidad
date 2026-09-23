
#' Metadata de una fuente de datos
#'
#' @param nombre     Nombre descriptivo del dataset/planilla
#' @param organismo  Organismo/fuente
#' @param cobertura  Cobertura temporal y/o geográfica
#' @param variables  Variables clave, separadas por coma
#' @param enlace     URL de descarga/consulta pública
#' @param archivo    
fuente_card <- function(nombre, organismo, cobertura, variables,
                        enlace = NA, archivo = NA) {
  filas <- c(
    glue::glue("Organismo: {organismo}"),
    glue::glue("Cobertura: {cobertura}"),
    glue::glue("Variables: {variables}")
  )
  if (!is.na(enlace)) {
    filas <- c(filas, glue::glue("Sitio de descarga: ({enlace})"))
  }
  if (!is.na(archivo)) {
    filas <- c(filas, glue::glue("Archivo local: `{archivo}`"))
  }
  cat(glue::glue("{nombre}\n\n"), paste(filas, collapse = "  \n"), "\n")
}

#' Lista, como enlaces de descarga en Markdown, los archivos de una carpeta
#'
#' Pensada para fuentes cuyos "datos" son archivos completos (PDF, XLSX, XLS,
#' CSV) y no una planilla a importar/transformar. Se usa dentro de un chunk
#' con `#| results: asis`.
#'
#' @param dir        Carpeta a escanear (ruta relativa al proyecto)
#' @param patron     Regex de extensiones a incluir
#' @param recursive  Si TRUE, también lista archivos en subcarpetas
listar_enlaces <- function(dir, patron = "\\.(pdf|xlsx|xls|csv)$", recursive = FALSE) {
  archivos <- sort(list.files(dir, pattern = patron, full.names = FALSE,
                              recursive = recursive, ignore.case = TRUE))
  if (length(archivos) == 0) {
    cat(glue::glue("_No se encontraron archivos en `{dir}`._"), "\n")
  } else {
    enlaces <- glue::glue("- [{archivos}]({file.path(dir, archivos)})")
    cat(paste(enlaces, collapse = "\n"), "\n")
  }
}

#' Envoltorio estándar para las tablas interactivas del sitio
tabla_interactiva <- function(df) {
  DT::datatable(
    df,
    options = list(scrollX = TRUE, pageLength = 5, dom = "ftip"),
    rownames = FALSE,
    class = "compact stripe hover"
  )
}