# ==============================================================================
# scripts/cran_stats.R
#
# Recupera descargas CRAN de tus paquetes y genera una tabla markdown que se
# inserta automáticamente en README.md, entre los marcadores:
#   <!-- CRAN-STATS:START -->  ...  <!-- CRAN-STATS:END -->
#
# Basado en tu script original (cran_stats_paulesantos.R), adaptado para
# correr sin interacción dentro de un GitHub Action.
# ==============================================================================

if (!requireNamespace("cranlogs", quietly = TRUE)) install.packages("cranlogs")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

library(cranlogs)
library(dplyr)

# ------------------------------------------------------------------------
# 1) Tus paquetes publicados
# ------------------------------------------------------------------------
mis_paquetes <- c(
  "avesperu",
  "fuzzystring",
  "geoperu",
  "iucnr",
  "mtsta",
  "peruflorads43",
  "perumammals",
  "perutimber",
  "ppendemic",
  "redbookperu",
  "reptiledb.data",
  "rmdd",
  "tidyttmoment",
  "wcvpmatch"
)

# ------------------------------------------------------------------------
# 2) Filtra los que realmente están en CRAN ahora mismo
# ------------------------------------------------------------------------
check_on_cran <- function(pkgs) {
  cran_db <- tools::CRAN_package_db()
  en_cran <- pkgs[pkgs %in% cran_db$Package]
  fuera_cran <- setdiff(pkgs, en_cran)
  if (length(fuera_cran) > 0) {
    message("No están en CRAN (r-universe / GitHub only): ",
            paste(fuera_cran, collapse = ", "))
  }
  en_cran
}

paquetes_cran <- check_on_cran(mis_paquetes)

# ------------------------------------------------------------------------
# 3) Descargas: mes, semana y total histórico
# ------------------------------------------------------------------------
obtener_descargas <- function(pkgs) {
  if (length(pkgs) == 0) return(NULL)

  descargas_mes <- cran_downloads(packages = pkgs, when = "last-month") |>
    group_by(package) |>
    summarise(descargas_ultimo_mes = sum(count), .groups = "drop")

  descargas_semana <- cran_downloads(packages = pkgs, when = "last-week") |>
    group_by(package) |>
    summarise(descargas_ultima_semana = sum(count), .groups = "drop")

  descargas_total <- cran_downloads(packages = pkgs, from = "2012-10-01",
                                     to = Sys.Date() - 1) |>
    group_by(package) |>
    summarise(descargas_totales = sum(count), .groups = "drop")

  descargas_total |>
    left_join(descargas_mes, by = "package") |>
    left_join(descargas_semana, by = "package") |>
    arrange(desc(descargas_totales))
}

resumen <- obtener_descargas(paquetes_cran)

if (is.null(resumen) || nrow(resumen) == 0) {
  stop("No se obtuvieron datos de descargas. Revisa la lista de paquetes.")
}

# ------------------------------------------------------------------------
# 4) Construir la tabla en markdown
# ------------------------------------------------------------------------
fmt <- function(x) format(x, big.mark = ",")

encabezado <- c(
  "| Paquete | Descargas totales | Último mes | Última semana |",
  "|---|---:|---:|---:|"
)

filas <- vapply(seq_len(nrow(resumen)), function(i) {
  pkg <- resumen$package[i]
  sprintf(
    "| [%s](https://cran.r-project.org/package=%s) | %s | %s | %s |",
    pkg, pkg,
    fmt(resumen$descargas_totales[i]),
    fmt(resumen$descargas_ultimo_mes[i]),
    fmt(resumen$descargas_ultima_semana[i])
  )
}, character(1))

total_mes       <- sum(resumen$descargas_ultimo_mes, na.rm = TRUE)
total_historico <- sum(resumen$descargas_totales, na.rm = TRUE)

bloque <- c(
  "<!-- CRAN-STATS:START -->",
  sprintf(
    "**📦 Descargas CRAN** · Total histórico: **%s** · Último mes: **%s** · _Actualizado: %s_",
    fmt(total_historico), fmt(total_mes), Sys.Date()
  ),
  "",
  encabezado,
  filas,
  "<!-- CRAN-STATS:END -->"
)

# ------------------------------------------------------------------------
# 5) Insertar el bloque en README.md, reemplazando lo que haya entre marcadores
# ------------------------------------------------------------------------
readme_path <- "README.md"
readme <- readLines(readme_path, warn = FALSE)

start_idx <- grep("<!-- CRAN-STATS:START -->", readme)
end_idx   <- grep("<!-- CRAN-STATS:END -->", readme)

if (length(start_idx) == 1 && length(end_idx) == 1 && start_idx < end_idx) {
  nuevo_readme <- c(
    readme[seq_len(start_idx - 1)],
    bloque,
    readme[seq(end_idx + 1, length(readme))]
  )
} else {
  # Si el README aún no tiene los marcadores, se agregan al final
  nuevo_readme <- c(readme, "", bloque)
}

writeLines(nuevo_readme, readme_path)
message("README.md actualizado correctamente.")
