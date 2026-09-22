# ============================================================================
# Headless demo run — no Shiny, no credentials, no network.
# Reads fixtures/demo_statement.xlsx, runs the full pipeline, writes the
# consolidated workbook and prints a summary.
#
#   Rscript scripts/run_demo.R
# ============================================================================

source("R/pipeline.R")

ENTRADA <- "fixtures/demo_statement.xlsx"
SALIDA  <- "out/consolidated.xlsx"

if (!file.exists(ENTRADA)) {
  stop("Fixture not found. Run: python fixtures/generate_demo_statement.py")
}
dir.create("out", showWarnings = FALSE)

cat("Reading", ENTRADA, "\n")
hojas <- macro1_crear_tablas(ENTRADA)
cat("  sheets recognised as accounts:", paste(names(hojas), collapse = ", "), "\n")

hojas <- macro2_agregar_empresa(hojas, ENTRADA)
df    <- macro3_consolidar(hojas)
cat("  rows consolidated:", nrow(df), "| columns:", ncol(df), "\n")

cat("\nCompanies parsed from cell A1 of each sheet:\n")
for (e in unique(df$EMPRESA)) cat("  -", e, "\n")

df <- macro4_clasificar(df)

cat("\nClassification (", nrow(df), "rows ):\n", sep = "")
tabla <- sort(table(df$DESCRIPCION_GENERAL[df$DESCRIPCION_GENERAL != ""]),
              decreasing = TRUE)
for (i in seq_along(tabla)) {
  cat(sprintf("  %-22s %5d\n", names(tabla)[i], tabla[i]))
}
sin <- sum(df$DESCRIPCION_GENERAL == "")
cat(sprintf("  %-22s %5d  (%.1f%%)\n", "[unclassified]", sin,
            100 * sin / nrow(df)))

openxlsx::write.xlsx(df, SALIDA, overwrite = TRUE)
cat("\nWrote", SALIDA, "\n")

# ---------------------------------------------------------------------------
# Second module: the daily cash summary (the other half of the original macro)
# ---------------------------------------------------------------------------
cat("\nRunning daily summary module...\n")
res <- suppressMessages(ejecutar_resumen_por_dia(ENTRADA))
cat("  accounts covered:", res$cuentas_total, "\n")
cat("  days in period  :", length(res$dias), "\n")
cat("  summary rows    :", nrow(res$resumen), "\n")
if (!is.null(res$aviso_saldo)) cat("  warning         :", res$aviso_saldo, "\n")

invisible(file.copy(res$archivo, "out/daily_summary.xlsx", overwrite = TRUE))
cat("Wrote out/daily_summary.xlsx\n")
