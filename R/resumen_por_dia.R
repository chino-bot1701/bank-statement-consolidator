library(readxl)
library(openxlsx)
library(dplyr)

# ----------------------------------------------------------------------------
# HELPER: Normalizar texto eliminando acentos
# ----------------------------------------------------------------------------
normalizar_texto <- function(x) {
  x <- as.character(x)
  Encoding(x) <- "UTF-8"
  x <- toupper(trimws(x))
  x <- gsub("\u00c1|\u00e1", "A", x)
  x <- gsub("\u00c9|\u00e9", "E", x)
  x <- gsub("\u00cd|\u00ed", "I", x)
  x <- gsub("\u00d3|\u00f3", "O", x)
  x <- gsub("\u00da|\u00fa", "U", x)
  x <- gsub("\u00dc|\u00fc", "U", x)
  x <- gsub("\u00d1|\u00f1", "N", x)
  x
}

# ----------------------------------------------------------------------------
# UTILIDADES
# ----------------------------------------------------------------------------

nz_num <- function(valor) {
  # Maneja guiones "-" que Banco del Istmo usa como celda vacia en depositos/retiros
  if (is.null(valor) || length(valor) == 0) return(0)
  v <- suppressWarnings(as.numeric(valor))
  if (is.na(v)) return(0)
  v
}

# [FIX-1] limpiar_num_cuenta:
# Ahora elimina ceros iniciales (ej: "02801101" -> "2801101") antes de convertir.
limpiar_num_cuenta <- function(valor) {
  if (is.null(valor) || length(valor) == 0) return("")
  v_chr <- trimws(as.character(valor))
  if (is.na(v_chr) || v_chr == "" || v_chr == "NA") return("")
  v_chr <- sub("^0+([1-9])", "\\1", v_chr)
  v_num <- suppressWarnings(as.numeric(v_chr))
  if (!is.na(v_num)) {
    return(trimws(format(v_num, scientific = FALSE, big.mark = "", trim = TRUE, nsmall = 0)))
  }
  v_chr
}

# FIX FECHAS: Convertir fecha robustamente desde lo que readxl entregue.
limpiar_fecha <- function(valor) {
  if (is.null(valor) || length(valor) == 0) return(NA)
  if (is.na(valor))             return(NA)
  
  fecha_valida <- function(f) {
    if (is.na(f)) return(NA)
    yr <- as.integer(format(f, "%Y"))
    if (yr >= 2000 && yr <= 2035) f else NA
  }
  
  if (inherits(valor, "Date"))    return(fecha_valida(valor))
  if (inherits(valor, "POSIXct")) return(fecha_valida(as.Date(valor)))
  
  v_chr <- trimws(as.character(valor))
  if (nchar(v_chr) >= 10) {
    fecha <- tryCatch(as.Date(substr(v_chr, 1, 10)), error = function(e) NA)
    if (!is.na(fecha)) return(fecha_valida(fecha))
  }
  v_num <- suppressWarnings(as.numeric(v_chr))
  if (!is.na(v_num) && v_num > 40000) {
    fecha <- tryCatch(as.Date(v_num, origin = "1899-12-30"), error = function(e) NA)
    return(fecha_valida(fecha))
  }
  NA
}

limpiar_empresa_nombre <- function(nombre) {
  nombre <- as.character(nombre)
  Encoding(nombre) <- "UTF-8"
  if (is.na(nombre) || nchar(trimws(nombre)) == 0) return(nombre)
  if (nchar(nombre) > 5) {
    resultado <- substr(nombre, 6, nchar(nombre))
    pos <- regexpr(",", resultado, fixed = TRUE)
    if (pos > 0) resultado <- substr(resultado, 1, pos - 1)
    return(trimws(resultado))
  }
  nombre
}

# ----------------------------------------------------------------------------
# DETECCION DE FILA DE ENCABEZADOS
# ----------------------------------------------------------------------------
detectar_fila_inicio_rd <- function(archivo, hoja) {
  encabezados_esperados <- c("CUENTA", "FECHA DE OPERACION", "FECHA", "REFERENCIA", "DESCRIPCION")
  preview <- tryCatch(
    read_excel(archivo, sheet = hoja, range = cell_rows(1:25), col_names = FALSE),
    error = function(e) NULL
  )
  if (is.null(preview)) return(14)
  for (i in seq_len(nrow(preview))) {
    fila_norm <- normalizar_texto(paste(as.character(unlist(preview[i, ])), collapse = " "))
    coincidencias <- sum(sapply(encabezados_esperados, function(e) grepl(e, fila_norm, fixed = TRUE)))
    if (coincidencias >= 2) return(i)
  }
  return(14)
}

# ----------------------------------------------------------------------------
# EXTRACCION DE CUENTA Y EMPRESA
# ----------------------------------------------------------------------------
extraer_cuenta_hoja <- function(archivo, hoja) {
  preview <- tryCatch(
    read_excel(archivo, sheet = hoja, range = cell_rows(1:10), col_names = FALSE),
    error = function(e) NULL
  )
  if (is.null(preview)) return(hoja)
  for (i in seq_len(nrow(preview))) {
    val <- as.character(preview[[1]][i])
    if (is.na(val) || val == "") next
    Encoding(val) <- "UTF-8"
    m <- regmatches(val, regexpr("\\|\\s*(\\d{6,15})\\s*\\|", val, perl = TRUE))
    if (length(m) > 0) {
      cuenta_raw <- trimws(gsub("\\|", "", m[1]))
      return(limpiar_num_cuenta(cuenta_raw))
    }
  }
  return(hoja)
}

extraer_empresa_hoja <- function(archivo, hoja) {
  preview <- tryCatch(
    read_excel(archivo, sheet = hoja, range = cell_rows(1:5), col_names = FALSE),
    error = function(e) NULL
  )
  if (is.null(preview)) return(paste0("EMPRESA_", hoja))
  for (i in seq_len(nrow(preview))) {
    val <- as.character(preview[[1]][i])
    if (is.na(val) || val == "") next
    Encoding(val) <- "UTF-8"
    if (grepl("^\\d{4}\\s+.+,", val)) return(limpiar_empresa_nombre(val))
  }
  return(paste0("EMPRESA_", hoja))
}

# ----------------------------------------------------------------------------
# [FIX-5] LECTURA DE SALDO INICIAL
#
# PRIORIDAD DE FUENTES (de mayor a menor confiabilidad):
#   1. Hoja "Saldo Inicial" del .xlsm  -> fuente oficial, siempre correcta
#   2. "Inicial del dia" en cabecera   -> saldo al inicio del ultimo dia del extracto,
#                                         NO es el saldo inicial del periodo en general,
#                                         pero para archivos donde el extracto cubre exactamente
#                                         el periodo correcto puede ser util como referencia.
#                                         Se usa como fallback cuando no hay hoja Saldo Inicial.
#   3. "Final Mes Anterior" en cabecera -> ELIMINADO: este valor nunca corresponde al
#                                          saldo inicial del periodo analizado.
#
# AVISO: Si no existe la hoja "Saldo Inicial", la funcion emite una advertencia clara
# porque los resultados pueden ser incorrectos. La solucion correcta es siempre
# procesar el archivo .xlsm completo que incluye dicha hoja.
# ----------------------------------------------------------------------------
leer_saldo_inicial <- function(archivo) {
  hojas <- excel_sheets(archivo)
  
  # --- FUENTE 1: hoja "Saldo Inicial" (fuente oficial) ---
  hoja_saldo <- hojas[tolower(trimws(hojas)) %in% c("saldo inicial", "saldoinicial", "saldo_inicial")]
  if (length(hoja_saldo) > 0) {
    df <- tryCatch(read_excel(archivo, sheet = hoja_saldo[1], col_names = TRUE), error = function(e) NULL)
    if (!is.null(df) && nrow(df) > 0) {
      nms        <- normalizar_texto(names(df))
      col_cuenta <- which(nms %in% c("CUENTA", "NUM CUENTA", "NUMERO DE CUENTA"))[1]
      col_saldo  <- which(nms %in% c("SALDO", "SALDO INICIAL", "SALDO INI"))[1]
      if (is.na(col_cuenta)) col_cuenta <- 1
      if (is.na(col_saldo))  col_saldo  <- 3
      
      resultado <- data.frame(
        Cuenta = sapply(df[[col_cuenta]], limpiar_num_cuenta),
        Saldo  = sapply(df[[col_saldo]],  nz_num),
        stringsAsFactors = FALSE
      )
      resultado <- resultado[
        !is.na(resultado$Cuenta) & resultado$Cuenta != "" & resultado$Cuenta != "NA",
      ]
      if (nrow(resultado) > 0) {
        message("Saldos iniciales cargados desde hoja '", hoja_saldo[1],
                "': ", paste(resultado$Cuenta, collapse = ", "))
        return(list(datos = resultado, fuente = "hoja_saldo_inicial", aviso = NULL))
      }
    }
  }
  
  # --- SIN hoja "Saldo Inicial": emitir advertencia ---
  aviso <- paste0(
    "ADVERTENCIA: Este archivo no contiene la hoja 'Saldo Inicial'. ",
    "Los saldos iniciales se estimaran desde el campo 'Inicial del dia' de cada extracto, ",
    "que representa el saldo al inicio del ultimo dia descargado, NO el saldo inicial del periodo. ",
    "Para resultados correctos, procese el archivo .xlsm completo que incluye dicha hoja."
  )
  message(aviso)
  
  # --- FUENTE 2 (fallback): "Inicial del dia" en cabecera de cada hoja ---
  # Este valor es el saldo al comienzo del ultimo dia del extracto.
  # Es una mejor aproximacion que "Final Mes Anterior" pero sigue sin ser
  # el saldo inicial exacto del periodo cuando el extracto cubre mas de un dia.
  hojas_validas <- hojas[grepl("^\\d{4}$", hojas)]
  filas_saldo   <- list()
  
  for (hoja in hojas_validas) {
    tryCatch({
      preview <- read_excel(archivo, sheet = hoja, range = cell_rows(1:18), col_names = FALSE)
      cuenta  <- extraer_cuenta_hoja(archivo, hoja)
      saldo   <- 0
      
      for (i in seq_len(nrow(preview))) {
        val_a1 <- as.character(preview[[1]][i])
        if (is.na(val_a1)) next
        Encoding(val_a1) <- "UTF-8"
        
        # [FIX-5] Buscar "Inicial del dia" en lugar de "Final Mes Anterior"
        if (grepl("Inicial del d[ií]a", val_a1, ignore.case = TRUE)) {
          # Intentar leer desde columna B si existe
          if (ncol(preview) >= 2) {
            v2 <- nz_num(preview[[2]][i])
            if (v2 != 0) { saldo <- v2; break }
          }
          # Si no, extraer el numero del texto en columna A
          # Formato: "Inicial del dia: $19,422.21 MXP"
          m <- regmatches(val_a1, regexpr("\\$([\\d,]+\\.\\d{2})", val_a1, perl = TRUE))
          if (length(m) > 0) saldo <- nz_num(gsub("[\\$,]", "", m[1]))
          break
        }
      }
      
      if (!cuenta %in% names(filas_saldo)) {
        filas_saldo[[cuenta]] <- saldo
      }
    }, error = function(e) message("  Error leyendo saldo en hoja ", hoja, ": ", e$message))
  }
  
  if (length(filas_saldo) == 0)
    return(list(
      datos  = data.frame(Cuenta = character(0), Saldo = numeric(0), stringsAsFactors = FALSE),
      fuente = "sin_saldo",
      aviso  = aviso
    ))
  
  resultado <- data.frame(
    Cuenta = names(filas_saldo),
    Saldo  = unlist(filas_saldo),
    stringsAsFactors = FALSE, row.names = NULL
  )
  message("Saldos iniciales cargados desde 'Inicial del dia' de hojas individuales: ",
          paste(resultado$Cuenta, collapse = ", "))
  
  list(datos = resultado, fuente = "inicial_del_dia", aviso = aviso)
}

# ----------------------------------------------------------------------------
# CUENTAS ESPECIALES
# ----------------------------------------------------------------------------
CUENTAS_ESPECIALES <- c(
  "2801102", "2801103", "2801104",
  "2801105",   "2801106",   "2801107",
  "2801108",  "2801109",  "2801110"
)

CUENTA_LOGICA_ESPECIAL <- "2801101"

# ----------------------------------------------------------------------------
# CLASIFICACION DE MOVIMIENTO
#
# [FIX-2] Bloque especial para 2801101:
#   - La exclusion de instrumentos MDD usa grepl() para tolerar variaciones.
#   - Orden de evaluacion para cuenta 2801101:
#       1. Es instrumento MDD?          -> excluir
#       2. Es ISR del contrato MDD?     -> salida
#       3. Es TSP AAI A OCI PART?       -> cobranza/salida segun columna
#       4. Contiene TSP (fondeo)?       -> fondeo
#       5. Cualquier otro caso          -> cobranza/salida normal
# ----------------------------------------------------------------------------
clasificar_movimiento <- function(col_descripcion, col_det_larga, deposito, retiro, cuenta) {
  col_e <- normalizar_texto(col_descripcion)
  col_l <- normalizar_texto(col_det_larga)
  dep   <- nz_num(deposito)
  ret   <- nz_num(retiro)
  
  cobranza <- 0; fondeo <- 0; salida <- 0
  
  if (cuenta == CUENTA_LOGICA_ESPECIAL) {
    
    if (grepl("VENC.*CAP.*MDD", col_e, fixed = FALSE) ||
        grepl("INVERSION MDD",  col_e, fixed = FALSE) ||
        grepl("INTERES MDD",    col_e, fixed = FALSE)) {
      # Excluir instrumentos MDD
      
    } else if (grepl("CONTRATO 0506950162", col_l, fixed = TRUE) &&
               grepl("IMPUESTO SOBRE LA RENTA", col_e, fixed = TRUE)) {
      cobranza <- cobranza + dep
      salida   <- salida   + ret
      
    } else if (grepl("TSP AAI A OCI PART", col_l, fixed = TRUE)) {
      if (dep != 0) cobranza <- cobranza + dep
      if (ret != 0) salida   <- salida   + ret
      
    } else if (grepl("TSP", col_l, fixed = TRUE)) {
      fondeo <- fondeo + dep - ret
      
    } else {
      cobranza <- cobranza + dep
      salida   <- salida   + ret
    }
    
  } else {
    if (grepl("TSP", col_l, fixed = TRUE)) {
      fondeo <- fondeo + dep - ret
    } else {
      cobranza <- cobranza + dep
      salida   <- salida   + ret
    }
  }
  
  c(cobranza, fondeo, salida)
}

# ----------------------------------------------------------------------------
# PROCESO PRINCIPAL
# ----------------------------------------------------------------------------
ejecutar_resumen_por_dia <- function(archivo, progress = NULL) {
  
  upd <- function(val, msg) {
    if (!is.null(progress)) progress$set(value = val, message = msg)
    message(msg)
  }
  
  upd(0.05, "Leyendo hojas del archivo...")
  hojas_todas   <- excel_sheets(archivo)
  hojas_validas <- hojas_todas[grepl("^\\d{4}$", hojas_todas)]
  if (length(hojas_validas) == 0) stop("No se encontraron hojas validas (4 digitos) en el archivo.")
  
  upd(0.10, "Cargando saldos iniciales...")
  saldo_ini_resultado <- leer_saldo_inicial(archivo)
  saldo_ini_df        <- saldo_ini_resultado$datos
  fuente_saldo        <- saldo_ini_resultado$fuente
  aviso_saldo         <- saldo_ini_resultado$aviso   # NULL si se uso la hoja oficial
  saldo_ini           <- setNames(saldo_ini_df$Saldo, saldo_ini_df$Cuenta)
  
  upd(0.15, "Procesando movimientos...")
  dict                <- list()
  mapa_empresa_cuenta <- list()
  total_hojas         <- length(hojas_validas)
  
  for (idx_h in seq_along(hojas_validas)) {
    hoja <- hojas_validas[idx_h]
    upd(0.15 + 0.45 * (idx_h / total_hojas), paste0("Procesando hoja: ", hoja))
    
    tryCatch({
      empresa_raw <- extraer_empresa_hoja(archivo, hoja)
      cuenta_hoja <- extraer_cuenta_hoja(archivo, hoja)
      fila_ini    <- detectar_fila_inicio_rd(archivo, hoja)
      
      datos <- read_excel(archivo, sheet = hoja, skip = fila_ini - 1, col_names = TRUE)
      
      if (nrow(datos) == 0 || ncol(datos) < 6) { next }
      
      nms_datos    <- normalizar_texto(names(datos))
      
      # [FIX-6] Usar FECHA DE OPERACION (col B) en lugar de FECHA VALOR (col C).
      # En Banco del Istmo, la "Fecha de Operacion" es cuando el cliente realizó la transaccion
      # y la "Fecha Valor" es cuando el banco la liquida (puede ser 1 dia despues).
      # El saldo correcto del dia X se calcula con los movimientos operados ese dia,
      # independientemente de cuando se acreditaron. Usar col B corrige casos donde
      # un SPEI operado el dia 25 se liquida el dia 26 y el script lo contabilizaba
      # en el dia 26 en lugar del 25, dando saldos incorrectos.
      col_fecha    <- which(nms_datos %in% c("FECHA DE OPERACION", "FECHA OPERACION"))[1]
      if (is.na(col_fecha)) {
        # Fallback a FECHA VALOR si por alguna razon no existe "Fecha de Operacion"
        col_fecha  <- which(nms_datos %in% c("FECHA", "FECHA VALOR"))[1]
        if (!is.na(col_fecha)) {
          message("Hoja ", hoja, ": no se encontro 'Fecha de Operacion', usando 'Fecha Valor' como fallback.")
        }
      }
      
      col_cuenta_d <- which(nms_datos == "CUENTA")[1]
      col_desc     <- which(nms_datos %in% c("DESCRIPCION", "DESCRIPCION CORTA"))[1]
      col_dep      <- which(nms_datos %in% c("DEPOSITOS", "DEPOSITO", "CARGO"))[1]
      col_ret      <- which(nms_datos %in% c("RETIROS", "RETIRO", "ABONO"))[1]
      col_det      <- which(nms_datos %in% c("DESCRIPCION DETALLADA", "DESCRIPCION LARGA", "DETALLE"))[1]
      
      # [FIX-3] Detectar columna SALDO para evitar que col_dep o col_ret apunten a ella.
      col_saldo_j  <- which(nms_datos %in% c("SALDO", "SALDO FINAL", "BALANCE"))[1]
      
      if (is.na(col_fecha) || is.na(col_dep) || is.na(col_ret)) {
        message("Hoja ", hoja, " omitida. Columnas detectadas: ",
                paste(nms_datos, collapse = " | "))
        next
      }
      
      # [FIX-3] Validar que col_dep y col_ret no coincidan con la columna SALDO
      if (!is.na(col_saldo_j)) {
        if (!is.na(col_dep) && col_dep == col_saldo_j) {
          message("ADVERTENCIA hoja ", hoja, ": col_dep coincide con col_saldo. Se omite hoja.")
          next
        }
        if (!is.na(col_ret) && col_ret == col_saldo_j) {
          message("ADVERTENCIA hoja ", hoja, ": col_ret coincide con col_saldo. Se omite hoja.")
          next
        }
      }
      
      cuenta_real <- if (!is.na(col_cuenta_d)) {
        v <- limpiar_num_cuenta(datos[[col_cuenta_d]][1])
        if (!is.na(v) && v != "") v else cuenta_hoja
      } else {
        cuenta_hoja
      }
      
      mapa_empresa_cuenta[[cuenta_real]] <- empresa_raw
      
      for (i in seq_len(nrow(datos))) {
        fecha_dt <- limpiar_fecha(datos[[col_fecha]][i])
        if (is.na(fecha_dt)) next
        
        desc_corta <- if (!is.na(col_desc)) as.character(datos[[col_desc]][i]) else ""
        desc_larga <- if (!is.na(col_det))  as.character(datos[[col_det]][i])  else ""
        if (is.na(desc_corta)) desc_corta <- ""
        if (is.na(desc_larga)) desc_larga <- ""
        
        vals  <- clasificar_movimiento(desc_corta, desc_larga,
                                       datos[[col_dep]][i], datos[[col_ret]][i],
                                       cuenta_real)
        clave <- paste(empresa_raw, cuenta_real, as.character(fecha_dt), sep = "|")
        dict[[clave]] <- if (!is.null(dict[[clave]])) dict[[clave]] + vals else vals
      }
      
    }, error = function(e) message("Error en hoja ", hoja, ": ", e$message))
  }
  
  if (length(dict) == 0) stop("No se encontraron movimientos validos en ninguna hoja.")
  
  upd(0.65, "Ordenando fechas...")
  dias_unicos <- sort(unique(sapply(names(dict), function(k) strsplit(k, "\\|")[[1]][3])))
  
  upd(0.70, "Construyendo tabla de resumen...")
  cuentas_lista <- unique(sapply(names(dict), function(k) {
    p <- strsplit(k, "\\|")[[1]]; paste(p[1], p[2], sep = "|")
  }))
  
  cuentas_normales <- cuentas_lista[!sapply(cuentas_lista, function(x) strsplit(x,"\\|")[[1]][2] %in% CUENTAS_ESPECIALES)]
  cuentas_esp_pres <- cuentas_lista[ sapply(cuentas_lista, function(x) strsplit(x,"\\|")[[1]][2] %in% CUENTAS_ESPECIALES)]
  
  construir_filas <- function(lista_cuentas) {
    if (length(lista_cuentas) == 0) return(data.frame())
    bind_rows(lapply(lista_cuentas, function(ec) {
      partes    <- strsplit(ec, "\\|")[[1]]
      empresa   <- partes[1]; cuenta <- partes[2]
      saldo_ant <- if (!is.na(saldo_ini[cuenta])) saldo_ini[cuenta] else 0
      fila      <- list(Empresa = empresa, Cuenta = cuenta, Saldo_Inicial = saldo_ant)
      for (dia in dias_unicos) {
        arr <- if (!is.null(dict[[paste(empresa, cuenta, dia, sep = "|")]])) {
          dict[[paste(empresa, cuenta, dia, sep = "|")]]
        } else {
          c(0, 0, 0)
        }
        saldo_final <- saldo_ant + arr[1] + arr[2] - arr[3]
        fila[[paste0(dia, "_Cobranza")]]    <- arr[1]
        fila[[paste0(dia, "_Fondeo")]]      <- arr[2]
        fila[[paste0(dia, "_Salidas")]]     <- arr[3]
        fila[[paste0("Saldo_Final_", dia)]] <- saldo_final
        saldo_ant <- saldo_final
      }
      as.data.frame(fila, stringsAsFactors = FALSE, check.names = FALSE)
    }))
  }
  
  upd(0.75, "Generando filas normales...")
  df_normal  <- construir_filas(cuentas_normales)
  if (nrow(df_normal) > 0) df_normal <- df_normal[order(df_normal$Empresa), ]
  
  upd(0.80, "Generando filas especiales...")
  df_especial <- construir_filas(cuentas_esp_pres)
  
  upd(0.85, "Calculando totales...")
  cols_num   <- names(df_normal)[!names(df_normal) %in% c("Empresa", "Cuenta")]
  fila_total <- as.data.frame(
    c(list(Empresa = "TOTAL", Cuenta = ""),
      setNames(lapply(cols_num, function(cn) sum(df_normal[[cn]], na.rm = TRUE)), cols_num)),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  
  fila_total_esp <- if (nrow(df_especial) > 0) {
    cols_e <- names(df_especial)[!names(df_especial) %in% c("Empresa", "Cuenta")]
    as.data.frame(
      c(list(Empresa = "TOTAL ESPECIALES", Cuenta = ""),
        setNames(lapply(cols_e, function(cn) sum(df_especial[[cn]], na.rm = TRUE)), cols_e)),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  } else {
    data.frame()
  }
  
  upd(0.90, "Generando archivo Excel...")
  tmp <- tempfile(fileext = ".xlsx")
  wb  <- createWorkbook()
  addWorksheet(wb, "RESUMEN")
  
  est_header <- createStyle(
    fontSize = 12, fontName = "Arial", fontColour = "#FFFFFF",
    halign = "center", textDecoration = "bold", fgFill = "#FF9966",
    border = "TopBottomLeftRight", borderColour = "#CC6633", wrapText = TRUE
  )
  est_total <- createStyle(
    fontSize = 12, fontName = "Arial", textDecoration = "bold",
    fgFill = "#FFCC99", border = "TopBottomLeftRight", borderColour = "#000000"
  )
  est_num <- createStyle(
    numFmt = '_($* #,##0.00_);_($* (#,##0.00);_($* "-"??_);_(@_)',
    fontName = "Arial", fontSize = 12
  )
  est_base <- createStyle(fontName = "Arial", fontSize = 12)
  
  # [FIX-5] Hoja de AVISOS si no se encontro "Saldo Inicial"
  if (!is.null(aviso_saldo)) {
    addWorksheet(wb, "AVISOS")
    aviso_df <- data.frame(
      Tipo    = "ADVERTENCIA",
      Mensaje = aviso_saldo,
      Detalle = paste0(
        "Fuente de saldos usada: ", fuente_saldo, ". ",
        "Los saldos iniciales provienen del campo 'Inicial del dia' de cada extracto Banco del Istmo. ",
        "Este valor es el saldo al inicio del ULTIMO dia del extracto, no del inicio del periodo. ",
        "Para garantizar calculos correctos, agregue la hoja 'Saldo Inicial' al archivo o ",
        "procese el .xlsm completo que ya la incluye."
      ),
      stringsAsFactors = FALSE
    )
    est_aviso_header <- createStyle(
      fontSize = 11, fontColour = "#FFFFFF", fontName = "Arial",
      halign = "center", textDecoration = "bold", fgFill = "#C0392B",
      border = "TopBottomLeftRight", wrapText = TRUE
    )
    est_aviso_celda <- createStyle(
      fontSize = 10, fontName = "Arial", wrapText = TRUE,
      border = "TopBottomLeftRight", fgFill = "#FDECEA"
    )
    writeData(wb, "AVISOS", aviso_df, startRow = 1)
    addStyle(wb, "AVISOS", est_aviso_header, rows = 1, cols = 1:3, gridExpand = TRUE)
    addStyle(wb, "AVISOS", est_aviso_celda,  rows = 2, cols = 1:3, gridExpand = TRUE)
    setColWidths(wb, "AVISOS", cols = 1, widths = 16)
    setColWidths(wb, "AVISOS", cols = 2, widths = 60)
    setColWidths(wb, "AVISOS", cols = 3, widths = 80)
    setRowHeights(wb, "AVISOS", rows = 2, heights = 80)
    message("Se agrego hoja 'AVISOS' al resultado con la advertencia de saldo inicial.")
  }
  
  df_final <- if (nrow(df_especial) > 0) {
    bind_rows(df_normal, fila_total, df_especial, fila_total_esp)
  } else {
    bind_rows(df_normal, fila_total)
  }
  
  nms_d <- names(df_final)
  nms_d <- gsub("_Cobranza$",    " Cobranza",   nms_d)
  nms_d <- gsub("_Fondeo$",      " Fondeo",     nms_d)
  nms_d <- gsub("_Salidas$",     " Salidas",    nms_d)
  nms_d <- gsub("^Saldo_Final_", "Saldo Final ", nms_d)
  nms_d <- gsub("_", " ", nms_d)
  names(df_final) <- nms_d
  
  writeData(wb, "RESUMEN", df_final, startRow = 1)
  nc <- ncol(df_final)
  nr <- nrow(df_final)
  
  addStyle(wb, "RESUMEN", est_header, rows = 1,          cols = 1:nc, gridExpand = TRUE)
  if (nr >= 2) addStyle(wb, "RESUMEN", est_base, rows = 2:(nr + 1), cols = 1:nc, gridExpand = TRUE)
  if (nr >= 2 && nc >= 3) addStyle(wb, "RESUMEN", est_num, rows = 2:(nr + 1), cols = 3:nc, gridExpand = TRUE, stack = TRUE)
  
  fila_tot_xls <- nrow(df_normal) + 2
  addStyle(wb, "RESUMEN", est_total, rows = fila_tot_xls, cols = 1:nc, gridExpand = TRUE, stack = TRUE)
  if (nrow(df_especial) > 0) {
    addStyle(wb, "RESUMEN", est_total,
             rows = fila_tot_xls + nrow(df_especial) + 1,
             cols = 1:nc, gridExpand = TRUE, stack = TRUE)
  }
  
  setColWidths(wb, "RESUMEN", cols = 1,    widths = 35)
  setColWidths(wb, "RESUMEN", cols = 2,    widths = 18)
  setColWidths(wb, "RESUMEN", cols = 3:nc, widths = 16)
  freezePane(wb, "RESUMEN", firstActiveRow = 2, firstActiveCol = 3)
  
  saveWorkbook(wb, tmp, overwrite = TRUE)
  upd(1.0, "Resumen por dia completado.")
  
  list(
    archivo       = tmp,
    resumen       = df_final,
    dias          = dias_unicos,
    cuentas_total = length(cuentas_lista),
    aviso_saldo   = aviso_saldo,      # NULL = todo bien | texto = advertencia activa
    fuente_saldo  = fuente_saldo
  )
}



