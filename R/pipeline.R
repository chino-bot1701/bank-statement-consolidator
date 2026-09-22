# ==========================================================================
# BANK STATEMENT CONSOLIDATOR - core pipeline
# Direct translation of the original Excel VBA macro into R.
# Kept free of any Shiny dependency so it can be run and tested headless.
# ==========================================================================

library(readxl)
library(openxlsx)
library(dplyr)
library(stringr)

source("R/resumen_por_dia.R")

# ============================================================================
# FUNCIONES TRADUCIDAS DE VBA
# ============================================================================

# Equivalente a QuitaAcentos + LimpiaTexto
limpiar_texto <- function(s) {
  if (is.na(s) || is.null(s)) return("")
  s <- as.character(s)
  s <- toupper(trimws(s))
  s <- chartr("áéíóúüñÁÉÍÓÚÜÑ", "aeiouunAEIOUUN", s)
  s <- gsub("[^A-Z0-9]", "", s)
  return(s)
}

# Detectar fila de encabezados (equivalente a buscar A14 en la macro)
detectar_fila_inicio <- function(archivo, hoja) {
  encabezados_esperados <- c("CUENTA", "FECHA DE OPERACION", "FECHA", "REFERENCIA", "DESCRIPCION")
  preview <- tryCatch(
    read_excel(archivo, sheet = hoja, range = cell_rows(1:25), col_names = FALSE),
    error = function(e) NULL
  )
  if (is.null(preview)) return(14)
  for (i in 1:nrow(preview)) {
    fila <- toupper(trimws(as.character(unlist(preview[i, ]))))
    fila_limpia <- chartr("áéíóúüñÁÉÍÓÚÜÑ", "aeiouunAEIOUUN", fila)
    coincidencias <- sum(sapply(encabezados_esperados, function(e) any(grepl(e, fila_limpia, fixed = TRUE))))
    if (coincidencias >= 2) return(i)
  }
  return(14)
}

# ============================================================================
# MACRO 1: Crear tablas por pestana (leer cada hoja valida)
# ============================================================================
macro1_crear_tablas <- function(archivo) {
  hojas     <- excel_sheets(archivo)
  validas   <- hojas[grepl("^\\d{4}$", hojas)]
  lista     <- list()
  
  for (hoja in validas) {
    tryCatch({
      fila_ini <- detectar_fila_inicio(archivo, hoja)
      datos    <- read_excel(archivo, sheet = hoja, skip = fila_ini - 1, col_names = TRUE)
      
      if (nrow(datos) > 0 && ncol(datos) >= 6) {
        datos <- datos[, 1:min(13, ncol(datos))]
        
        # Eliminar filas con $0.00 USD
        filas_usd <- apply(datos, 1, function(f) any(grepl("\\$0\\.00\\s*USD", as.character(f), ignore.case = TRUE)))
        datos     <- datos[!filas_usd, ]
        
        # Eliminar filas de resumen (CUENTA vacía = filas de DEPÓSITOS/OPERACIONES/TOTAL de Banco del Istmo)
        col_cuenta <- names(datos)[1]
        datos <- datos[!is.na(datos[[col_cuenta]]) & trimws(as.character(datos[[col_cuenta]])) != "", ]
        
        if (nrow(datos) > 0) lista[[hoja]] <- datos
      }
    }, error = function(e) {
      message("Error leyendo hoja: ", hoja, " -> ", e$message)
    })
  }
  return(lista)
}

# ============================================================================
# MACRO 2: Agregar columna "EMPRESA" con texto filtrado de A1
# ============================================================================
extraer_texto_filtrado <- function(texto) {
  if (is.null(texto) || is.na(texto) || texto == "") return("")
  texto <- as.character(texto)
  m <- regmatches(texto, regexpr("^\\d{4}\\s+(.+?)\\s*,", texto, perl = TRUE))
  if (length(m) == 0) return("")
  sub("^\\d{4}\\s+(.+?)\\s*,.*", "\\1", m, perl = TRUE)
}

macro2_agregar_empresa <- function(lista_datos, archivo) {
  hojas_validas <- names(lista_datos)
  
  for (hoja in hojas_validas) {
    texto_filtrado <- ""
    
    for (celda_rango in c("A1", "A2")) {
      val <- tryCatch({
        r <- read_excel(archivo, sheet = hoja, range = celda_rango, col_names = FALSE)
        if (nrow(r) > 0) as.character(r[[1]][1]) else ""
      }, error = function(e) "")
      
      texto_filtrado <- extraer_texto_filtrado(val)
      if (texto_filtrado != "") break
    }
    
    if (texto_filtrado == "") texto_filtrado <- paste0("EMPRESA_", hoja)
    
    lista_datos[[hoja]] <- cbind(EMPRESA = texto_filtrado, lista_datos[[hoja]])
  }
  return(lista_datos)
}

# ============================================================================
# MACRO 3: Consolidar todas las hojas
# ============================================================================
macro3_consolidar <- function(lista_datos) {
  if (length(lista_datos) == 0) return(data.frame())
  bind_rows(lapply(names(lista_datos), function(n) {
    d <- lista_datos[[n]]
    d$HOJA <- n
    # Convertir todas las columnas a character para evitar conflictos de tipo entre hojas
    d[] <- lapply(d, function(col) as.character(col))
    d
  }))
}

# ============================================================================
# MACRO 4: Clasificar datos (78 reglas)
# ============================================================================
macro4_clasificar <- function(df) {
  df$DESCRIPCION_GENERAL  <- ""
  df$DESCRIPCION_DETALLADA <- ""
  
  # Detectar columnas por nombre (robusto ante cualquier estructura del archivo)
  nms <- names(df)
  nombre_c6  <- c("DESCRIPCIÓN", "DESCRIPCION")[c("DESCRIPCIÓN", "DESCRIPCION") %in% nms][1]
  nombre_c13 <- c("DESCRIPCIÓN DETALLADA", "DESCRIPCION DETALLADA")[c("DESCRIPCIÓN DETALLADA", "DESCRIPCION DETALLADA") %in% nms][1]
  
  # Funcion like de VBA
  like <- function(val, patron) {
    if (is.na(val) || val == "") return(FALSE)
    grepl(patron, val, ignore.case = TRUE, fixed = FALSE)
  }
  
  for (i in seq_len(nrow(df))) {
    c6  <- if (!is.na(nombre_c6))  { v <- df[[nombre_c6]][i];  ifelse(is.na(v), "", as.character(v)) } else ""
    c13 <- if (!is.na(nombre_c13)) { v <- df[[nombre_c13]][i]; ifelse(is.na(v), "", as.character(v)) } else ""
    
    dg <- ""; dd <- ""
    
    # --- COBRANZA ---
    if (like(c13, "SPEI RECIBIDO"))                                             { dg <- "COBRANZA";          dd <- "DEPOSITO" }
    if (like(c13, "PAGO EFECTIVO"))                                             { dg <- "COBRANZA";          dd <- "EFECTIVO" }
    if (like(c6,  "DEPOSITO DE CUENTA DE TERCEROS"))                            { dg <- "COBRANZA";          dd <- "DEPOSITO" }
    if (like(c6,  "CHEQUE SBC"))                                                { dg <- "COBRANZA";          dd <- "DEPOSITO CHEQUE" }
    if (like(c6,  "TRASPASO DE CTA") && like(c13, "RENTA CORTA"))                   { dg <- "COBRANZA";          dd <- "QUINTAS" }
    if (like(c6,  "TRASPASO") && like(c13, "DE LA CUENTA:"))                   { dg <- "COBRANZA";          dd <- "QUINTAS" }
    if (like(c6,  "DEPOSITO CHQ.BANCO") && like(c13, "DEPOSITO DE LA CUENTA")) { dg <- "COBRANZA";        dd <- "DEPOSITO" }
    if (like(c6,  "Deposito en efectivo") && like(c13, "Te depositaron"))       { dg <- "COBRANZA";          dd <- "DEPOSITO" }
    if (like(c6,  "REC TRANSF INTL") && like(c13, "INVERSIONISTA INTL"))                   { dg <- "COBRANZA";          dd <- "INDUSTRIAL" }
    if (like(c6,  "DEP.EFECTIVO") && c13 == "")                                 { dg <- "COBRANZA";          dd <- "EFECTIVO" }
    if (like(c6,  "ESTACIONAMIENTO PALT") && c13 == "")                         { dg <- "COBRANZA";          dd <- "ESTACIONAMIENTO" }
    if (like(c6,  "HOTEL ALTAMIRA") && c13 == "")                                { dg <- "COBRANZA";          dd <- "HOTEL" }
    if (like(c6,  "PLAZA PASEO ALTAMIRA") && c13 == "")                            { dg <- "COBRANZA";          dd <- "DEPOSITO" }
    if (like(c13, "ABONO CH/LOCAL"))                                             { dg <- "COBRANZA";          dd <- "DEPOSITO" }
    if (like(c6,  "TRASPASO DE CTA") && like(c13, "DE CUENTA"))                { dg <- "COBRANZA";          dd <- "QUINTAS" }
    if (like(c6,  "CONCENTRACION DE PAGOS") && like(c13, "PAGO DETALLE"))      { dg <- "COBRANZA";          dd <- "DEPOSITO" }
    if (like(c13, "TEF BCO"))                                                    { dg <- "COBRANZA";          dd <- "DEPOSITO" }
    if (like(c6,  "DEPOSITO EN EFECTIVO") && like(c13, "QUERETARO"))           { dg <- "COBRANZA";          dd <- "ESTACIONAMIENTO" }
    
    # --- COMISIONES ---
    if (like(c6,  "COMISION"))                                                   { dg <- "COMISIONES";        dd <- "COMISIONES" }
    if (like(c6,  "CARGO POR COMIS") && like(c13, "INCUMPLIMIENTO"))           { dg <- "COMISIONES";        dd <- "INCUMPLIMIENTO DE PAGOS" }
    if (like(c13, "COMISION"))                                                   { dg <- "COMISIONES";        dd <- "COMISIONES" }
    
    # --- CUENTAS PROPIAS ---
    if (like(c6,  "DEPOSITO DE CUENTA PROPIA"))                                  { dg <- "CUENTAS PROPIAS";   dd <- "DEPOSITO" }
    if (like(c6,  "TRASPASO A CUENTA PROPIA"))                                   { dg <- "CUENTAS PROPIAS";   dd <- "TRASPASO" }
    
    # --- DEVOLUCIONES ---
    if (like(c6,  "DEV.SPEICUENTA BLOQUEADA") && like(c13, "CUENTA BLOQUEADA")) { dg <- "DEVOLUCIONES";      dd <- "DEVOLUCIONES" }
    if (like(c6,  "DEV.SPEICUENTA CANCELADA") && like(c13, "CUENTA CANCELADA")) { dg <- "DEVOLUCIONES";      dd <- "DEVOLUCIONES" }
    if (like(c6,  "DEV.SPEICUENTA"))                                             { dg <- "DEVOLUCIONES DE PAGO"; dd <- "DEVOLUCIONES DE PAGO" }
    
    # --- IMPUESTOS ---
    if (like(c6,  "I.V.A") || like(c6, "IVA"))                                  { dg <- "IMPUESTOS";         dd <- "I.V.A." }
    if (like(c6,  "I.S.R."))                                                     { dg <- "IMPUESTOS";         dd <- "I.S.R." }
    if (like(c13, "INFONACOT"))                                                  { dg <- "IMPUESTOS";         dd <- "INFONACOT" }
    if (like(c13, "PAGO DE IMPUEST") || like(c13, "Impuesto") || like(c13, "IMPUESTO") || like(c13, "IMPUESTOS NUEVO")) { dg <- "IMPUESTOS"; dd <- "IMPUESTOS" }
    if (like(c6,  "Impuesto Sobre la Renta"))                                    { dg <- "IMPUESTOS";         dd <- "I.S.R." }
    if (like(c6,  "PAGO DE SUA-IMSS"))                                           { dg <- "IMPUESTOS";         dd <- "IMPUESTOS" }
    
    # --- INVERSIONES ---
    if (like(c6,  "INVERSION"))                                                  { dg <- "INVERSIONES";       dd <- "INVERSION" }
    if (like(c6,  "INTERES MDD"))                                                { dg <- "INVERSIONES";       dd <- "DEPOSITO DE INTERESES" }
    if (like(c6,  "DESINV"))                                                     { dg <- "INVERSIONES";       dd <- "DESINVERSION" }
    if (like(c6,  "VENC. CAP. MDD") && like(c13, "CONTRATO"))                  { dg <- "INVERSIONES";       dd <- "VENCIMIENTO DE CAPITAL" }
    if (like(c6,  "LIQ.INT.BRUTOS LIQ") && c13 == "")                           { dg <- "INVERSIONES";       dd <- "INVERSIONES" }
    
    # --- OTROS ---
    if (like(c6,  "CHEQUE") && like(c13, "DEPOSITO A CTA."))                   { dg <- "OTROS";             dd <- "OTROS" }
    if (like(c6,  "CHEQUE") && c13 == "")                                        { dg <- "OTROS";             dd <- "OTROS" }
    if (like(c6,  "COM.CHQ.EXPED. LIQ") && c13 == "")                           { dg <- "OTROS";             dd <- "OTROS" }
    
    # --- PAGO DE NOMINA ---
    if (like(c6,  "RETIRO DEP. ELECTRONICO") && like(c13, "DE LA EMISORA :"))  { dg <- "PAGO DE NOMINA";   dd <- "PAGO DE NOMINA" }
    
    # --- PAGO PROVEEDORES ---
    if (like(c6,  "TRASPASO A CUENTA DE TERCEROS"))                              { dg <- "PAGO PROVEEDORES";  dd <- "PAGO PROVEEDORES" }
    if (like(c6,  "COMPRA ORDEN DE PAGO SPEI"))                                  { dg <- "PAGO PROVEEDORES";  dd <- "PAGO PROVEEDORES" }
    if (like(c13, "COMERCIALIZADOR"))                                            { dg <- "PAGO PROVEEDORES";  dd <- "PAGO PROVEEDORES" }
    
    # --- SERVICIOS ---
    if (like(c13, "AGUA Y DRENAJE"))                                             { dg <- "SERVICIOS";         dd <- "AGUA Y DRENAJE" }
    if (like(c13, "COMISION FEDERA"))                                            { dg <- "SERVICIOS";         dd <- "CFE" }
    if (like(c13, "AGUA POTABLE"))                                               { dg <- "SERVICIOS";         dd <- "AGUA POTABLE" }
    if (like(c13, "AGUAS DEL MUNIC"))                                            { dg <- "SERVICIOS";         dd <- "AGUA POTABLE" }
    if (like(c13, "ASEGURADORA ISTMO"))                                                      { dg <- "SERVICIOS";         dd <- "SEGUROS" }
    if (like(c13, "SEGUROS"))                                                    { dg <- "SERVICIOS";         dd <- "SEGUROS" }
    if (like(c13, "TELECOM SIERRA"))                                                     { dg <- "SERVICIOS";         dd <- "TELEFONIA" }
    if (like(c13, "REDFIBRA"))                                                      { dg <- "SERVICIOS";         dd <- "INTERNET" }
    if (like(c13, "\\(AGUA POT"))                                                { dg <- "SERVICIOS";         dd <- "AGUA POTABLE" }
    if (like(c13, "TELEFONOS"))                                                  { dg <- "SERVICIOS";         dd <- "TELEFONIA" }
    if (like(c13, "TV SATELITAL"))                                                        { dg <- "SERVICIOS";         dd <- "TV" }
    if (like(c13, "GAS URBANO"))                                                    { dg <- "SERVICIOS";         dd <- "GAS NATURAL" }
    
    # --- CHEQUE ---
    if (like(c6,  "CERTIFICA.CHQ."))                                             { dg <- "CHEQUE";            dd <- "CERTIFICADO" }
    
    # --- PRESTAMO ---
    if (like(c6,  "PAGO DE CAPITAL"))                                            { dg <- "PRESTAMO";          dd <- "PAGO DE PRESTAMO" }
    if (like(c6,  "PAGO DE INTERESES"))                                          { dg <- "PRESTAMO";          dd <- "PAGO DE INTERESES" }
    
    # --- CREDITO ---
    if (like(c6,  "PAGO ARRENDADORA"))                                           { dg <- "CREDITO";           dd <- "ARRENDAMIENTO" }
    
    # --- COMPENSACION ---
    if (like(c13, "COMPENSACION"))                                               { dg <- "COMPENSACION";      dd <- "COMPENSACION" }
    
    # --- PENDIENTE ---
    if (like(c13, "GOBIERNO DEL ES"))                                            { dg <- "PENDIENTE";         dd <- "PENDIENTE" }
    if (like(c6,  "COMPRA ORDEN DE PAGO SPID"))                                  { dg <- "PENDIENTE";         dd <- "PENDIENTE" }
    if (like(c6,  "RECEPCION ORDEN DE PAGO SPID"))                               { dg <- "PENDIENTE";         dd <- "PENDIENTE" }
    if (like(c6,  "RENTA MENSUAL"))                                              { dg <- "PENDIENTE";         dd <- "PENDIENTE" }
    if (like(c6,  "CUENTAS POR PAGAR - SAP"))                                   { dg <- "PENDIENTE";         dd <- "PENDIENTE" }
    if (like(c6,  "DEP.PAGO MULTIPLE"))                                          { dg <- "PENDIENTE";         dd <- "PENDIENTE" }
    if (like(c13, "GEM MUNICIPIOS"))                                             { dg <- "PENDIENTE";         dd <- "PENDIENTE" }
    if (like(c13, "MUNICIPIO DE PU"))                                            { dg <- "PENDIENTE";         dd <- "PENDIENTE" }
    if (like(c6,  "CHEQ CA"))                                                    { dg <- "PENDIENTE";         dd <- "PENDIENTE" }
    if (like(c6,  "CORPORATIVO HABITACI"))                                       { dg <- "PENDIENTE";         dd <- "PENDIENTE" }
    if (like(c6,  "DEV.SPEI"))                                                   { dg <- "DEVOLUCIONES DE PAGO"; dd <- "DEVOLUCIONES DE PAGO" }
    if (like(c6,  "COM NL"))                                                     { dg <- "PENDIENTE";         dd <- "PENDIENTE" }
    if (like(c6,  "REF. EXTRANJERO:") && like(c13, "COMPRA Y VENTA DE DOLARES")) { dg <- "PENDIENTE";        dd <- "PENDIENTE" }
    if (like(c6,  "REF. EXTRANJERO:") && like(c13, "TIPO OP:"))                 { dg <- "PENDIENTE";         dd <- "PENDIENTE" }
    if (like(c6,  "RETIRO DEP. ELECTRONICO") && !like(c13, "DE LA EMISORA :")) { dg <- "PENDIENTE";          dd <- "PENDIENTE" }
    
    # --- SIN MOVIMIENTOS ---
    if (like(c6,  "SIN MOVIMIENTOS"))                                            { dg <- "SIN MOVIMIENTOS";   dd <- "SIN MOVIMIENTOS" }
    
    df$DESCRIPCION_GENERAL[i]  <- dg
    df$DESCRIPCION_DETALLADA[i] <- dd
  }
  return(df)
}

# ============================================================================
# MACRO 5: Extraer cuenta de descripcion (busca "LA CUENTA")
# ============================================================================
macro5_extraer_cuenta <- function(df) {
  df$CUENTA_DESCRIPCION <- ""
  
  # Detectar columna por nombre en lugar de posicion hardcodeada
  nms_m5 <- names(df)
  nombre_desc <- c("DESCRIPCIÓN DETALLADA", "DESCRIPCION DETALLADA")[c("DESCRIPCIÓN DETALLADA", "DESCRIPCION DETALLADA") %in% nms_m5][1]
  if (is.na(nombre_desc)) return(df)
  
  for (i in seq_len(nrow(df))) {
    val <- as.character(df[[nombre_desc]][i])
    if (is.na(val) || val == "") next
    
    pos <- regexpr("LA CUENTA", val, ignore.case = TRUE)
    if (pos == -1) next
    
    inicio <- pos + nchar("LA CUENTA")
    resto  <- substr(val, inicio, nchar(val))
    pos_coma <- regexpr(",", resto)
    
    if (pos_coma > 0) {
      cuenta <- trimws(substr(resto, 1, pos_coma - 1))
      if (nchar(cuenta) > 10) cuenta <- substr(cuenta, 1, 10)
      df$CUENTA_DESCRIPCION[i] <- cuenta
    }
  }
  return(df)
}

# ============================================================================
# MACRO 6: Crear hoja Fondeo
# ============================================================================
macro6_fondeo <- function(df) {
  cuentas_propias <- df[df$DESCRIPCION_GENERAL == "CUENTAS PROPIAS", ]
  
  if (nrow(cuentas_propias) == 0) {
    return(data.frame(Empresa = character(0), Cuenta = character(0), stringsAsFactors = FALSE))
  }
  
  col_empresa <- "EMPRESA"
  col_cuenta  <- names(df)[2]  # segunda columna = CUENTA (primera del excel original)
  
  fondeo <- cuentas_propias %>%
    select(Empresa = all_of(col_empresa), Cuenta = all_of(col_cuenta)) %>%
    filter(!is.na(Cuenta) & Cuenta != "") %>%
    distinct(Cuenta, .keep_all = TRUE) %>%
    arrange(Empresa, Cuenta)
  
  if ("CUENTA_DESCRIPCION" %in% names(cuentas_propias)) {
    cuentas_extra <- cuentas_propias %>%
      filter(!is.na(CUENTA_DESCRIPCION) & CUENTA_DESCRIPCION != "") %>%
      select(Empresa = all_of(col_empresa), Cuenta = CUENTA_DESCRIPCION) %>%
      distinct(Cuenta, .keep_all = TRUE)
    
    fondeo <- bind_rows(fondeo, cuentas_extra) %>%
      distinct(Cuenta, .keep_all = TRUE) %>%
      arrange(Empresa, Cuenta)
  }
  
  return(fondeo)
}

# ============================================================================
# PROCESO COMPLETO - CONSOLIDADO
# ============================================================================
ejecutar_proceso_completo <- function(archivo, progress = NULL) {
  upd <- function(val, msg) {
    if (!is.null(progress)) progress$set(value = val, message = msg)
  }
  
  upd(0.10, "Leyendo hojas del archivo...")
  lista <- macro1_crear_tablas(archivo)
  
  if (length(lista) == 0) stop("No se encontraron hojas validas (4 digitos) en el archivo.")
  
  upd(0.25, "Extrayendo nombre de empresa...")
  lista <- macro2_agregar_empresa(lista, archivo)
  
  upd(0.40, "Consolidando hojas...")
  consolidado <- macro3_consolidar(lista)
  
  upd(0.55, "Clasificando movimientos...")
  consolidado <- macro4_clasificar(consolidado)
  
  upd(0.70, "Extrayendo cuentas de descripcion...")
  consolidado <- macro5_extraer_cuenta(consolidado)
  
  upd(0.85, "Generando hoja Fondeo...")
  fondeo <- macro6_fondeo(consolidado)
  
  upd(0.95, "Generando archivo Excel...")
  
  tmp <- tempfile(fileext = ".xlsx")
  wb  <- createWorkbook()
  
  # Estilos
  estilo_header <- createStyle(
    fontSize = 11, fontColour = "#FFFFFF", fontName = "Calibri",
    halign = "center", valign = "center", textDecoration = "bold",
    fgFill = "#802e25", border = "TopBottomLeftRight", borderColour = "#401712",
    wrapText = TRUE
  )
  estilo_header_fondeo <- createStyle(
    fontSize = 11, fontColour = "#FFFFFF", fontName = "Calibri",
    halign = "center", valign = "center", textDecoration = "bold",
    fgFill = "#401712", border = "TopBottomLeftRight", borderColour = "#000000"
  )
  
  # Hoja Consolidado
  addWorksheet(wb, "Consolidado")
  writeData(wb, "Consolidado", consolidado, startRow = 1)
  addStyle(wb, "Consolidado", estilo_header, rows = 1, cols = 1:ncol(consolidado), gridExpand = TRUE)
  setColWidths(wb, "Consolidado", cols = 1:ncol(consolidado), widths = "auto")
  
  # Hoja Fondeo
  addWorksheet(wb, "Fondeo")
  writeData(wb, "Fondeo", fondeo, startRow = 1)
  if (nrow(fondeo) > 0) {
    addStyle(wb, "Fondeo", estilo_header_fondeo, rows = 1, cols = 1:ncol(fondeo), gridExpand = TRUE)
    setColWidths(wb, "Fondeo", cols = 1:ncol(fondeo), widths = c(35, 20))
  }
  
  saveWorkbook(wb, tmp, overwrite = TRUE)
  upd(1.0, "Proceso completado.")
  
  list(
    archivo       = tmp,
    consolidado   = consolidado,
    fondeo        = fondeo,
    hojas_leidas  = length(lista)
  )
}

# ============================================================================
# UI
# ============================================================================
