# ==========================================================================
# BANK STATEMENT CONSOLIDATOR - Shiny front end
# The processing logic lives in R/pipeline.R.
# Run with:  shiny::runApp()
# ==========================================================================

library(shiny)
library(shinyjs)

source("R/pipeline.R")

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;600&family=DM+Serif+Display&display=swap');

      *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }

      :root {
        --bg:        #f5f0eb;
        --sidebar:   #2c2420;
        --sidebar2:  #3a2e29;
        --card:      #ffffff;
        --border:    #ddd5c8;
        --border2:   #4a3830;
        --text:      #2c2420;
        --text-soft: #7a6e66;
        --text-side: #c8bdb5;
        --accent:    #9b4f3a;
        --accent2:   #c47a5a;
        --accent-lt: #f0e0d6;
        --ok:        #6b8f6b;
        --ok-lt:     #e8f0e8;
      }

      body {
        font-family: 'DM Sans', sans-serif;
        background: var(--bg);
        color: var(--text);
        min-height: 100vh;
      }

      /* ---- HEADER ---- */
      .app-header {
        background: var(--sidebar);
        padding: 20px 36px;
        display: flex;
        align-items: center;
        gap: 16px;
        border-bottom: 1px solid var(--border2);
      }
      .app-header-text h1 {
        font-family: 'DM Serif Display', serif;
        font-size: 1.25rem; font-weight: 400;
        color: #f5f0eb; letter-spacing: 0.3px;
        line-height: 1;
      }
      .app-header-text p {
        font-size: 0.78rem; color: var(--text-side);
        margin-top: 3px; font-weight: 300; letter-spacing: 0.5px;
        text-transform: uppercase;
      }

      /* ---- LAYOUT ---- */
      .main-layout {
        display: grid;
        grid-template-columns: 300px 1fr;
        gap: 0;
        min-height: calc(100vh - 77px);
      }
      .sidebar {
        background: var(--sidebar);
        border-right: 1px solid var(--border2);
        padding: 24px 20px;
        display: flex; flex-direction: column; gap: 16px;
      }
      .main-content {
        padding: 28px 32px;
        overflow-x: auto;
        background: var(--bg);
      }

      /* ---- LABELS SIDEBAR ---- */
      .side-label {
        font-size: 0.68rem; font-weight: 600;
        text-transform: uppercase; letter-spacing: 2px;
        color: var(--text-side);
        margin-bottom: 8px;
        opacity: 0.7;
      }

      /* ---- UPLOAD ---- */
      .upload-wrapper {
        position: relative;
        cursor: pointer;
      }
      .upload-visual {
        border: 1.5px dashed #5a4840;
        border-radius: 8px;
        padding: 22px 14px;
        text-align: center;
        transition: all 0.2s;
        background: rgba(255,255,255,0.04);
      }
      .upload-wrapper:hover .upload-visual {
        border-color: var(--accent2);
        background: rgba(196,122,90,0.08);
      }
      .upload-visual svg {
        display: block; margin: 0 auto 8px;
        opacity: 0.5;
      }
      .upload-visual-title {
        font-size: 0.82rem; color: #c8bdb5;
        font-weight: 500; margin-bottom: 2px;
      }
      .upload-visual-sub {
        font-size: 0.72rem; color: #6a5e58;
      }
      .upload-wrapper .shiny-input-container {
        position: absolute !important;
        top: 0; left: 0; width: 100%; height: 100%;
        opacity: 0 !important;
        cursor: pointer !important;
        margin: 0 !important;
      }
      .upload-wrapper input[type=file] {
        width: 100% !important; height: 100% !important;
        cursor: pointer !important;
      }

      /* ---- FILE INFO ---- */
      .file-info {
        background: rgba(155,79,58,0.15);
        border: 1px solid #5a3a30;
        border-radius: 6px; padding: 9px 12px;
        font-size: 0.78rem; color: var(--text-side);
        display: flex; align-items: center; gap: 8px;
        word-break: break-all;
      }
      .file-dot {
        width: 7px; height: 7px; border-radius: 50%;
        background: var(--accent2); flex-shrink: 0;
      }

      /* ---- BOTONES PROCESO ---- */
      .btn-ejecutar {
        width: 100%;
        background: var(--accent);
        color: #fff !important;
        border: none;
        border-radius: 7px;
        padding: 12px 0;
        font-family: 'DM Sans', sans-serif;
        font-size: 0.88rem; font-weight: 600;
        letter-spacing: 1px; text-transform: uppercase;
        cursor: pointer;
        transition: background 0.2s, transform 0.15s;
        margin-bottom: 4px;
      }
      .btn-ejecutar:hover:not(:disabled) {
        background: #b35e47;
        transform: translateY(-1px);
      }
      .btn-ejecutar:disabled { opacity: 0.4; cursor: not-allowed; }

      .btn-resumen {
        width: 100%;
        background: #3a5a3a;
        color: #fff !important;
        border: none;
        border-radius: 7px;
        padding: 12px 0;
        font-family: 'DM Sans', sans-serif;
        font-size: 0.88rem; font-weight: 600;
        letter-spacing: 1px; text-transform: uppercase;
        cursor: pointer;
        transition: background 0.2s, transform 0.15s;
      }
      .btn-resumen:hover:not(:disabled) {
        background: #4a7a4a;
        transform: translateY(-1px);
      }
      .btn-resumen:disabled { opacity: 0.4; cursor: not-allowed; }

      .btn-descargar {
        width: 100%;
        background: transparent;
        color: var(--accent2) !important;
        border: 1px solid #5a3a30;
        border-radius: 7px;
        padding: 11px 0;
        font-family: 'DM Sans', sans-serif;
        font-size: 0.85rem; font-weight: 600;
        letter-spacing: 0.8px; text-transform: uppercase;
        cursor: pointer;
        transition: all 0.2s;
      }
      .btn-descargar:hover {
        background: rgba(155,79,58,0.2);
        border-color: var(--accent2);
        color: #f5f0eb !important;
      }

      /* ---- DIVIDER ---- */
      .side-divider {
        height: 1px; background: var(--border2);
        margin: 4px 0;
      }

      /* ---- STATS ---- */
      .stats-grid {
        display: grid; grid-template-columns: 1fr 1fr;
        gap: 8px;
      }
      .stat-box {
        background: rgba(255,255,255,0.04);
        border: 1px solid var(--border2);
        border-radius: 7px;
        padding: 10px 12px;
        text-align: center;
      }
      .stat-number {
        font-family: 'DM Serif Display', serif;
        font-size: 2rem; font-weight: 400;
        color: var(--accent2); line-height: 1;
      }
      .stat-label {
        font-size: 0.78rem; color: #6a5e58;
        text-transform: uppercase; letter-spacing: 1px;
        margin-top: 3px;
      }

      /* ---- CONTENIDO PRINCIPAL ---- */
      .section-title {
        font-family: 'DM Serif Display', serif;
        font-size: 1.1rem; font-weight: 400;
        color: var(--text); margin-bottom: 4px;
      }
      .section-sub {
        font-size: 0.8rem; color: var(--text-soft);
        margin-bottom: 20px;
      }

      /* ---- TABLA PREVIEW ---- */
      .table-wrap {
        overflow-x: auto;
        border-radius: 8px;
        border: 1px solid var(--border);
        box-shadow: 0 1px 4px rgba(0,0,0,0.06);
      }
      table.preview-table {
        width: 100%; border-collapse: collapse;
        font-size: 0.8rem; background: #fff;
      }
      table.preview-table thead tr {
        background: var(--sidebar);
      }
      table.preview-table thead th {
        color: #e0d4c3; font-weight: 500;
        padding: 10px 14px; text-align: left;
        white-space: nowrap;
        font-size: 0.75rem; letter-spacing: 0.5px;
        border-right: 1px solid rgba(255,255,255,0.07);
      }
      table.preview-table tbody tr:nth-child(even) { background: #faf7f4; }
      table.preview-table tbody tr:hover { background: var(--accent-lt); }
      table.preview-table tbody td {
        padding: 8px 14px; color: var(--text);
        border-right: 1px solid var(--border);
        border-bottom: 1px solid var(--border);
        white-space: nowrap; max-width: 240px;
        overflow: hidden; text-overflow: ellipsis;
      }

      /* ---- PROGRESS ---- */
      .shiny-progress-bar { background: var(--accent) !important; }
      .progress { margin: 0 !important; }

      /* ---- ALERTS ---- */
      .alerta-ok {
        background: var(--ok-lt);
        border: 1px solid #b8d0b8;
        border-left: 3px solid var(--ok);
        border-radius: 6px; padding: 12px 16px;
        font-size: 0.85rem; color: #3d5e3d;
      }
      .alerta-error {
        background: #fdf0ed;
        border: 1px solid #e8c5b8;
        border-left: 3px solid var(--accent);
        border-radius: 6px; padding: 12px 16px;
        font-size: 0.85rem; color: var(--accent);
      }
      .alerta-info {
        background: #fff;
        border: 1px solid var(--border);
        border-radius: 8px; padding: 36px 24px;
        font-size: 0.875rem; color: var(--text-soft);
        text-align: center;
        box-shadow: 0 1px 3px rgba(0,0,0,0.05);
      }
      .alerta-info strong {
        display: block; font-size: 1rem;
        color: var(--text); margin-bottom: 6px;
        font-family: 'DM Serif Display', serif;
        font-weight: 400;
      }

      /* ---- TABS ---- */
      .nav-tabs {
        border-bottom: 1.5px solid var(--border);
        margin-bottom: 18px;
      }
      .nav-tabs > li > a {
        font-family: 'DM Sans', sans-serif;
        font-size: 0.82rem; font-weight: 500;
        letter-spacing: 0.5px;
        color: var(--text-soft) !important;
        background: transparent !important;
        border: none !important; border-radius: 0 !important;
        padding: 8px 16px;
        transition: color 0.2s;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li > a:hover { color: var(--accent) !important; }
      .nav-tabs > li.active {
        border-bottom: 2px solid var(--accent) !important;
        margin-bottom: -1.5px;
      }
      .tab-content { padding-top: 2px; }

      /* ---- SECCION BOTONES ---- */
      .seccion-botones {
        display: flex; flex-direction: column; gap: 8px;
      }
      .seccion-label {
        font-size: 0.65rem; font-weight: 600;
        text-transform: uppercase; letter-spacing: 2px;
        color: #5a4840; margin-bottom: 2px;
      }
    "))
  ),
  
  tags$script(HTML("
    $(document).on('click', '.upload-visual', function() {
      $(this).closest('.upload-wrapper').find('input[type=file]').trigger('click');
    });
  ")),
  
  # HEADER
  div(class = "app-header",
      div(class = "app-header-text",
          tags$h1("Macro Consolidador"),
          tags$p("Procesador de estados de cuenta Banco del Istmo")
      )
  ),
  
  # LAYOUT PRINCIPAL
  div(class = "main-layout",
      
      # SIDEBAR
      div(class = "sidebar",
          
          # Upload
          div(
            div(class = "side-label", "Archivo de entrada"),
            div(class = "upload-wrapper",
                fileInput("archivo", label = NULL, accept = c(".xlsx", ".xls", ".xlsm")),
                div(class = "upload-visual",
                    tags$svg(width = "28", height = "28", viewBox = "0 0 24 24",
                             fill = "none", stroke = "#c8bdb5", `stroke-width` = "1.5",
                             tags$path(`stroke-linecap` = "round", `stroke-linejoin` = "round",
                                       d = "M3 16.5v2.25A2.25 2.25 0 005.25 21h13.5A2.25 2.25 0 0021 18.75V16.5m-13.5-9L12 3m0 0l4.5 4.5M12 3v13.5"
                             )
                    ),
                    div(class = "upload-visual-title", "Seleccionar archivo"),
                    div(class = "upload-visual-sub", "Haz clic o arrastra un .xlsx / .xlsm")
                )
            ),
            br(),
            uiOutput("file_info_ui")
          ),
          
          div(class = "side-divider"),
          
          # Dos botones de proceso
          div(class = "seccion-botones",
              div(class = "seccion-label", "Procesos disponibles"),
              
              # Boton 1: Consolidado y clasificacion
              actionButton("ejecutar", "Consolidar y clasificar",
                           class = "btn-ejecutar", disabled = NA),
              
              # Boton 2: Resumen por dia (VBA traducido)
              actionButton("ejecutar_resumen", "Resumen por dia",
                           class = "btn-resumen", disabled = NA)
          ),
          
          # Botones de descarga (aparecen segun resultado activo)
          uiOutput("descargar_ui"),
          
          div(class = "side-divider"),
          
          # Stats
          uiOutput("stats_ui")
      ),
      
      # CONTENIDO PRINCIPAL
      div(class = "main-content",
          uiOutput("contenido_ui")
      )
  )
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {
  
  resultado        <- reactiveVal(NULL)   # resultado del consolidado
  resultado_resumen <- reactiveVal(NULL)  # resultado del resumen por dia
  procesando       <- reactiveVal(FALSE)
  modo_activo      <- reactiveVal("ninguno")  # "consolidado" | "resumen" | "ninguno"
  
  # Habilitar botones cuando hay archivo
  observe({
    if (!is.null(input$archivo)) {
      shinyjs::enable("ejecutar")
      shinyjs::enable("ejecutar_resumen")
    } else {
      shinyjs::disable("ejecutar")
      shinyjs::disable("ejecutar_resumen")
    }
  })
  
  # Info del archivo cargado
  output$file_info_ui <- renderUI({
    req(input$archivo)
    div(class = "file-info",
        div(class = "file-dot"),
        span(input$archivo$name)
    )
  })
  
  # ---- BOTON 1: Consolidar y clasificar ----
  observeEvent(input$ejecutar, {
    req(input$archivo)
    procesando(TRUE)
    resultado(NULL)
    resultado_resumen(NULL)
    modo_activo("consolidado")
    
    withProgress(message = "Iniciando consolidado...", value = 0, {
      tryCatch({
        res <- ejecutar_proceso_completo(
          archivo  = input$archivo$datapath,
          progress = list(set = function(value, message) setProgress(value, message = message))
        )
        resultado(res)
      }, error = function(e) {
        resultado(list(error = e$message))
      })
    })
    
    procesando(FALSE)
  })
  
  # ---- BOTON 2: Resumen por dia ----
  observeEvent(input$ejecutar_resumen, {
    req(input$archivo)
    procesando(TRUE)
    resultado(NULL)
    resultado_resumen(NULL)
    modo_activo("resumen")
    
    withProgress(message = "Iniciando resumen por dia...", value = 0, {
      tryCatch({
        res <- ejecutar_resumen_por_dia(
          archivo  = input$archivo$datapath,
          progress = list(set = function(value, message) setProgress(value, message = message))
        )
        resultado_resumen(res)
      }, error = function(e) {
        resultado_resumen(list(error = e$message))
      })
    })
    
    procesando(FALSE)
  })
  
  # ---- STATS (solo para consolidado) ----
  output$stats_ui <- renderUI({
    res <- resultado()
    if (is.null(res) || !is.null(res$error)) return(NULL)
    
    div(style = "margin-top: 6px;",
        div(class = "side-label", "Resumen"),
        div(class = "stats-grid",
            div(class = "stat-box",
                div(class = "stat-number", res$hojas_leidas),
                div(class = "stat-label", "Hojas")
            ),
            div(class = "stat-box",
                div(class = "stat-number", nrow(res$consolidado)),
                div(class = "stat-label", "Registros")
            ),
            div(class = "stat-box",
                div(class = "stat-number",
                    sum(res$consolidado$DESCRIPCION_GENERAL != "", na.rm = TRUE)
                ),
                div(class = "stat-label", "Clasificados")
            ),
            div(class = "stat-box",
                div(class = "stat-number", nrow(res$fondeo)),
                div(class = "stat-label", "Fondeo")
            )
        )
    )
  })
  
  # ---- BOTONES DE DESCARGA ----
  output$descargar_ui <- renderUI({
    modo <- modo_activo()
    
    if (modo == "consolidado") {
      res <- resultado()
      if (is.null(res) || !is.null(res$error)) return(NULL)
      tagList(
        downloadButton("descargar",            "Descargar consolidado", class = "btn-descargar"),
        br(),
        downloadButton("descargar_categorias", "Descargar categorias",  class = "btn-descargar",
                       style = "margin-top: 6px;")
      )
    } else if (modo == "resumen") {
      res <- resultado_resumen()
      if (is.null(res) || !is.null(res$error)) return(NULL)
      downloadButton("descargar_resumen", "Descargar resumen", class = "btn-descargar")
    } else {
      NULL
    }
  })
  
  # Descarga consolidado
  output$descargar <- downloadHandler(
    filename = function() paste0("Consolidado_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content  = function(file) {
      res <- resultado()
      req(!is.null(res) && is.null(res$error))
      file.copy(res$archivo, file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  # Descarga categorias
  output$descargar_categorias <- downloadHandler(
    filename = function() paste0("Categorias_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content  = function(file) {
      res <- resultado()
      req(!is.null(res) && is.null(res$error))
      
      tabla_cat <- res$consolidado %>%
        group_by(DESCRIPCION_GENERAL) %>%
        summarise(Registros = n(), .groups = "drop") %>%
        arrange(desc(Registros))
      
      wb_cat <- createWorkbook()
      addWorksheet(wb_cat, "Categorias")
      writeData(wb_cat, "Categorias", tabla_cat)
      
      estilo <- createStyle(
        fontSize = 11, fontColour = "#FFFFFF", fontName = "Calibri",
        halign = "center", textDecoration = "bold",
        fgFill = "#2c2420", border = "TopBottomLeftRight", borderColour = "#4a3830"
      )
      addStyle(wb_cat, "Categorias", estilo, rows = 1, cols = 1:2, gridExpand = TRUE)
      setColWidths(wb_cat, "Categorias", cols = 1:2, widths = c(30, 15))
      saveWorkbook(wb_cat, file, overwrite = TRUE)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  # Descarga resumen por dia
  output$descargar_resumen <- downloadHandler(
    filename = function() paste0("Resumen_por_dia_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content  = function(file) {
      res <- resultado_resumen()
      req(!is.null(res) && is.null(res$error))
      file.copy(res$archivo, file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  # ---- CONTENIDO PRINCIPAL ----
  output$contenido_ui <- renderUI({
    modo <- modo_activo()
    
    # Estado inicial
    if (modo == "ninguno" || (!procesando() && is.null(resultado()) && is.null(resultado_resumen()))) {
      if (procesando()) {
        return(div(class = "alerta-info",
                   tags$strong("Procesando..."),
                   "El archivo se esta procesando, por favor espera."
        ))
      }
      return(div(class = "alerta-info",
                 tags$strong("Sin proceso ejecutado"),
                 "Selecciona un archivo Excel y presiona uno de los dos botones para comenzar."
      ))
    }
    
    # ---- VISTA: CONSOLIDADO ----
    if (modo == "consolidado") {
      res <- resultado()
      
      if (is.null(res)) {
        return(div(class = "alerta-info",
                   tags$strong("Procesando..."),
                   "Consolidando y clasificando movimientos, por favor espera."
        ))
      }
      if (!is.null(res$error)) {
        return(div(class = "alerta-error",
                   tags$strong("Error al consolidar:"), br(), res$error
        ))
      }
      
      tagList(
        div(class = "alerta-ok", style = "margin-bottom: 20px;",
            tags$strong("Consolidado completado. "),
            sprintf("Se procesaron %d hojas y se generaron %d registros.",
                    res$hojas_leidas, nrow(res$consolidado))
        ),
        tabsetPanel(
          tabPanel("Consolidado",
                   br(),
                   div(class = "section-title", "Consolidado"),
                   div(class = "section-sub",
                       paste("Vista previa — primeros 50 registros de", nrow(res$consolidado), "totales")),
                   div(class = "table-wrap", renderTabla(head(res$consolidado, 50)))
          ),
          tabPanel("Fondeo",
                   br(),
                   div(class = "section-title", "Hoja de Fondeo"),
                   div(class = "section-sub", "Cuentas propias detectadas en el consolidado"),
                   div(class = "table-wrap", renderTabla(res$fondeo))
          ),
          tabPanel("Categorias",
                   br(),
                   div(class = "section-title", "Distribucion por categoria"),
                   div(class = "section-sub", "Resumen de clasificacion de movimientos"),
                   div(class = "table-wrap",
                       renderTabla(
                         res$consolidado %>%
                           group_by(DESCRIPCION_GENERAL) %>%
                           summarise(Registros = n(), .groups = "drop") %>%
                           arrange(desc(Registros))
                       )
                   )
          )
        )
      )
    }
    
    # ---- VISTA: RESUMEN POR DIA ----
    else if (modo == "resumen") {
      res <- resultado_resumen()
      
      if (is.null(res)) {
        return(div(class = "alerta-info",
                   tags$strong("Procesando..."),
                   "Generando resumen por dia, por favor espera."
        ))
      }
      if (!is.null(res$error)) {
        return(div(class = "alerta-error",
                   tags$strong("Error al generar resumen:"), br(), res$error
        ))
      }
      
      tagList(
        div(class = "alerta-ok", style = "margin-bottom: 20px;",
            tags$strong("Resumen por dia completado. "),
            sprintf("Se procesaron %d cuentas en %d dias.",
                    res$cuentas_total, length(res$dias))
        ),
        # [FIX-5] Mostrar advertencia si el archivo no tenia hoja "Saldo Inicial"
        if (!is.null(res$aviso_saldo)) {
          div(
            style = "margin-bottom: 16px; background: #fff3cd; border: 1px solid #ffc107; border-left: 4px solid #e67e22; border-radius: 6px; padding: 14px 16px;",
            tags$strong(style = "color: #856404; display: block; margin-bottom: 6px;",
                        "⚠️ ADVERTENCIA: Saldo Inicial no encontrado"),
            tags$p(style = "font-size: 0.83rem; color: #6d5a00; margin: 0;",
                   "Este archivo no contiene la hoja 'Saldo Inicial'. Los saldos iniciales se estimaron
                    desde el campo 'Inicial del dia' de cada extracto, que corresponde al saldo al inicio
                    del ultimo dia descargado (no al inicio del periodo analizado)."),
            tags$p(style = "font-size: 0.83rem; color: #6d5a00; margin-top: 6px; font-weight: 600;",
                   "Para resultados correctos, procese el archivo .xlsm completo que incluye la hoja 'Saldo Inicial'.")
          )
        },
        tabsetPanel(
          tabPanel("Resumen",
                   br(),
                   div(class = "section-title", "Resumen por dia"),
                   div(class = "section-sub",
                       paste("Vista previa — primeros 50 registros de",
                             nrow(res$resumen), "totales")),
                   div(class = "table-wrap", renderTabla(head(res$resumen, 50)))
          ),
          tabPanel("Dias procesados",
                   br(),
                   div(class = "section-title", "Fechas detectadas"),
                   div(class = "section-sub", "Todos los dias con movimientos encontrados en el archivo"),
                   div(class = "table-wrap",
                       renderTabla(data.frame(Fecha = res$dias, stringsAsFactors = FALSE))
                   )
          )
        )
      )
    }
  })
}

# ----------------------------------------------------------------------------
# Funcion auxiliar para renderizar tablas HTML
# ----------------------------------------------------------------------------
renderTabla <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(div(class = "alerta-info", "Sin datos para mostrar."))
  }
  
  encabezados <- paste0(
    "<th>", htmltools::htmlEscape(names(df)), "</th>",
    collapse = ""
  )
  
  filas <- apply(df, 1, function(row) {
    celdas <- paste0(
      "<td>", htmltools::htmlEscape(ifelse(is.na(row), "", as.character(row))), "</td>",
      collapse = ""
    )
    paste0("<tr>", celdas, "</tr>")
  })
  
  HTML(paste0(
    '<table class="preview-table">',
    '<thead><tr>', encabezados, '</tr></thead>',
    '<tbody>', paste(filas, collapse = ""), '</tbody>',
    '</table>'
  ))
}

shinyApp(ui = ui, server = server)
