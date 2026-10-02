# =============================================================================
# ANÁLISIS ARQUETIPOS NHL — Shiny App
# Análisis multivariante de jugadores NHL (Temporada 2018-2019)
# =============================================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(plotly)
library(DT)
library(scales)
library(jpeg)
library(png)
library(grid)
library(FactoMineR)
library(factoextra)

# Cargar datos preprocesados si no están en memoria
if (file.exists("datos_app.RData")) {
  load("datos_app.RData")
}
# =============================================================================
# PALETA Y CONSTANTES GLOBALES
# =============================================================================

# mismos colores que en el informe (salvo el rosa para contrastar mejor con el gris) 
COLORES_CLUSTERS <- c(
  "1" = "#95A5A6", 
  "2" = "#52B788",
  "3" = "#A81E2C", 
  "4" = "#0077B6",
  "5" = "#C527F5",
  "6" = "#FFB703"
)

NOMBRES_ARQUETIPOS <- c(
  "1" = "Benchwarmers",
  "2" = "Esp. Bloqueadores",
  "3" = "Superestrellas",
  "4" = "Muros Defensivos",
  "5" = "Esp. Físicos",
  "6" = "Rotación / Two-Way"
)

DESCRIPCIONES_ARQUETIPOS <- c(
  "1" = "Jugadores de profundidad con escaso tiempo en hielo y producción estadística mínima. Representan la última línea de reserva de las plantillas.",
  "2" = "Defensores especializados en el bloqueo de tiros. Rol conservador, desplegados en situaciones de inferioridad numérica. Escasa aportación ofensiva.",
  "3" = "La élite ofensiva de la liga. Máximo tiempo en hielo, altos goles y asistencias. Jugadores franquicia que dominan el volumen de juego completo.",
  "4" = "Defensas de primera línea con altísima carga de minutos. Frenan a las estrellas rivales y lideran el juego físico. Pueden iniciar salidas de disco.",
  "5" = "Especialistas en el juego físico y contacto. Perfil híbrido (delanteros y defensas). Muchos hits con capacidad de penalización variable.",
  "6" = "Jugadores de rotación activa. Columna vertebral operativa que permite el descanso de las líneas principales. Perfil two-way equilibrado."
)

# mismos colores que el informe
COLORES_RADAR <- c(
  "Goles"        = "#A81E2C",
  "Asistencias"  = "#FFB703",
  "Tiros"        = "#0077B6",
  "Bloqueos"     = "#52B788",
  "Robos"        = "#BE95FF",
  "Hits"         = "#FF6B35",
  "Perdidas"     = "#8B4513",
  "Penalizaciones"= "#555555",
  "Minutos"      = "#1C3B5E"
)

# mismos colores que el informe
COLORES_POSICION <- c(
  "C"  = "#1b9e77",
  "D"  = "#d95f02",
  "LW" = "#7570b3",
  "RW" = "#e7298a"
)

# =============================================================================
# CSS PERSONALIZADO — ESTÉTICA NHL 
# =============================================================================

nhl_css <- "
/* ---- IMPORTACIÓN DE FUENTES ---- */
@import url('https://fonts.googleapis.com/css2?family=Barlow+Condensed:wght@400;600;700;900&family=Barlow:wght@300;400;500&display=swap');

/* ---- VARIABLES DE COLOR (HEMOS ESCOGIDO EL COLOR DE UNA PISTA DE HOCKEY) ---- */
:root {
  --nhl-black:   #F4F6F9; /* Fondo de la aplicación */
  --nhl-dark:    #FFFFFF; /* Fondo de las cajas */
  --nhl-panel:   #FFFFFF; /* Fondo de los paneles */
  --nhl-blue:    #1C3B5E; /* Azul Marino */
  --nhl-ice:     #E8F4FD;
  --nhl-red:     #A81E2C; /* Granate */
  --nhl-gold:    #FFB703;
  --nhl-border:  #D1D8E0;
  --nhl-text:    #1C3B5E; /* Texto principal */
  --nhl-muted:   #5A6B7C; /* Texto secundario */
  --radius:      6px;
}

/* ---- BODY / GLOBAL ---- */
body, .wrapper {
  background-color: var(--nhl-black) !important;
  font-family: 'Barlow', sans-serif !important;
  color: var(--nhl-text) !important;
}

/* ---- NAVBAR SUPERIOR ---- */
.main-header .logo {
  background-color: var(--nhl-red) !important;
  border-bottom: none !important;
  font-family: 'Barlow Condensed', sans-serif !important;
  font-weight: 900 !important;
  font-size: 18px !important;
  letter-spacing: 2px !important;
  color: white !important;
}
.main-header .navbar {
  background-color: #FFFFFF !important;
  border-bottom: 2px solid var(--nhl-red) !important;
}
.main-header .navbar .sidebar-toggle {
  color: var(--nhl-blue) !important;
}
.main-header .navbar .sidebar-toggle:hover {
  background-color: var(--nhl-ice) !important;
}

/* ---- SIDEBAR ---- */
.main-sidebar {
  background-color: var(--nhl-blue) !important;
  border-right: none !important;
}
.sidebar-menu > li > a {
  font-family: 'Barlow Condensed', sans-serif !important;
  font-weight: 600 !important;
  letter-spacing: 1px !important;
  color: #A9B7C6 !important;
  border-left: 3px solid transparent !important;
  transition: all 0.2s ease !important;
}
.sidebar-menu > li.active > a,
.sidebar-menu > li > a:hover {
  color: white !important;
  background-color: rgba(255, 255, 255, 0.05) !important;
  border-left: 3px solid var(--nhl-red) !important;
}
.sidebar-menu > li > a > .fa {
  color: #A9B7C6 !important;
  width: 20px;
}
.sidebar-menu > li.active > a > .fa {
  color: white !important;
}

/* ---- CONTENT AREA ---- */
.content-wrapper {
  background-color: var(--nhl-black) !important;
}
.content {
  padding: 20px !important;
}

/* ---- BOXES ---- */
.box {
  background-color: var(--nhl-panel) !important;
  border: 1px solid var(--nhl-border) !important;
  border-radius: var(--radius) !important;
  box-shadow: 0 2px 10px rgba(0,0,0,0.05) !important;
}
.box-header {
  background-color: #FFFFFF !important;
  border-bottom: 2px solid var(--nhl-red) !important;
  padding: 10px 15px !important;
}
.box-title {
  font-family: 'Barlow Condensed', sans-serif !important;
  font-weight: 700 !important;
  font-size: 15px !important;
  letter-spacing: 1.5px !important;
  text-transform: uppercase !important;
  color: var(--nhl-blue) !important;
}
.box-body {
  padding: 15px !important;
}

/* ---- VALUE BOXES ---- */
.small-box {
  border-radius: var(--radius) !important;
  border: none !important;
  box-shadow: 0 2px 10px rgba(0,0,0,0.1) !important;
  position: relative !important;
}
.small-box > .inner {
  padding: 12px 15px !important;
}
.small-box > .inner h3 {
  font-family: 'Barlow Condensed', sans-serif !important;
  font-weight: 900 !important;
  font-size: 28px !important;
  z-index: 5 !important;
  position: relative !important;
  color: white !important;
}
.small-box > .inner p {
  font-family: 'Barlow Condensed', sans-serif !important;
  font-weight: 600 !important;
  letter-spacing: 1px !important;
  font-size: 13px !important;
  z-index: 5 !important;
  position: relative !important;
  color: white !important;
}
.small-box > .icon { 
  opacity: 0.25 !important; 
  position: absolute !important;
  top: 50% !important;
  transform: translateY(-50%) !important;
  right: 20px !important;
  transition: all 0.3s linear !important;
  color: white !important;
}
.small-box:hover .icon {
  transform: translateY(-50%) scale(1.1) !important; 
}

/* ---- SELECTIZE / SELECT INPUTS ---- */
.selectize-input, .form-control {
  background-color: #FFFFFF !important;
  border: 1px solid var(--nhl-border) !important;
  color: var(--nhl-text) !important;
  border-radius: var(--radius) !important;
  font-family: 'Barlow', sans-serif !important;
}
.selectize-dropdown {
  background-color: #FFFFFF !important;
  border: 1px solid var(--nhl-blue) !important;
  color: var(--nhl-text) !important;
}
.selectize-dropdown-content .option:hover,
.selectize-dropdown-content .active {
  background-color: var(--nhl-ice) !important;
  color: var(--nhl-blue) !important;
}
label {
  font-family: 'Barlow Condensed', sans-serif !important;
  font-weight: 600 !important;
  letter-spacing: 1px !important;
  text-transform: uppercase !important;
  font-size: 12px !important;
  color: var(--nhl-muted) !important;
}

/* ---- SLIDER ---- */
.irs-bar, .irs-bar-edge { background: var(--nhl-blue) !important; border-color: var(--nhl-blue) !important; }
.irs-handle { border-color: var(--nhl-blue) !important; }
.irs-from, .irs-to, .irs-single { background: var(--nhl-blue) !important; }
.irs-grid-text { color: var(--nhl-muted) !important; }
.irs-line { background: var(--nhl-border) !important; }

/* ---- DATATABLE ---- */
.dataTables_wrapper,
table.dataTable {
  background-color: var(--nhl-panel) !important;
  color: var(--nhl-text) !important;
  font-family: 'Barlow', sans-serif !important;
  font-size: 13px !important;
}
table.dataTable thead th {
  background-color: var(--nhl-blue) !important;
  color: white !important;
  font-family: 'Barlow Condensed', sans-serif !important;
  font-weight: 700 !important;
  letter-spacing: 1px !important;
  text-transform: uppercase !important;
  border-bottom: 2px solid var(--nhl-red) !important;
}
table.dataTable tbody tr {
  background-color: #FFFFFF !important;
  color: var(--nhl-text) !important;
}
table.dataTable tbody tr:hover {
  background-color: var(--nhl-ice) !important;
}
table.dataTable tbody tr.selected {
  background-color: var(--nhl-ice) !important;
}
.dataTables_info, .dataTables_paginate, .dataTables_filter, .dataTables_length {
  color: var(--nhl-muted) !important;
}
.dataTables_filter input, .dataTables_length select {
  background-color: #FFFFFF !important;
  border: 1px solid var(--nhl-border) !important;
  color: var(--nhl-text) !important;
  border-radius: var(--radius) !important;
}
.paginate_button {
  color: var(--nhl-muted) !important;
}
.paginate_button.current, .paginate_button.current:hover {
  background: var(--nhl-blue) !important;
  color: white !important;
  border-color: var(--nhl-blue) !important;
}

/* ---- PLOTLY ---- */
.plotly .modebar { background: transparent !important; }
.plotly .modebar-btn path { fill: var(--nhl-muted) !important; }

/* ---- ARQUETIPO BADGE ---- */
.arquetipo-badge {
  display: inline-block;
  padding: 4px 12px;
  border-radius: 20px;
  font-family: 'Barlow Condensed', sans-serif;
  font-weight: 700;
  font-size: 13px;
  letter-spacing: 1px;
  text-transform: uppercase;
}

/* ---- STAT CARD (para el explorador de jugadores) ---- */
.stat-card {
  background: #F4F6F9;
  border: 1px solid var(--nhl-border);
  border-radius: var(--radius);
  padding: 12px 15px;
  text-align: center;
  margin-bottom: 8px;
}
.stat-card .stat-val {
  font-family: 'Barlow Condensed', sans-serif;
  font-weight: 900;
  font-size: 26px;
  color: var(--nhl-blue);
  display: block;
}
.stat-card .stat-lbl {
  font-family: 'Barlow Condensed', sans-serif;
  font-weight: 600;
  font-size: 11px;
  letter-spacing: 1.5px;
  text-transform: uppercase;
  color: var(--nhl-muted);
  display: block;
  margin-top: 2px;
}

/* ---- HEADER DE PESTAÑA ACTIVO ---- */
.nav-tabs-custom > .nav-tabs > li.active > a {
  border-top: 3px solid var(--nhl-red) !important;
  background-color: #FFFFFF !important;
  color: var(--nhl-blue) !important;
}
.nav-tabs-custom > .nav-tabs > li > a {
  color: var(--nhl-muted) !important;
  background-color: #F4F6F9 !important;
}
.nav-tabs-custom { background: var(--nhl-panel) !important; box-shadow: none !important;}

/* ---- SCROLLBAR ---- */
::-webkit-scrollbar { width: 6px; height: 6px; }
::-webkit-scrollbar-track { background: var(--nhl-black); }
::-webkit-scrollbar-thumb { background: var(--nhl-border); border-radius: 3px; }
::-webkit-scrollbar-thumb:hover { background: var(--nhl-blue); }

/* ---- DESCRIPCIÓN ARQUETIPO ---- */
.desc-box {
  background: var(--nhl-ice);
  border-left: 3px solid var(--nhl-red);
  padding: 12px 16px;
  border-radius: 0 var(--radius) var(--radius) 0;
  font-size: 13px;
  color: var(--nhl-text);
  line-height: 1.6;
  margin-top: 8px;
}

/* ---- RADIO BUTTONS ---- */
.radio label, .checkbox label {
  color: var(--nhl-text) !important;
  font-size: 13px !important;
  text-transform: none !important;
  letter-spacing: 0 !important;
  font-weight: 400 !important;
}

/* ---- TITLE STRIP en la parte superior ---- */
.content-header {
  background: #FFFFFF !important;
  border-bottom: 1px solid var(--nhl-border) !important;
  padding: 10px 20px !important;
}
.content-header h1 {
  font-family: 'Barlow Condensed', sans-serif !important;
  font-weight: 900 !important;
  letter-spacing: 2px !important;
  font-size: 20px !important;
  color: var(--nhl-blue) !important;
}
.content-header .breadcrumb {
  background: transparent !important;
}
.content-header .breadcrumb > li + li::before {
  color: var(--nhl-muted) !important;
}
.content-header .breadcrumb > .active {
  color: var(--nhl-red) !important;
}

/* ---- ACCIONES BOTONES ---- */
.btn-primary {
  background-color: var(--nhl-red) !important;
  border-color: var(--nhl-red) !important;
  font-family: 'Barlow Condensed', sans-serif !important;
  font-weight: 700 !important;
  letter-spacing: 1px !important;
}
.btn-primary:hover {
  background-color: #8c1723 !important;
}
"

# =============================================================================
# FUNCIONES AUXILIARES
# =============================================================================

# Tema ggplot estilo NHL Claro
tema_nhl <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background    = element_rect(fill = "#FFFFFF", color = NA),
      panel.background   = element_rect(fill = "#FFFFFF", color = NA),
      panel.grid.major   = element_line(color = "#E0E4E8", linewidth = 0.5),
      panel.grid.minor   = element_line(color = "#F4F6F9", linewidth = 0.3),
      panel.border       = element_blank(),
      axis.text          = element_text(color = "#5A6B7C", family = "sans", size = 10),
      axis.title         = element_text(color = "#1C3B5E", family = "sans", size = 11, face = "bold"),
      plot.title         = element_text(color = "#1C3B5E", family = "sans", size = 14, face = "bold", margin = margin(b = 4)),
      plot.subtitle      = element_text(color = "#5A6B7C", family = "sans", size = 11, margin = margin(b = 10)),
      plot.caption       = element_text(color = "#5A6B7C", family = "sans", size = 9),
      legend.background  = element_rect(fill = "#FFFFFF", color = "#D1D8E0"),
      legend.text        = element_text(color = "#1C3B5E", family = "sans", size = 10),
      legend.title       = element_text(color = "#1C3B5E", family = "sans", size = 11, face = "bold"),
      strip.background   = element_rect(fill = "#FFFFFF", color = "#D1D8E0"),
      strip.text         = element_text(color = "#1C3B5E", family = "sans", size = 10, face = "bold"),
      plot.margin        = margin(15, 15, 10, 15)
    )
}

# Etiquetas de arquetipos con número
etiqueta_arquetipo <- function(id) {
  paste0("C", id, " · ", NOMBRES_ARQUETIPOS[as.character(id)])
}

# Color deL fondo para badge de arquetipo 
color_badge_bg <- function(hex) {
  paste0(hex, "33") 
}

# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  skin = "black",
  
  # ---- HEADER ----
  dashboardHeader(
    title = tags$span(
      tags$img(src = "NHL.png",
               height = "30px", style = "margin-right:8px; vertical-align:middle;"),
      "ANÁLISIS ARQUETIPOS NHL"
    ),
    titleWidth = 300
  ),
  
  # ---- SIDEBAR ----
  dashboardSidebar(
    width = 300,
    tags$div(
      style = "padding: 15px 15px 5px; color: #A9B7C6; font-size: 10px; letter-spacing: 2px; text-transform: uppercase;",
      "Temporada 2018-19"
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("Vista General",      tabName = "overview",  icon = icon("chart-bar")),
      menuItem("Explorador",         tabName = "explorer",  icon = icon("search")),
      menuItem("Análisis PCA",       tabName = "pca",       icon = icon("project-diagram")),
      menuItem("Arquetipos",         tabName = "clusters",  icon = icon("users")),
      menuItem("Análisis de Equipos",tabName = "teams",     icon = icon("hockey-puck")),
      menuItem("Market Finder",      tabName = "market",    icon = icon("filter"))
    ),
    tags$hr(style = "border-color: rgba(255,255,255,0.1); margin: 10px 15px;"),
    tags$div(
      style = "padding: 5px 15px 10px;",
      tags$p(style = "color:#A9B7C6; font-size:10px; letter-spacing:1px; text-transform:uppercase; margin-bottom:6px;",
             "Leyenda de arquetipos"),
      lapply(1:6, function(i) {
        tags$div(
          style = paste0("display:flex; align-items:center; margin-bottom:5px;"),
          tags$div(style = paste0("width:10px; height:10px; border-radius:50%; background:",
                                  COLORES_CLUSTERS[as.character(i)], "; margin-right:8px; flex-shrink:0;")),
          tags$span(style = "color:#FFFFFF; font-size:11px;",
                    paste0("C", i, " · ", NOMBRES_ARQUETIPOS[as.character(i)]))
        )
      })
    )
  ),
  
  # ---- BODY ----
  dashboardBody(
    tags$head(tags$style(HTML(nhl_css))),
    
    tabItems(
      
      # =======================================================================
      # TAB 1: VISTA GENERAL
      # =======================================================================
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("vb_jugadores",  width = 3),
                valueBoxOutput("vb_equipos",    width = 3),
                valueBoxOutput("vb_partidos",   width = 3),
                valueBoxOutput("vb_estrellas",  width = 3)
              ),
              fluidRow(
                box(
                  title = "DISTRIBUCIÓN DE ARQUETIPOS EN LA LIGA",
                  width = 7, solidHeader = TRUE,
                  plotlyOutput("plot_liga_dist", height = "350px")
                ),
                box(
                  title = "COMPOSICIÓN POR POSICIÓN",
                  width = 5, solidHeader = TRUE,
                  plotlyOutput("plot_posicion_cluster", height = "350px")
                )
              ),
              fluidRow(
                box(
                  title = "DISTRIBUCIÓN DE MÉTRICAS CLAVE POR PARTIDO",
                  width = 12, solidHeader = TRUE,
                  radioButtons("var_histograma", label = "Selecciona variable:",
                               choices = c("Goles" = "goals", "Asistencias" = "assists",
                                           "Tiros" = "shots", "Hits" = "hits",
                                           "Bloqueos" = "blocked", "Tiempo en Hielo (seg)" = "timeOnIce",
                                           "±" = "plusMinus", "Penalizaciones" = "penaltyMinutes",
                                           "Robos" = "takeaways", "Pérdidas" = "giveaways"),
                               inline = TRUE),
                  plotlyOutput("plot_histograma", height = "280px")
                )
              )
      ),
      
      # =======================================================================
      # TAB 2: EXPLORADOR DE JUGADORES
      # =======================================================================
      tabItem(tabName = "explorer",
              fluidRow(
                box(
                  title = "BÚSQUEDA DE JUGADOR", width = 4, solidHeader = TRUE,
                  selectizeInput("sel_jugador", "Jugador", choices = NULL,
                                 options = list(placeholder = "Escribe un nombre...")),
                  tags$hr(style = "border-color:#D1D8E0;"),
                  uiOutput("ui_ficha_jugador")
                ),
                box(
                  title = "RADAR DE RENDIMIENTO", width = 4, solidHeader = TRUE,
                  plotlyOutput("plot_radar_jugador", height = "380px")
                ),
                box(
                  title = "POSICIÓN EN EL ESPACIO PCA", width = 4, solidHeader = TRUE,
                  plotlyOutput("plot_pca_jugador", height = "380px")
                )
              ),
              fluidRow(
                box(
                  title = "TODOS LOS JUGADORES — TABLA COMPLETA", width = 12, solidHeader = TRUE,
                  DTOutput("tabla_jugadores")
                )
              )
      ),
      
      # =======================================================================
      # TAB 3: ANÁLISIS PCA
      # =======================================================================
      tabItem(tabName = "pca",
              fluidRow(
                box(
                  title = "BIPLOT INTERACTIVO", width = 8, solidHeader = TRUE,
                  fluidRow(
                    column(4, selectInput("pca_color_by", "Colorear por:",
                                          choices = c("Arquetipo" = "Cluster_Final",
                                                      "Posición" = "primaryPosition"))),
                    column(4, selectInput("pca_ejeX", "Eje X:",
                                          choices = c("PC1 — Volumen/Estrella" = "PC1",
                                                      "PC2 — Ofensivo vs Defensivo" = "PC2",
                                                      "PC3 — Agresividad" = "PC3",
                                                      "PC4 — Eficacia" = "PC4"),
                                          selected = "PC1")),
                    column(4, selectInput("pca_ejeY", "Eje Y:",
                                          choices = c("PC1 — Volumen/Estrella" = "PC1",
                                                      "PC2 — Ofensivo vs Defensivo" = "PC2",
                                                      "PC3 — Agresividad" = "PC3",
                                                      "PC4 — Eficacia" = "PC4"),
                                          selected = "PC2"))
                  ),
                  plotlyOutput("plot_biplot", height = "430px")
                ),
                box(
                  title = "HEATMAP DE LOADINGS", width = 4, solidHeader = TRUE,
                  plotlyOutput("plot_loadings", height = "490px")
                )
              ),
              fluidRow(
                box(
                  title = "VARIANZA EXPLICADA — MÉTODO DEL CODO", width = 6, solidHeader = TRUE,
                  plotlyOutput("plot_varianza", height = "280px")
                ),
                box(
                  title = "INTERPRETACIÓN DE COMPONENTES", width = 6, solidHeader = TRUE,
                  tags$div(style = "overflow-y:auto; max-height:300px; padding-right:5px;",
                           tags$div(class = "desc-box", style = "margin-bottom:8px;",
                                    tags$strong(style = "color:var(--nhl-blue);", "PC1 · Perfil de Estrella / Volumen (39.03%)"),
                                    tags$br(),
                                    "Define el estatus del jugador: estrellas con mucho tiempo en hielo y generación
                 de peligro constante vs. suplentes con poco impacto estadístico."
                           ),
                           tags$div(class = "desc-box", style = "margin-bottom:8px;",
                                    tags$strong(style = "color:var(--nhl-blue);", "PC2 · Especialista Defensivo (19.68%)"),
                                    tags$br(),
                                    "Eje Ataque vs. Defensa. Valores negativos = muros (bloquean y chocan).
                 Valores positivos = delanteros buscadores de portería."
                           ),
                           tags$div(class = "desc-box", style = "margin-bottom:8px;",
                                    tags$strong(style = "color:var(--nhl-blue);", "PC3 · Agresividad Disciplinaria (13.16%)"),
                                    tags$br(),
                                    "Define la dureza del jugador. Más negativo = más tiempo en caja de castigo y más golpes."
                           ),
                           tags$div(class = "desc-box",
                                    tags$strong(style = "color:var(--nhl-blue);", "PC4 · Factor Eficacia (9.55%)"),
                                    tags$br(),
                                    "Componente casi pura de rendimiento colectivo (plusMinus).
                 Separa a los jugadores que están en hielo cuando su equipo marca o encaja."
                           )
                  )
                )
              )
      ),
      
      # =======================================================================
      # TAB 4: ARQUETIPOS
      # =======================================================================
      tabItem(tabName = "clusters",
              fluidRow(
                box(
                  title = "SELECCIONA UN ARQUETIPO", width = 4, solidHeader = TRUE,
                  selectInput("sel_cluster", "Arquetipo:",
                              choices = setNames(1:6, sapply(1:6, etiqueta_arquetipo))),
                  uiOutput("ui_desc_cluster"),
                  tags$hr(style = "border-color:#D1D8E0;"),
                  tags$p(style = "color:#5A6B7C; font-size:11px; letter-spacing:1px; text-transform:uppercase; margin-bottom:6px;",
                         "Jugadores más representativos"),
                  DTOutput("tabla_top5_cluster")
                ),
                box(
                  title = "RADAR DEL ARQUETIPO", width = 4, solidHeader = TRUE,
                  plotlyOutput("plot_radar_cluster", height = "360px")
                ),
                box(
                  title = "TOP 5 REPRESENTATIVOS — RADARES", width = 4, solidHeader = TRUE,
                  plotlyOutput("plot_radar_top5", height = "360px")
                )
              ),
              fluidRow(
                box(
                  title = "COMPARATIVA DE ARQUETIPOS — MÉTRICAS MEDIAS", width = 8, solidHeader = TRUE,
                  selectInput("var_comparativa", "Variable a comparar:",
                              choices = c("Goles" = "goals", "Asistencias" = "assists",
                                          "Tiros" = "shots", "Hits" = "hits",
                                          "Bloqueos" = "blocked", "Tiempo en Hielo" = "timeOnIce",
                                          "±" = "plusMinus", "Penalizaciones" = "penaltyMinutes",
                                          "Robos" = "takeaways", "Pérdidas" = "giveaways")),
                  plotlyOutput("plot_comparativa_clusters", height = "280px")
                ),
                box(
                  title = "COMPOSICIÓN POR POSICIÓN (NORMALIZADA)", width = 4, solidHeader = TRUE,
                  plotlyOutput("plot_posicion_norm", height = "340px")
                )
              )
      ),
      
      # =======================================================================
      # TAB 5: ANÁLISIS DE EQUIPOS
      # =======================================================================
      tabItem(tabName = "teams",
              fluidRow(
                box(
                  title = "COMPARADOR DE EQUIPOS", width = 5, solidHeader = TRUE,
                  selectInput("sel_equipo1", "Equipo A:", choices = NULL),
                  selectInput("sel_equipo2", "Equipo B (opcional):", choices = NULL),
                  tags$hr(style = "border-color:#D1D8E0;"),
                  plotlyOutput("plot_equipo_comp", height = "340px")
                ),
                box(
                  title = "RANKING DE EQUIPOS POR ARQUETIPO", width = 7, solidHeader = TRUE,
                  fluidRow(
                    column(6, selectInput("ranking_cluster", "Arquetipo:",
                                          choices = setNames(1:6, sapply(1:6, etiqueta_arquetipo)))),
                    column(6, radioButtons("ranking_dir", "Orden:",
                                           choices = c("Mayor %" = "desc", "Menor %" = "asc"),
                                           inline = TRUE))
                  ),
                  plotlyOutput("plot_ranking_equipos", height = "370px")
                )
              ),
              fluidRow(
                box(
                  title = "CARA A CARA: LÍDERES EN HIELO", width = 12, solidHeader = TRUE,
                  
                  tags$div(style = "display: flex; justify-content: center; margin-bottom: 10px;",
                           tags$div(style = "width: 300px;",
                                    selectInput("sel_arquetipo_vs", "Selecciona el Arquetipo a comparar:",
                                                choices = setNames(1:6, sapply(1:6, etiqueta_arquetipo)))
                           )
                  ),
                  
                  tags$p(style = "color:#5A6B7C; font-size:13px; margin-bottom:15px; text-align:center;",
                         "Comparativa directa entre los jugadores con más minutos promedio del arquetipo seleccionado."),
                  uiOutput("ui_comparativa_lideres")
                )
              )
      ),
      
      # =======================================================================
      # TAB 6: MARKET FINDER
      # =======================================================================
      tabItem(tabName = "market",
              fluidRow(
                box(
                  title = "FILTROS DE RENDIMIENTO", width = 3, solidHeader = TRUE,
                  tags$p(style = "color:#5A6B7C;", "Define los umbrales para encontrar perfiles específicos."),
                  sliderInput("f_goals", "Goles por partido:", 0, 1, c(0, 1), step = 0.05),
                  sliderInput("f_assists", "Asistencias por partido:", 0, 1.2, c(0, 1.2), step = 0.05),
                  sliderInput("f_shots", "Tiros por partido:", 0, 5, c(0, 5), step = 0.1),
                  sliderInput("f_hits", "Hits por partido:", 0, 5, c(0, 5), step = 0.1),
                  sliderInput("f_blocked", "Bloqueos por partido:", 0, 4, c(0, 4), step = 0.1),
                  sliderInput("f_takeaways", "Robos (Takeaways):", 0, 2, c(0, 2), step = 0.1),
                  sliderInput("f_giveaways", "Pérdidas (Giveaways):", 0, 2, c(0, 2), step = 0.1),
                  sliderInput("f_penalties", "Min. Penalización:", 0, 5, c(0, 5), step = 0.1),
                  sliderInput("f_toi", "Minutos por partido:", 0, 30, c(0, 30), step = 1),
                  selectInput("f_pos", "Posición:", choices = c("Todas", "C", "D", "LW", "RW"), selected = "Todas"),
                  hr(style="border-color:#D1D8E0;"),
                  actionButton("reset_filters", "Limpiar Filtros", class = "btn-block")
                ),
                box(
                  title = "JUGADORES QUE CUMPLEN EL PERFIL", width = 9, solidHeader = TRUE,
                  DTOutput("tabla_market")
                )
              )
      )
    ) 
  ) 
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  datos_jugadores <- reactive({
    req(exists("data_jugadores_con_clusters"))
    df <- data_jugadores_con_clusters
    df$Cluster_Final <- as.factor(df$Cluster_Final)
    df$nombre_completo <- paste(df$firstName, df$lastName)
    df
  })
  
  datos_equipo_cluster <- reactive({
    req(exists("data_jugadores_equipo_cluster"))
    df <- data_jugadores_equipo_cluster
    df$Cluster_Final <- as.factor(df$Cluster_Final)
    df
  })
  
  datos_pca <- reactive({
    req(exists("datos_para_clustering"))
    as.data.frame(datos_para_clustering)
  })
  
  modelo_km <- reactive({
    req(exists("km_clusters"))
    km_clusters
  })
  
  modelo_pca <- reactive({
    req(exists("pca2"))
    pca2
  })
  
  datos_norm <- reactive({
    req(exists("data_norm_global"))
    df <- data_norm_global
    df$Cluster_Final <- as.factor(df$Cluster_Final)
    df
  })
  
  resumen_radar <- reactive({
    req(exists("resumen_radar_limpio"))
    resumen_radar_limpio
  })
  
  observe({
    dj <- datos_jugadores()
    jugadores <- sort(unique(dj$nombre_completo))
    updateSelectizeInput(session, "sel_jugador",
                         choices = jugadores,
                         selected = jugadores[1],
                         server = TRUE)
  })
  
  observe({
    dec <- datos_equipo_cluster()
    equipos <- sort(unique(dec$teamName))
    opciones_b <- c("(Ninguno)" = "", equipos)
    updateSelectInput(session, "sel_equipo1", choices = equipos, selected = equipos[1])
    updateSelectInput(session, "sel_equipo2", choices = opciones_b, selected = "")
  })
  
  output$vb_jugadores <- renderValueBox({
    valueBox(nrow(datos_jugadores()), "JUGADORES ANALIZADOS", icon = icon("user"), color = "navy")
  })
  output$vb_equipos <- renderValueBox({
    valueBox(length(unique(datos_equipo_cluster()$teamName)), "FRANQUICIAS", icon = icon("building"), color = "light-blue")
  })
  output$vb_partidos <- renderValueBox({
    valueBox("82", "PARTIDOS POR EQUIPO", icon = icon("calendar"), color = "blue")
  })
  output$vb_estrellas <- renderValueBox({
    n <- sum(datos_jugadores()$Cluster_Final == "3")
    valueBox(n, "SUPERESTRELLAS (C3)", icon = icon("star"), color = "red")
  })
  
  output$plot_liga_dist <- renderPlotly({
    liga_plot <- datos_jugadores() %>%
      group_by(Cluster_Final) %>% tally() %>%
      mutate(
        pct = n / sum(n) * 100,
        nombre = NOMBRES_ARQUETIPOS[as.character(Cluster_Final)],
        color  = COLORES_CLUSTERS[as.character(Cluster_Final)],
        label  = paste0("C", Cluster_Final, "<br>", nombre, "<br><b>", round(pct, 1), "%</b><br>", n, " jugadores")
      )
    
    plot_ly(liga_plot, x = ~paste0("C", Cluster_Final), y = ~pct, type = "bar",
            marker = list(color = ~color, line = list(color = "#FFFFFF", width = 1.5)),
            text = ~label, hoverinfo = "text", textposition = "outside",
            texttemplate = "<b>%{y:.1f}%</b>", textfont = list(color = "#1C3B5E", size = 11)) %>%
      layout(
        paper_bgcolor = "#FFFFFF", plot_bgcolor  = "#FFFFFF",
        xaxis = list(title = "Arquetipo", color = "#1C3B5E", tickfont = list(color = "#5A6B7C")),
        yaxis = list(title = "% de Jugadores", color = "#1C3B5E", tickfont = list(color = "#5A6B7C"), gridcolor = "#E0E4E8"),
        font  = list(color = "#1C3B5E"), showlegend = FALSE
      )
  })
  
  output$plot_posicion_cluster <- renderPlotly({
    pos_plot <- datos_jugadores() %>%
      filter(primaryPosition %in% c("C", "LW", "RW", "D")) %>%
      group_by(Cluster_Final, primaryPosition) %>% tally() %>%
      group_by(Cluster_Final) %>% mutate(pct = n / sum(n) * 100)
    
    plot_ly(pos_plot, x = ~paste0("C", Cluster_Final), y = ~pct, color = ~primaryPosition,
            colors = COLORES_POSICION, type = "bar",
            hovertemplate = "<b>%{x}</b><br>%{fullData.name}: %{y:.1f}%<extra></extra>") %>%
      layout(
        barmode = "stack", paper_bgcolor = "#FFFFFF", plot_bgcolor  = "#FFFFFF",
        xaxis = list(title = "Arquetipo", color = "#1C3B5E", tickfont = list(color = "#5A6B7C")),
        yaxis = list(title = "% Posición", color = "#1C3B5E", tickfont = list(color = "#5A6B7C"), gridcolor = "#E0E4E8"),
        legend = list(font = list(color = "#1C3B5E")), font = list(color = "#1C3B5E")
      )
  })
  
  output$plot_histograma <- renderPlotly({
    var <- input$var_histograma
    vals <- datos_jugadores()[[var]]
    titulo_var <- names(which(c("goals"="Goles","assists"="Asistencias","shots"="Tiros",
                                "hits"="Hits","blocked"="Bloqueos","timeOnIce"="Tiempo en Hielo (seg)",
                                "plusMinus"="±","penaltyMinutes"="Penalizaciones",
                                "takeaways"="Robos","giveaways"="Pérdidas") == var))
    
    plot_ly(x = vals, type = "histogram", nbinsx = 30,
            marker = list(color = "#0077B6", line = list(color = "#FFFFFF", width = 0.5)),
            hovertemplate = "Valor: %{x}<br>Frecuencia: %{y}<extra></extra>") %>%
      layout(
        title = list(text = paste("Distribución de", titulo_var), font = list(color = "#1C3B5E")),
        paper_bgcolor = "#FFFFFF", plot_bgcolor  = "#FFFFFF",
        xaxis = list(title = titulo_var, color = "#1C3B5E", tickfont = list(color = "#5A6B7C"), gridcolor = "#E0E4E8"),
        yaxis = list(title = "Frecuencia", color = "#1C3B5E", tickfont = list(color = "#5A6B7C"), gridcolor = "#E0E4E8"),
        font = list(color = "#1C3B5E")
      )
  })
  
  jugador_sel <- reactive({
    req(input$sel_jugador)
    datos_jugadores() %>% filter(nombre_completo == input$sel_jugador) %>% slice(1)
  })
  
  output$ui_ficha_jugador <- renderUI({
    j <- jugador_sel()
    req(nrow(j) > 0)
    cl  <- as.character(j$Cluster_Final)
    col <- COLORES_CLUSTERS[cl]
    nom <- NOMBRES_ARQUETIPOS[cl]
    
    tagList(
      tags$div(
        style = "text-align:center; margin-bottom:15px;",
        tags$h3(style = "color:var(--nhl-blue); margin:0; font-size:20px; font-weight:900;", j$nombre_completo),
        tags$div(
          style = paste0("display:inline-block; margin-top:6px; padding:4px 14px; border-radius:20px;",
                         "background:", col, "33; border:1px solid ", col, ";",
                         "color:", col, "; font-size:12px; font-weight:700; letter-spacing:1px;"),
          paste0("C", cl, " · ", nom)
        )
      ),
      fluidRow(
        column(6, tags$div(class = "stat-card", tags$span(class = "stat-val", j$primaryPosition), tags$span(class = "stat-lbl", "Posición"))),
        column(6, tags$div(class = "stat-card", tags$span(class = "stat-val", j$partidos_jugados), tags$span(class = "stat-lbl", "Partidos")))
      ),
      fluidRow(
        column(6, tags$div(class = "stat-card", tags$span(class = "stat-val", round(j$goals, 2)), tags$span(class = "stat-lbl", "Goles/partido"))),
        column(6, tags$div(class = "stat-card", tags$span(class = "stat-val", round(j$assists, 2)), tags$span(class = "stat-lbl", "Asist./partido")))
      ),
      fluidRow(
        column(6, tags$div(class = "stat-card", tags$span(class = "stat-val", round(j$timeOnIce / 60, 1)), tags$span(class = "stat-lbl", "Min./partido"))),
        column(6, tags$div(class = "stat-card", tags$span(class = "stat-val", round(j$plusMinus, 2)), tags$span(class = "stat-lbl", "Plus/Minus")))
      )
    )
  })
  
  output$plot_radar_jugador <- renderPlotly({
    j <- jugador_sel()
    req(nrow(j) > 0)
    dn <- datos_norm()
    j_norm <- dn %>% filter(firstName == j$firstName, lastName == j$lastName) %>% slice(1)
    
    if (nrow(j_norm) == 0) {
      return(plotly_empty() %>% layout(paper_bgcolor = "#FFFFFF", annotations = list(text = "Datos normalizados no disponibles", showarrow = FALSE, font = list(color = "#1C3B5E"))))
    }
    
    vars <- c("Goles" = "goals", "Asistencias" = "assists", "Tiros" = "shots",
              "Pérdidas" = "giveaways", "Bloqueos" = "blocked", "Hits" = "hits",
              "Robos" = "takeaways", "Penaliz." = "penaltyMinutes", "Minutos" = "timeOnIce")
    
    vals <- sapply(vars, function(v) {
      val <- j_norm[[v]]
      if (is.null(val) || length(val) == 0) 0 else as.numeric(val[1])
    })
    
    cl  <- as.character(j$Cluster_Final)
    col <- COLORES_CLUSTERS[cl]
    
    plot_ly(
      type = "scatterpolar", r = c(vals, vals[1]), theta = c(names(vars), names(vars)[1]),
      fill = "toself", fillcolor = paste0(col, "40"), line = list(color = col, width = 2),
      mode = "lines+markers", marker = list(color = col, size = 6),
      hovertemplate = "<b>%{theta}</b>: %{r:.3f}<extra></extra>"
    ) %>%
      layout(
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, 1), color = "#5A6B7C", gridcolor = "#E0E4E8"),
          angularaxis = list(color = "#1C3B5E", direction = "clockwise", rotation = 70)
        ),
        paper_bgcolor = "#FFFFFF", plot_bgcolor  = "#FFFFFF", font = list(color = "#1C3B5E"),
        showlegend = FALSE, margin = list(l = 50, r = 60, t = 40, b = 40) 
      )
  })
  
  output$plot_pca_jugador <- renderPlotly({
    j <- jugador_sel()
    req(nrow(j) > 0)
    dj <- datos_jugadores()
    if (!all(c("PC1","PC2") %in% colnames(dj))) return(plotly_empty())
    
    df_plot <- dj %>%
      mutate(
        color = COLORES_CLUSTERS[as.character(Cluster_Final)],
        es_jugador = (nombre_completo == j$nombre_completo),
        tooltip = paste0("<b>", nombre_completo, "</b><br>Arquetipo: C", Cluster_Final, " · ", NOMBRES_ARQUETIPOS[as.character(Cluster_Final)], "<br>Posición: ", primaryPosition, "<br>PC1: ", round(PC1, 2), "<br>PC2: ", round(PC2, 2))
      )
    
    otros  <- df_plot %>% filter(!es_jugador)
    selec  <- df_plot %>% filter(es_jugador)
    
    plot_ly() %>%
      add_trace(data = otros, x = ~PC1, y = ~PC2, type = "scatter", mode = "markers",
                marker = list(color = ~color, size = 5, opacity = 0.7), text = ~tooltip, hoverinfo = "text", showlegend = FALSE) %>%
      add_trace(data = selec, x = ~PC1, y = ~PC2, type = "scatter", mode = "markers",
                marker = list(color = "#FFB703", size = 14, symbol = "star", line = list(color = "#1C3B5E", width = 2)),
                text = ~tooltip, hoverinfo = "text", name = j$nombre_completo) %>%
      layout(
        xaxis = list(title = "PC1 — Volumen/Estrella", color = "#1C3B5E", tickfont = list(color = "#5A6B7C"), gridcolor = "#E0E4E8", zeroline = FALSE),
        yaxis = list(title = "PC2 — Ofensivo vs Defensivo", color = "#1C3B5E", tickfont = list(color = "#5A6B7C"), gridcolor = "#E0E4E8", zeroline = FALSE),
        paper_bgcolor = "#FFFFFF", plot_bgcolor  = "#FFFFFF", font = list(color = "#1C3B5E"), legend = list(font = list(color = "#1C3B5E"))
      )
  })
  
  output$tabla_jugadores <- renderDT({
    dj <- datos_jugadores() %>%
      mutate(Arquetipo = paste0("C", Cluster_Final, " · ", NOMBRES_ARQUETIPOS[as.character(Cluster_Final)])) %>%
      select(Jugador = nombre_completo, Posición = primaryPosition, Arquetipo, Partidos = partidos_jugados, Goles = goals, Asistencias = assists, Tiros = shots, Hits = hits, Bloqueos = blocked, Robos = takeaways, Pérdidas = giveaways, `±` = plusMinus, `Min/partido` = timeOnIce) %>%
      mutate(across(where(is.numeric), ~round(., 2)), `Min/partido` = round(`Min/partido` / 60, 1))
    
    datatable(dj, options = list(pageLength = 15, scrollX = TRUE, dom = "lfrtip", language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ jugadores", info = "Mostrando _START_-_END_ de _TOTAL_ jugadores")), rownames = FALSE, selection = "single") %>%
      formatStyle(columns = colnames(dj), color = "#1C3B5E", backgroundColor = "#FFFFFF")
  })
  
  output$plot_biplot <- renderPlotly({
    dj  <- datos_jugadores()
    req(all(c("PC1","PC2","PC3","PC4") %in% colnames(dj)))
    ejeX <- input$pca_ejeX
    ejeY <- input$pca_ejeY
    color_by <- input$pca_color_by
    
    df_plot <- dj %>%
      mutate(
        xval = .data[[ejeX]], yval = .data[[ejeY]],
        color_var = as.character(.data[[color_by]]),
        tooltip = paste0("<b>", nombre_completo, "</b><br>C", Cluster_Final, " · ", NOMBRES_ARQUETIPOS[as.character(Cluster_Final)], "<br>Posición: ", primaryPosition, "<br>", ejeX, ": ", round(xval, 2), "<br>", ejeY, ": ", round(yval, 2))
      ) %>% filter(!is.na(xval), !is.na(yval))
    
    groups <- if (color_by == "Cluster_Final") {
      list(keys = as.character(1:6), cols = COLORES_CLUSTERS, labs = paste0("C", 1:6, " · ", NOMBRES_ARQUETIPOS))
    } else {
      list(keys = c("C","D","LW","RW"), cols = COLORES_POSICION, labs = c("C","D","LW","RW"))
    }
    
    fig <- plot_ly()
    for (i in seq_along(groups$keys)) {
      g   <- groups$keys[i]
      sub <- df_plot %>% filter(color_var == g)
      if (nrow(sub) == 0) next
      fig <- fig %>% add_trace(
        data = sub, x = ~xval, y = ~yval, type = "scatter", mode = "markers", name = groups$labs[i],
        marker = list(color = groups$cols[g], size = 6, opacity = 0.7, line = list(color = "#FFFFFF", width = 0.5)),
        text = ~tooltip, hoverinfo = "text"
      )
    }
    fig %>% layout(
      xaxis = list(title = ejeX, color = "#1C3B5E", tickfont = list(color = "#5A6B7C"), gridcolor = "#E0E4E8", zeroline = FALSE),
      yaxis = list(title = ejeY, color = "#1C3B5E", tickfont = list(color = "#5A6B7C"), gridcolor = "#E0E4E8", zeroline = FALSE),
      paper_bgcolor = "#FFFFFF", plot_bgcolor  = "#FFFFFF", font = list(color = "#1C3B5E"),
      legend = list(font = list(color = "#1C3B5E"), bgcolor = "#FFFFFF", bordercolor = "#D1D8E0", borderwidth = 1)
    )
  })
  
  output$plot_loadings <- renderPlotly({
    pca_model <- modelo_pca()
    loadings <- as.data.frame(pca_model$rotation[, 1:4])
    loadings$Variable <- rownames(loadings)
    lm <- pivot_longer(loadings, -Variable, names_to = "PC", values_to = "Valor")
    
    plot_ly(lm, x = ~PC, y = ~Variable, z = ~Valor, type = "heatmap",
            colorscale = list(list(0, "#0077B6"), list(0.5, "#FFFFFF"), list(1, "#A81E2C")),
            zmin = -1, zmax = 1, text = ~round(Valor, 2),
            hovertemplate = "<b>%{y}</b> en %{x}<br>Loading: %{z:.2f}<extra></extra>") %>%
      layout(
        paper_bgcolor = "#FFFFFF", plot_bgcolor  = "#FFFFFF",
        xaxis = list(title = "Componente", color = "#1C3B5E", tickfont = list(color = "#5A6B7C")),
        yaxis = list(title = "", color = "#1C3B5E", tickfont = list(color = "#5A6B7C")),
        font = list(color = "#1C3B5E")
      )
  })
  
  output$plot_varianza <- renderPlotly({
    pca_model <- modelo_pca()
    ve <- pca_model$sdev^2
    pve <- ve / sum(ve)
    pve_cum <- cumsum(pve)
    n_show <- min(10, length(pve))
    
    df_ve <- data.frame(PC = paste0("PC", 1:n_show), Varianza = pve[1:n_show] * 100, Acumulada = pve_cum[1:n_show] * 100)
    df_ve$PC <- factor(df_ve$PC, levels = paste0("PC", 1:n_show))
    
    plot_ly(df_ve) %>%
      add_trace(x = ~PC, y = ~Varianza, type = "bar", name = "Varianza explicada", marker = list(color = "#0077B6"), hovertemplate = "<b>%{x}</b><br>Varianza: %{y:.1f}%<extra></extra>") %>%
      add_trace(x = ~PC, y = ~Acumulada, type = "scatter", mode = "lines+markers", name = "Acumulada", yaxis = "y2", line = list(color = "#FFB703", width = 2), marker = list(color = "#FFB703", size = 7), hovertemplate = "<b>%{x}</b><br>Acumulada: %{y:.1f}%<extra></extra>") %>%
      layout(
        yaxis  = list(title = "Varianza (%)", color = "#1C3B5E", tickfont = list(color = "#5A6B7C"), gridcolor = "#E0E4E8"),
        yaxis2 = list(title = "Acumulada (%)", overlaying = "y", side = "right", color = "#FFB703", tickfont = list(color = "#FFB703"), range = c(0, 105), gridcolor = "transparent"),
        xaxis  = list(title = "Componente", color = "#1C3B5E", tickfont = list(color = "#5A6B7C"), categoryorder = "array", categoryarray = levels(df_ve$PC)),
        paper_bgcolor = "#FFFFFF", plot_bgcolor  = "#FFFFFF", font   = list(color = "#1C3B5E"),
        legend = list(font = list(color = "#1C3B5E"), bgcolor = "rgba(255,255,255,0.9)", bordercolor = "#D1D8E0", borderwidth = 1, x = 1.15, y = 1, xanchor = "left", yanchor = "top"),
        margin = list(r = 150),
        shapes = list(list(type = "line", x0 = -0.5, x1 = n_show - 0.5, y0 = 80, y1 = 80, yref = "y2", line = list(color = "#52B788", dash = "dash", width = 1)))
      )
  })
  
  output$ui_desc_cluster <- renderUI({
    cl <- input$sel_cluster
    col <- COLORES_CLUSTERS[as.character(cl)]
    tags$div(
      style = paste0("padding:12px 14px; border-radius:6px; margin-top:8px;", "background:", col, "15; border-left:3px solid ", col, ";"),
      tags$strong(style = paste0("color:", col, "; font-size:13px;"), paste0("C", cl, " · ", NOMBRES_ARQUETIPOS[as.character(cl)])),
      tags$br(),
      tags$span(style = "color:#5A6B7C; font-size:12px; line-height:1.6;", DESCRIPCIONES_ARQUETIPOS[as.character(cl)])
    )
  })
  
  output$tabla_top5_cluster <- renderDT({
    cl <- as.integer(input$sel_cluster)
    dj <- datos_jugadores()
    centroide <- modelo_km()$centers[cl, ]
    pcs_cols <- intersect(colnames(dj), paste0("PC", 1:4))
    
    df_cl <- dj %>% filter(Cluster_Final == cl) %>% rowwise() %>% mutate(dist_centroide = sqrt(sum((c_across(all_of(pcs_cols)) - centroide)^2))) %>% ungroup() %>% arrange(dist_centroide) %>% head(5) %>% select(Jugador = nombre_completo, Pos = primaryPosition, Goles = goals, Asist = assists, Tiros = shots) %>% mutate(across(where(is.numeric), ~round(., 2)))
    datatable(df_cl, options = list(dom = "t", pageLength = 5), rownames = FALSE) %>% formatStyle(columns = colnames(df_cl), color = "#1C3B5E", backgroundColor = "#FFFFFF")
  })
  
  output$plot_radar_cluster <- renderPlotly({
    cl <- as.character(input$sel_cluster)
    rr <- resumen_radar()
    sub <- rr %>% filter(Cluster_Final == cl)
    if (nrow(sub) == 0) return(plotly_empty())
    
    sub <- sub %>% mutate(Variable = as.character(Variable)) %>% mutate(Variable = ifelse(Variable == "Penalizaciones", "Penaliz.", Variable)) %>% mutate(Variable = ifelse(Variable == "Perdidas", "Pérdidas", Variable))
    orden_deseado <- c("Goles", "Asistencias", "Tiros", "Pérdidas", "Bloqueos", "Hits", "Robos", "Penaliz.", "Minutos")
    sub <- sub %>% mutate(orden = match(Variable, orden_deseado)) %>% arrange(orden)
    col <- COLORES_CLUSTERS[cl]
    
    plot_ly(
      type = "scatterpolar", r = c(sub$Valor, sub$Valor[1]), theta = c(sub$Variable, sub$Variable[1]),
      fill = "toself", fillcolor = paste0(col, "40"), line = list(color = col, width = 2.5),
      mode = "lines+markers", marker = list(color = col, size = 7), hovertemplate = "<b>%{theta}</b>: %{r:.3f}<extra></extra>"
    ) %>%
      layout(
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, 1), color = "#5A6B7C", gridcolor = "#E0E4E8"),
          angularaxis = list(color = "#1C3B5E", direction = "clockwise", rotation = 70)
        ),
        paper_bgcolor = "#FFFFFF", plot_bgcolor  = "#FFFFFF", font = list(color = "#1C3B5E"),
        showlegend = FALSE, margin = list(l = 50, r = 60, t = 40, b = 40) 
      )
  })
  
  output$plot_radar_top5 <- renderPlotly({
    cl  <- as.integer(input$sel_cluster)
    dj  <- datos_jugadores()
    dn  <- datos_norm()
    centroide <- modelo_km()$centers[cl, ]
    pcs_cols  <- intersect(colnames(dj), paste0("PC", 1:4))
    
    top5 <- dj %>% filter(Cluster_Final == cl) %>% rowwise() %>% mutate(dist_centroide = sqrt(sum((c_across(all_of(pcs_cols)) - centroide)^2))) %>% ungroup() %>% arrange(dist_centroide) %>% head(5) %>% select(firstName, lastName)
    vars <- c("goals", "assists", "shots", "giveaways", "blocked", "hits", "takeaways", "penaltyMinutes", "timeOnIce")
    var_labs <- c("Goles", "Asistencias", "Tiros", "Pérdidas", "Bloqueos", "Hits", "Robos", "Penaliz.", "Minutos")
    col <- COLORES_CLUSTERS[as.character(cl)]
    cols5 <- colorRampPalette(c(col, "#A9B7C6"))(7)[1:5] 
    
    fig <- plot_ly()
    for (i in seq_len(nrow(top5))) {
      j_norm <- dn %>% filter(firstName == top5$firstName[i], lastName == top5$lastName[i]) %>% slice(1)
      if (nrow(j_norm) == 0) next
      vals <- sapply(vars, function(v) { vv <- j_norm[[v]]; if (is.null(vv) || length(vv) == 0) 0 else as.numeric(vv[1]) })
      fig <- fig %>% add_trace(
        type = "scatterpolar", r = c(vals, vals[1]), theta = c(var_labs, var_labs[1]),
        fill = "none", line = list(color = cols5[i], width = 2), mode = "lines+markers", marker = list(color = cols5[i], size = 6), name = paste(top5$firstName[i], top5$lastName[i])
      )
    }
    fig %>% layout(
      polar = list(
        radialaxis = list(visible = TRUE, range = c(0, 1), color = "#5A6B7C", gridcolor = "#E0E4E8"),
        angularaxis = list(color = "#1C3B5E", direction = "clockwise", rotation = 70)
      ),
      paper_bgcolor = "#FFFFFF", plot_bgcolor  = "#FFFFFF", font = list(color = "#1C3B5E"),
      legend = list(font = list(color = "#1C3B5E", size = 10), bgcolor = "rgba(255,255,255,0.9)", bordercolor = "#D1D8E0", orientation = "h", x = 0, y = -0.3),  
      margin = list(l = 50, r = 50, t = 40, b = 60) 
    )
  })
  
  output$plot_comparativa_clusters <- renderPlotly({
    var <- input$var_comparativa
    dj  <- datos_jugadores()
    resumen <- dj %>% group_by(Cluster_Final) %>% summarise(media = mean(.data[[var]], na.rm = TRUE), .groups = "drop") %>% mutate(color = COLORES_CLUSTERS[as.character(Cluster_Final)], label = paste0("C", Cluster_Final, "<br>", NOMBRES_ARQUETIPOS[as.character(Cluster_Final)]))
    
    y_max <- max(resumen$media, na.rm = TRUE)
    y_min <- min(resumen$media, na.rm = TRUE)
    rango_y <- c(ifelse(y_min < 0, y_min * 1.15, 0), ifelse(y_max > 0, y_max * 1.15, 0))
    
    plot_ly(resumen, x = ~label, y = ~media, type = "bar",
            marker = list(color = ~color, line = list(color = "#FFFFFF", width = 1)),
            text = ~round(media, 3), textposition = "outside", cliponaxis = FALSE,
            textfont = list(color = "#1C3B5E", size = 11), hovertemplate = "<b>%{x}</b><br>Media: %{y:.3f}<extra></extra>") %>%
      layout(
        xaxis = list(title = "", color = "#1C3B5E", tickfont = list(color = "#5A6B7C")),
        yaxis = list(title = names(which(c("goals"="Goles","assists"="Asistencias","shots"="Tiros","hits"="Hits","blocked"="Bloqueos","timeOnIce"="Tiempo en Hielo","plusMinus"="±","penaltyMinutes"="Penalizaciones","takeaways"="Robos","giveaways"="Pérdidas") == var)),
                     color = "#1C3B5E", tickfont = list(color = "#5A6B7C"), gridcolor = "#E0E4E8", range = rango_y),
        paper_bgcolor = "#FFFFFF", plot_bgcolor  = "#FFFFFF", font = list(color = "#1C3B5E"),
        showlegend = FALSE, margin = list(t = 20)
      )
  })
  
  output$plot_posicion_norm <- renderPlotly({
    dj <- datos_jugadores() %>% filter(primaryPosition %in% c("C", "LW", "RW", "D"))
    pos_norm <- dj %>% group_by(primaryPosition, Cluster_Final) %>% tally() %>% group_by(primaryPosition) %>% mutate(peso_relativo = n / sum(n)) %>% group_by(Cluster_Final) %>% mutate(pct_ajustado = peso_relativo / sum(peso_relativo) * 100) %>% ungroup()
    
    plot_ly(pos_norm, x = ~paste0("C", Cluster_Final), y = ~pct_ajustado, color = ~primaryPosition, colors = COLORES_POSICION, type = "bar", hovertemplate = "<b>%{x}</b><br>%{fullData.name}: %{y:.1f}%<extra></extra>") %>%
      layout(
        barmode = "stack", paper_bgcolor = "#FFFFFF", plot_bgcolor  = "#FFFFFF",
        xaxis = list(title = "Arquetipo", color = "#1C3B5E", tickfont = list(color = "#5A6B7C")),
        yaxis = list(title = "% Normalizado", color = "#1C3B5E", tickfont = list(color = "#5A6B7C"), gridcolor = "#E0E4E8"),
        legend = list(font = list(color = "#1C3B5E")), font = list(color = "#1C3B5E")
      )
  })
  
  output$plot_equipo_comp <- renderPlotly({
    dec <- datos_equipo_cluster()
    eq1 <- input$sel_equipo1
    eq2 <- input$sel_equipo2
    equipos <- if (nzchar(eq2)) c(eq1, eq2) else eq1
    
    df_plot <- dec %>% filter(teamName %in% equipos) %>% group_by(teamName, Cluster_Final) %>% tally() %>% group_by(teamName) %>% mutate(pct = n / sum(n) * 100, color = COLORES_CLUSTERS[as.character(Cluster_Final)], nombre_arq = NOMBRES_ARQUETIPOS[as.character(Cluster_Final)])
    
    fig <- plot_ly()
    for (cl in as.character(1:6)) {
      sub <- df_plot %>% filter(as.character(Cluster_Final) == cl)
      if (nrow(sub) == 0) next
      fig <- fig %>% add_trace(data = sub, x = ~teamName, y = ~pct, type = "bar", name = paste0("C", cl, " · ", NOMBRES_ARQUETIPOS[cl]), marker = list(color = COLORES_CLUSTERS[cl], line = list(color = "#FFFFFF", width = 1)), text = ~paste0(round(pct, 1), "%"), textposition = "inside", textfont = list(color = "white", size = 10), hovertemplate = paste0("<b>%{x}</b><br>C", cl, " · ", NOMBRES_ARQUETIPOS[cl], "<br>%{y:.1f}%<extra></extra>"))
    }
    fig %>% layout(
      barmode = "stack", xaxis = list(title = "", color = "#1C3B5E", tickfont = list(color = "#5A6B7C")),
      yaxis = list(title = "% del Roster", color = "#1C3B5E", tickfont = list(color = "#5A6B7C"), gridcolor = "#E0E4E8", range = c(0, 105)),
      paper_bgcolor = "#FFFFFF", plot_bgcolor  = "#FFFFFF", font = list(color = "#1C3B5E"),
      legend = list(font = list(color = "#1C3B5E", size = 10), bgcolor = "#FFFFFF", bordercolor = "#D1D8E0")
    )
  })
  
  output$plot_ranking_equipos <- renderPlotly({
    cl  <- as.character(input$ranking_cluster)
    dir <- input$ranking_dir
    dec <- datos_equipo_cluster()
    col <- COLORES_CLUSTERS[cl]
    
    ranking <- dec %>% group_by(teamName, Cluster_Final) %>% tally() %>% group_by(teamName) %>% mutate(pct = n / sum(n) * 100) %>% filter(as.character(Cluster_Final) == cl) %>% ungroup()
    if (nrow(ranking) == 0) return(plotly_empty())
    ranking <- if (dir == "desc") arrange(ranking, desc(pct)) else arrange(ranking, pct)
    ranking$teamName <- factor(ranking$teamName, levels = ranking$teamName)
    
    plot_ly(ranking, x = ~pct, y = ~teamName, type = "bar", orientation = "h", marker = list(color = col, line = list(color = "#FFFFFF", width = 0.5)), text = ~paste0(round(pct, 1), "%"), textposition = "outside", textfont = list(color = "#1C3B5E", size = 10), hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<extra></extra>") %>%
      layout(
        xaxis = list(title = "% del Roster", color = "#1C3B5E", tickfont = list(color = "#5A6B7C"), gridcolor = "#E0E4E8"),
        yaxis = list(title = "", color = "#1C3B5E", tickfont = list(color = "#5A6B7C"), autorange = "reversed"),
        paper_bgcolor = "#FFFFFF", plot_bgcolor  = "#FFFFFF", font = list(color = "#1C3B5E"), showlegend = FALSE
      )
  })
  
  lideres_equipos <- reactive({
    eq1 <- input$sel_equipo1
    eq2 <- input$sel_equipo2
    arq <- input$sel_arquetipo_vs
    if (!nzchar(eq2)) return(NULL) 
    equipos <- c(eq1, eq2)
    dj <- datos_jugadores()
    dec <- datos_equipo_cluster() %>% select(firstName, lastName, teamName)
    
    df_completo <- dj %>% inner_join(dec, by = c("firstName", "lastName")) %>% filter(teamName %in% equipos, Cluster_Final == arq)
    if (nrow(df_completo) == 0) return(data.frame()) 
    df_completo %>% group_by(teamName) %>% slice_max(order_by = timeOnIce, n = 1, with_ties = FALSE) %>% ungroup()
  })
  
  output$ui_comparativa_lideres <- renderUI({
    lideres <- lideres_equipos()
    eq1 <- input$sel_equipo1
    eq2 <- input$sel_equipo2
    if (!nzchar(eq2)) {
      return(tags$div(style = "text-align:center; padding: 40px; color:#5A6B7C;", tags$h3(style = "font-family: 'Barlow Condensed'; font-weight: 700; color:#1C3B5E;", "SELECCIONA UN SEGUNDO EQUIPO"), tags$p("Elige un 'Equipo B' en los selectores de arriba para activar el Cara a Cara.")))
    }
    
    crear_tarjeta <- function(j, color_borde, nombre_equipo) {
      if (is.null(j) || nrow(j) == 0) {
        return(tags$div(style = paste0("padding: 20px; background-color: #FFFFFF; border-radius: 8px; box-shadow: 0 4px 15px rgba(0,0,0,0.05); border: 2px solid #D1D8E0; border-top: 6px solid #5A6B7C; text-align: center; height: 100%; display: flex; flex-direction: column; justify-content: center;"), tags$h4(style = "color:#1C3B5E; font-weight:600;", nombre_equipo), tags$p(style = "color:#5A6B7C; font-style:italic;", "No dispone de jugadores de este arquetipo.")))
      }
      cl <- as.character(j$Cluster_Final); col <- COLORES_CLUSTERS[cl]; nom <- NOMBRES_ARQUETIPOS[cl]
      tags$div(
        style = paste0("padding: 20px; background-color: #FFFFFF; border-radius: 8px; box-shadow: 0 4px 15px rgba(0,0,0,0.05); border: 2px solid #D1D8E0; border-top: 6px solid ", color_borde, "; text-align: center; height: 100%;"),
        tags$h3(style = "color:var(--nhl-blue); margin-top:0; font-family: 'Barlow Condensed'; font-weight:900; font-size: 24px;", paste(j$firstName, j$lastName)),
        tags$h5(style = "color:#5A6B7C; margin-bottom:15px; font-weight:600;", j$teamName),
        tags$div(style = paste0("display:inline-block; margin-bottom:20px; padding:4px 14px; border-radius:20px; background:", col, "33; border:1px solid ", col, "; color:", col, "; font-size:12px; font-weight:700;"), paste0("C", cl, " · ", nom)),
        fluidRow(
          column(6, tags$div(class = "stat-card", tags$span(class = "stat-val", j$primaryPosition), tags$span(class = "stat-lbl", "Posición"))),
          column(6, tags$div(class = "stat-card", tags$span(class = "stat-val", round(j$timeOnIce/60, 1)), tags$span(class = "stat-lbl", "Minutos/P")))
        ),
        fluidRow(
          column(6, tags$div(class = "stat-card", tags$span(class = "stat-val", round(j$goals, 2)), tags$span(class = "stat-lbl", "Goles"))),
          column(6, tags$div(class = "stat-card", tags$span(class = "stat-val", round(j$assists, 2)), tags$span(class = "stat-lbl", "Asist.")))
        ),
        fluidRow(
          column(6, tags$div(class = "stat-card", tags$span(class = "stat-val", round(j$hits, 2)), tags$span(class = "stat-lbl", "Hits"))),
          column(6, tags$div(class = "stat-card", tags$span(class = "stat-val", round(j$blocked, 2)), tags$span(class = "stat-lbl", "Bloqueos")))
        )
      )
    }
    
    j1 <- if(is.null(lideres) || nrow(lideres %>% filter(teamName == eq1)) == 0) NULL else lideres %>% filter(teamName == eq1)
    j2 <- if(is.null(lideres) || nrow(lideres %>% filter(teamName == eq2)) == 0) NULL else lideres %>% filter(teamName == eq2)
    
    fluidRow(
      style = "display: flex; align-items: stretch;",
      column(5, crear_tarjeta(j1, "#0077B6", eq1)), 
      column(2, tags$div(style = "height: 100%; display: flex; align-items: center; justify-content: center; min-height: 250px;", tags$h1(style = "color: #FFB703; font-family: 'Barlow Condensed'; font-weight: 900; font-size: 55px; font-style: italic; text-shadow: 1px 1px 3px rgba(0,0,0,0.1);", "VS"))),
      column(5, crear_tarjeta(j2, "#A81E2C", eq2)) 
    )
  })
  
  output$tabla_market <- renderDT({
    df <- datos_jugadores()
    df_filtrado <- df %>% filter(goals >= input$f_goals[1], goals <= input$f_goals[2], assists >= input$f_assists[1], assists <= input$f_assists[2], shots >= input$f_shots[1], shots <= input$f_shots[2], hits >= input$f_hits[1], hits <= input$f_hits[2], blocked >= input$f_blocked[1], blocked <= input$f_blocked[2], takeaways >= input$f_takeaways[1], takeaways <= input$f_takeaways[2], giveaways >= input$f_giveaways[1], giveaways <= input$f_giveaways[2], penaltyMinutes >= input$f_penalties[1], penaltyMinutes <= input$f_penalties[2], (timeOnIce/60) >= input$f_toi[1], (timeOnIce/60) <= input$f_toi[2])
    if (input$f_pos != "Todas") df_filtrado <- df_filtrado %>% filter(primaryPosition == input$f_pos)
    
    df_display <- df_filtrado %>% mutate(Arquetipo = paste0("C", Cluster_Final, " · ", NOMBRES_ARQUETIPOS[as.character(Cluster_Final)]), Min_GP = round(timeOnIce/60, 1)) %>% select(Jugador = nombre_completo, Pos = primaryPosition, Arquetipo, Goles = goals, Asist = assists, Tiros = shots, Hits = hits, Blq = blocked, Rob = takeaways, Per = giveaways, PIM = penaltyMinutes, `Min/P` = Min_GP) %>% mutate(across(where(is.numeric), ~round(., 2)))
    
    datatable(df_display, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>% formatStyle(columns = colnames(df_display), color = "#1C3B5E", backgroundColor = "#FFFFFF")
  })
}

# =============================================================================
# LANZAR APP
# =============================================================================

shinyApp(ui = ui, server = server)
