# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
# https://shiny.posit.co/


library(shiny)
library(bslib)
library(googlesheets4)
library(tidyverse)
library(plotly)
library(lubridate)  # untuk fungsi today dan dmy

# UI dengan theme bslib dan layout terstruktur
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-image: url('https://ppid.bps.go.id/upload/header/1213_header_1741581599.png');
        background-size: cover;
        background-position: center;
        background-repeat: no-repeat;
        background-attachment: fixed;
      }

      .content-box {
        background-color: rgba(255,255,255,0.9);
        padding: 20px;
        border-radius: 10px;
        margin: 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
      }
    "))
  ),
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#0d6efd"
  ),
  tags$style(HTML("
  .filter-box {
    background-color: rgba(255,255,255,0.95);
    padding: 20px;
    border-radius: 10px;
    box-shadow: 0 4px 10px rgba(0,0,0,0.1);
    margin-bottom: 20px;
  }
")),
  
  
  # Icon dan Judul utama
    div(
      style ="
    background-size: cover;
    background-position: center;
    padding: 20px;
    color: black;
    text-align: center;
      ",
      
      style = "text-align:center;",
      img(
        src = "https://api.jatimnet.com/jinet/assets/media/news/news/image_front/LOGO_BPS_JATIM.png",
        height = "80px",
        style = "margin-bottom:3px;"
      ),
      tags$i(class = "bi bi-bar-chart-fill", style = "font-size:60px; color:#0d6efd;"),
      tags$h1("MONITORING HARGA PASAR", style = "font-weight:700; margin-top:0px; 
              color:black; margin-bottom: 0px"),
      tags$p("Pantau Harga Komoditas Secara Real-Time di Beberapa Pasar di Kabupaten Langkat", 
             style = "font-style: italic; font-size:18px; color:black;")
  ),
  
  
  # Navbar dengan tabPanel
  navbarPage(
    title = NULL,
    id = "page", 
    collapsible = TRUE,
     
    tabPanel(
      title = tags$b("Beranda"),
      value = "beranda",

      div(
        style = "background-color: #f0f0f0; padding: 20px;",
      fluidRow(
        column(
          width = 12,
          div(
            style = "text-align:center;padding: 30px; font-size: 18px; line-height: 1.6; color: #333;",
            tags$h3("Selamat Datang di Dashboard Monitoring Harga Pasar"),
          ),
            div(
            style = "padding: 30px; font-size: 18px; line-height: 1.6; color: #333;",
            tags$p("Aplikasi ini bertujuan untuk memantau harga komoditas secara real-time di berbagai pasar yang ada di Kabupaten Langkat."),
            tags$p("Data diperoleh langsung dari Google Sheets yang diisi oleh petugas pasar setiap hari."),
            tags$p("Gunakan tab navigasi di atas untuk melihat grafik harga dari masing-masing pasar."),
            tags$p("Jika Anda adalah admin, silakan masuk ke tab Pengaturan untuk mengatur sumber data.")
          )  # ← ini div() belum ditutup sebelumnya
        )
        )
      )
    ),
    
    
    tabPanel(
      title = "Pasar Stabat",
      value = "stabat",
      fluidRow(
        column(
          width = 4,
          div(class = "filter-box",
          h4("Filter Data"),
          selectizeInput(
            "Komoditas1",
            "Pilih Komoditas:",
            choices = NULL,
            width = "100%"
          ),
          dateRangeInput(
            "rentangtanggal1",
            "Rentang Tanggal",
            start = today() - months(3),
            end = today(),
            format = "dd-mm-yyyy",
            language = "id",
            width = "100%"
          ),
          actionButton("reload1", "Reload Data", class = "btn btn-primary w-100")
        )
        ),
        column(
          width = 8,
          plotlyOutput("plot1", height = "350px")
        )
      )
    ), 
     
    tabPanel(
      title = "Pasar Babalan",
      value = "babalan",
      fluidRow(
        column(
          width = 4,
          h4("Filter Data"),
          selectizeInput(
            "Komoditas2",
            "Pilih Komoditas:",
            choices = NULL,
            width = "100%"
          ),
          dateRangeInput(
            "rentangtanggal2",
            "Rentang Tanggal",
            start = today() - months(3),
            end = today(),
            format = "dd-mm-yyyy",
            language = "id",
            width = "100%"
          ),
          actionButton("reload2", "Reload Data", class = "btn btn-primary w-100")
        ),
        column(
          width = 8,
          plotlyOutput("plot2", height = "350px")
        )
      )
    ),
    
    tabPanel(
      title = "Pasar Kuala",
      value = "Kuala",
      fluidRow(
        column(
          width = 4,
          h4("Filter Data"),
          selectizeInput(
            "Komoditas3",
            "Pilih Komoditas:",
            choices = NULL,
            width = "100%"
          ),
          dateRangeInput(
            "rentangtanggal3",
            "Rentang Tanggal",
            start = today() - months(3),
            end = today(),
            format = "dd-mm-yyyy",
            language = "id",
            width = "100%"
          ),
          actionButton("reload3", "Reload Data", class = "btn btn-primary w-100")
        ),
        column(
          width = 8,
          plotlyOutput("plot3", height = "350px")
        )
      )
    ),
    
    tabPanel(
      title = "Pengaturan",
      value = "pengaturan",
      fluidRow(
        column(
          width = 6,
          textInput(
            "link",
            "Link googlesheets:",
            width = "100%"
          ),
          actionButton(
            "simpan",
            "Simpan",
            class = "btn btn-primary mt-2"
          )
        )
      )
    )
  )
) 
    

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  updateTextInput(
    inputId = "link",
    value = read_lines("link.txt")
  )
  
  ambildata <- function(){
    
    gs4_deauth()
    link <- read_lines("link.txt")
    
    showModal(modalDialog("Mohon tunggu... Data sedang diambil.", footer = NULL, easyClose = FALSE)) 
    
    sheet_names <- sheet_names(link)
    
    dataset <- NULL
    for (i in 1:length(sheet_names)) {
      dataset[[i]] <- read_sheet(
        ss = link,
        sheet = i,
        col_names = T,
        col_types = NULL
      )
    }
    
    removeModal()
    return(dataset)
  }
  
  data <- reactiveVal(ambildata())
  
  observeEvent(
    list(input$reload1, input$reload2, input$reload3),
    data(ambildata()),
    ignoreInit = T
  )
  
  data_longer <- reactive({
    data_longer_ <- NULL
    for (i in 1:length(data())) {
      data_longer_[[i]] <- data()[[i]]|>
        pivot_longer(
          cols = !c(No, Komoditas),
          names_to = "Tanggal",
          values_to = "Harga"
        )|>
        mutate(Tanggal = dmy(Tanggal))
    }
    return(data_longer_)
  })
  
  # Define UI for application that draws a histogram
  Komoditas <- reactive(as.list(data()[[1]]$Komoditas))
  
  observe({
    updateSelectizeInput(
      inputId = "Komoditas1",
      choices = Komoditas()
    )
    
    updateSelectizeInput(
      inputId = "Komoditas2",
      choices = Komoditas()
    )
    
    updateSelectizeInput(
      inputId = "Komoditas3",
      choices = Komoditas()
    )
  })
  
  buatplot <- function(datainput, inputkomoditas, rangetanggal){
    # showModal(modalDialog("Mohon tunggu plot sedang dibuat...", footer = NULL, easyClose = F))
    
    data_plot <- datainput|>
      filter(Komoditas == inputkomoditas)|>
      filter(rangetanggal[[1]] <= Tanggal & Tanggal <= rangetanggal[[2]])
    
    plot <- plot_ly(data_plot, type = "scatter", mode = "lines")|>
      add_trace(x = ~Tanggal, y = ~Harga, name = "Harga")
    
    # removeModal()
    
    plot
  }
  
  # percobaan 1 ----
  # a <- reactive(tibble(a = c(1,2), b = c(3,4)))
  # output$data_head <- renderPrint(
  #   a()
  # )
  output$plot1 <- renderPlotly({
    req(input$Komoditas1)
    buatplot(data_longer()[[1]], input$Komoditas1, input$rentangtanggal1)
  })
  
  output$plot2 <- renderPlotly({
    req(input$Komoditas2)
    buatplot(data_longer()[[2]], input$Komoditas2, input$rentangtanggal2)
  })
  
  output$plot3 <- renderPlotly({
    req(input$Komoditas3)
    buatplot(data_longer()[[3]], input$Komoditas3, input$rentangtanggal3)
  })
  
  observe({
    tryCatch({
      gs4_get(input$link)
      write_lines(input$link, "link.txt")
    }, error = function(e){
      showNotification("Error", type = "error")
    })
  })|>bindEvent(input$simpan)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
