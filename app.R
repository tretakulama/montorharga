#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(googlesheets4)
library(tidyverse)
library(plotly)
library(httr)

ui <- page_navbar(
  nav_panel(
    "Pasar Stabat",
    layout_columns(
      selectizeInput(
        "Komoditas1",
        "Pilih Komoditas:",
        choices = NULL
      ),
      dateRangeInput(
        inputId = "rentangtanggal1",
        label = "Rentang Tanggal",
        end = today(),
        start = today() - months(3),
        language = "id",
        format = "dd-mm-yyyy"
      ),
      actionButton("reload1", "Reload Data")
    ),
    plotlyOutput("plot1")
  ),
  nav_panel(
    "Pasar Babalan",
    layout_columns(
      selectizeInput(
        "Komoditas2",
        "Pilih Komoditas:",
        choices = NULL
      ),
      dateRangeInput(
        inputId = "rentangtanggal2",
        label = "Rentang Tanggal",
        end = today(),
        start = today() - months(3),
        language = "id",
        format = "dd-mm-yyyy"
      ),
      actionButton("reload2", "Reload Data")
    ),
    plotlyOutput("plot2")
  ),
  nav_panel(
    "Pasar Kuala",
    layout_columns(
      selectizeInput(
        "Komoditas3",
        "Pilih Komoditas:",
        choices = NULL
      ),
      dateRangeInput(
        inputId = "rentangtanggal3",
        label = "Rentang Tanggal",
        end = today(),
        start = today() - months(3),
        language = "id",
        format = "dd-mm-yyyy"
      ),
      actionButton("reload3", "Reload Data")
    ),
    plotlyOutput("plot3")
  ),
  nav_spacer(),
  nav_panel(
    "Pengaturan",
    textInput(
      "link",
      "Link googlesheets: ",
      width = "500px"
    ),
    actionButton(
      "simpan",
      "Simpan"
    )
  ),
  title = "Monitoring Harga Pasar",
  id = "page"
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  updateTextInput(
    inputId = "link",
    value = read_lines("link.txt")
  )

  ambildata <- function(){
    
    gs4_deauth()
    
    # --- BLOK DIAGNOSTIK 1: BACA LINK ---
    message("Mencoba membaca file link.txt...")
    link <- read_lines("link.txt")
    message("URL yang berhasil dibaca dari link.txt adalah: '", link, "'")
    # ------------------------------------
    
    if (length(link) == 0 || link == "") {
      showNotification("URL Google Sheet di link.txt kosong.", type = "error", duration = 10)
      return(NULL)
    }
    
    showModal(modalDialog("Mohon tunggu... Data sedang diambil.", footer = NULL, easyClose = FALSE)) 
    
    # --- BLOK DIAGNOSTIK 2: UJI KONEKSI DASAR ---
    message("Menguji koneksi dasar ke Google Sheets menggunakan httr...")
    require(httr) # Pastikan httr di-load
    response <- try(GET(link, timeout(10)), silent = TRUE) # Coba konek dalam 10 detik
    
    if (inherits(response, "try-error")) {
      message("GAGAL melakukan GET request. Kemungkinan server diblokir.")
    } else {
      message("Berhasil melakukan GET request. Status code: ", status_code(response))
    }
    # --------------------------------------------
    
    dataset <- tryCatch({
      message("Mencoba mengambil data dengan googlesheets4::read_sheet()...")
      sheet_names <- sheet_names(link)
      
      dataset_list <- NULL
      for (i in 1:length(sheet_names)) {
        dataset_list[[i]] <- read_sheet(
          ss = link,
          sheet = i,
          col_names = TRUE,
          col_types = "c"
        )
      }
      
      removeModal()
      message("Data berhasil diambil dan diproses oleh googlesheets4.")
      showNotification("Data berhasil dimuat ulang!", type = "message")
      
      # --- BLOK DIAGNOSTIK 3: PERIKSA STRUKTUR DATA ---
      message("Mencetak struktur data yang diambil:")
      print(str(dataset_list))
      # ---------------------------------------------
      
      return(dataset_list)
      
    }, error = function(e) {
      removeModal()
      # --- BLOK DIAGNOSTIK 4: TANGKAP ERROR ---
      message("!!! TERJADI ERROR saat mengambil data: ", e$message)
      # ------------------------------------------
      showNotification(
        paste("Gagal mengambil data. Error:", e$message),
        type = "error",
        duration = 15
      )
      return(NULL)
    })
    
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
        mutate(
          No = as.integer(No),
          Tanggal = dmy(Tanggal, locale = "id"),
          Harga = parse_number(Harga, locale = locale(grouping_mark = "."))
        )
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
    showModal(modalDialog("Mohon tunggu plot sedang dibuat...", footer = NULL, easyClose = T))
    
    data_plot <- datainput|>
      filter(Komoditas == inputkomoditas)|>
      filter(rangetanggal[[1]] <= Tanggal & Tanggal <= rangetanggal[[2]])
    
    plot <- plot_ly(data_plot, type = "scatter", mode = "lines")|>
      add_trace(x = ~Tanggal, y = ~Harga, name = "Harga")
    
    removeModal()
    
    plot
  }
  
# percobaan 1 ----
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
