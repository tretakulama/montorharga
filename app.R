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
    link <- read_lines("link.txt")
    
    showModal(modalDialog("Mohon tunggu... Data sedang diambil.", footer = NULL, easyClose = FALSE)) 
    
    sheet_names <- sheet_names(link)
    
    dataset <- NULL
      for (i in 1:length(sheet_names)) {
        dataset[[i]] <- read_sheet(
          ss = link,
          sheet = i,
          col_names = T,
          col_types = "c"
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
    plot <- ggplot(data_plot) +
      geom_point(mapping = aes(x = Tanggal, y = Harga)) +
      geom_line(mapping = aes(x = Tanggal, y = Harga))

    removeModal()
    
    ggplotly(plot)
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
