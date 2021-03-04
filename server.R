

server <- function(input, output, session) {
  
  data <- reactive({
    file1 <- input$file1
    if (is.null(file1)) { 
      return() 
    } 
    data <- read_excel(file1$datapath)
    data
  })
  
  observe({
    updateSelectInput(
      session,
      "response",
      choices = names(data()[-1])
    )
  })
  
  observe({
    updateCheckboxGroupInput(
      session,
      "covariate",
      choices = names(data()[-1])
    )
  })
  
  
  # observe button 1 press.
  observe({
    input$runbutton
    if(input$response == "Financial Rating"){
      current_model <- readRDS("models/ind_gam.RData")
    }
    
    if(input$response == "Qualitative Rating"){
      current_model <- readRDS("models/quali.RData")
    }
    
    ###DEBUG MODAL:
    showModal(modalDialog(
      title = " debug: different model loaded",
      length(current_model)
    ))
    
    
  })

  
  output$tbl <- DT::renderDataTable({
    DT::datatable(data())
  })

  
}
