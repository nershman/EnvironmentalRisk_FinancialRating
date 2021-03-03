

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
  
}
