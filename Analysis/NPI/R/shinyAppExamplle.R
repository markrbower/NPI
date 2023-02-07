library(shiny)
library(parallel)

ui <- fluidPage(
  
  # Application title
  titlePanel("Test parallel in Shiny app"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("n","N",value = 100),
      numericInput("mean","Mean",value = 1000),
      checkboxInput("parallel","Parallel?",value=FALSE)
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output) {
  
  simulate <- reactive({
    
    mean = input$mean
    
    sim = rnorm(input$n,mean=mean,sd = 100)
    
    return(sim)
    
  })
  
  p_simulate<-reactive({
    
    cl = makeCluster(detectCores()-1)
    
    var_mean = input$mean
    
    clusterExport(cl,varlist="var_mean", envir = environment())
    
    sim = parSapply(cl, 
                    1:input$n,
                    function(x) rnorm(1,mean=var_mean,sd = 100)
    )
    
    stopCluster(cl)
    
    sim
    
  })
  
  output$distPlot <- renderPlot({
    
    if(input$parallel){
      x = p_simulate()
    } else x = simulate()
    
    # draw the histogram with the specified number of bins
    hist(x)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)