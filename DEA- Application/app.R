library(shiny)
library(shinyLP)
library(Benchmarking)
library(shinythemes)
ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Know Your Peers"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "RTS",label = "Choose RTS",choices= c("crs","vrs","irs","fdh","drs")),
      plotOutput("plot", width = "100%")
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Data",dataTableOutput("Comp_data")),
        tabPanel("Efficiency", verbatimTextOutput("eff")), 
        tabPanel("Peers", verbatimTextOutput("peers")),
        tabPanel("Peer Weightage",verbatimTextOutput("lambda")),
        
        fluidRow(
          titlePanel("Calculate Your Efficiency"),
          column(4,
                 numericInput(inputId = "inp",label="Input" ,value = 0,min = 0),
                 numericInput(inputId = "out",label="Useful output" ,value = 0,min = 0),
                 # numericInput(inputId = "exp",label="Expenses" ,value = 0,min = 0),
                 # numericInput(inputId = "loans",label="Loans" ,value = 0,min = 0),
                 # numericInput(inputId = "dep",label="Deposits" ,value = 0,min= 0),
                 actionButton(inputId="submit",label = "Submit")),
          column("My Efficiency",verbatimTextOutput("myeff"), placeholder =TRUE, width =3)
          
        )
      )
      
    )
  )
)

server <- function(input, output) {
  Comp_data <- read.csv("Comp_data.csv")
  output$Comp_data <- renderDataTable({Comp_data[,1:4]})
  output$b1 <- renderTable(Comp_data[,1])
  x <- matrix(Comp_data$Expenses)
  y <- matrix(c(Comp_data$Loans,Comp_data$Deposits),ncol = 2)
  
  output$eff <- renderPrint({Efficiency <- {dea(x,y,RTS = input$RTS)$eff}
  as.data.frame(Efficiency)
  as.data.frame(cbind(levels(Comp_data$DMU),Efficiency))})
  output$plot <- renderPlot({dea.plot.frontier(x,y, RTS= input$RTS,main = "DEA PLOT",xlab="Input", ylab="Output")})
  output$peers <- renderPrint({as.data.frame(peers({dea(x,y,RTS = input$RTS)}))})
  output$lambda <- renderPrint({as.data.frame(lambda({dea(x,y,RTS = input$RTS)}))})
  
  
  # output$Pic <-
  #   renderImage({
  #     outfile <- tempfile(fileext = 'Capture.PNG')
  #     png(outfile, width = 400, height = 300)
  #     dev.off()
  #     list(
  #       src = outfile,
  #       contentType = 'image/png',
  #       width = 400,
  #       height = 300,
  #       alt = "This is alternate text"
  #     )
  
  
  myeff <- reactiveVal(0)
  observeEvent(input$submit, {
    neweff <- input$out/input$inp
    myeff(neweff)             
  })  
  
  
  # 
  # myeff <- eventReactive(input$submit,{
  #   a <- matrix(input$exp)
  #   b <- matrix(c(input$dep,input$loans),ncol = 2)
  #   e <- dea(a,b,RTS = "vrs", ORIENTATION="in")
  #   e
    
  
  output$myeff <- renderPrint({myeff()
    
  })
}

shinyApp(ui = ui, server = server)