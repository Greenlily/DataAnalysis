library(shiny)
library(ggplot2)
options(shiny.maxRequestSize=30*1024^2)
# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("数据可视化"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "请选择 CSV 文件",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Specification of range within an interval
      sliderInput("range", "Range:",
                  min = 0, max = 0.5, value = c(0.05,0.5)),
      
      # Input: Checkbox if file has header ----
      # checkboxInput("header", "Header", TRUE),

      
      # Horizontal line ----
      # tags$hr()

      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(合成 = "One",
                               分图 = "Many"),
                   selected = "Many")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      plotOutput("sepratePlot"),
      # plotOutput("distPlot"),
      
      verbatimTextOutput("summary"),
      tableOutput("contents")
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  data <- reactive({  
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ',',
                   quote = '""')
  })
  
  output$contents <- renderTable({
      return(head(data(),n=6L))
  })
  output$distPlot <- renderPlot({

    df<-data()
    ggplot(df, aes(df$Time.s.)) + 
      geom_line(aes(y = df$CH2.2.V., colour = "var0")) + 
      geom_line(aes(y = df$CH3.2.V., colour = "var1"))
  })
  output$summary <- renderPrint({
    summary(data())
  })
  output$sepratePlot <- renderPlot({
    
    df<-data()
     if(input$disp == "One") {
       ggplot(df, aes(df$Time.s.)) + 
    geom_line(aes(y = df$CH2.2.V., colour = "var0")) + 
      geom_line(aes(y = df$CH3.2.V., colour = "var1"))
    }
    else {
      p1<-ggplot(data = df, mapping = aes(x = df$Time.s., y = df$CH3.2.V.)) + 
        geom_line()+ labs(x="Time[s]",y="CH3.2.V.",title = "时序-量程（CH3）")+#geom_point() +
       scale_x_continuous(limits=input$range)
      p2<-ggplot(data = df, mapping = aes(x = df$Time.s., y = df$CH2.2.V.)) + 
        geom_line()+ labs(x="Time[s]",y="CH2.2.V.",title = "时序-量程（CH2）")+ #geom_point()+
       scale_x_continuous(limits=input$range)
      multiplot(p1, p2, cols=1)
    }
    
  })
}

# Create Shiny app ----
shinyApp(ui, server)


