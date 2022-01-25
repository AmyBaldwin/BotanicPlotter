## the basic botanic plot mapper


library(shiny)
library(ggplot2)
library(ggrepel)

ui <- fluidPage(
  
  h2("Botanic Plot Mapper"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "csv_to_use", 
        label = "Please upload a csv file", 
        multiple = FALSE, 
        accept = c("csv", "text", "csv/text"), 
        width = "225px"
      ), 
      br(),
      textInput(
        inputId = "my_title", 
        label = "Name of botanic plot:", 
        value = "", 
        width = "225px"
      ),
      br(),
      selectInput(
        inputId = "my_colors",
        label = "Pick a color", 
        choices = c("darkgreen", "green4", "forestgreen", "darkolivegreen"), 
        selected = "forestgreen", 
        multiple = FALSE, 
        width = "225px"
      ),
    ),
    mainPanel(
      plotOutput(
        outputId = "plt1", 
        width = "400px", 
        height = "400px"
      ) 
    )
  )
)

server <- function(input, output) {
  
  
  output$plt1 <- renderPlot({
    
    req(input$csv_to_use)
    
    csv_meta_info <- input$csv_to_use
    
    my_data <- read.csv(file = csv_meta_info$datapath)
    
    if (input$my_title == "") {
      
      title <- "*** Please enter the name of your botanic plot ***"
      
    } else {
      
      title <- input$my_title
      
    }
    
    ggplot(my_data, aes(x, y, size = DBH_cm_2009)) +
      geom_point(
        color = input$my_colors
      ) + 
      ggtitle(
        label = title
      ) +
      scale_y_continuous(
        breaks = seq(0, 50, by = 25)
      ) +
      scale_x_continuous(
        breaks = seq(0, 50, by = 25)
      ) +
      geom_text_repel(
        aes(label = tree_ID),
        fontface = "bold",
        nudge_x = 1,
        nudge_y = 0,
        size = 2
      ) +
      theme_bw() +
      theme(
        aspect.ratio = 1,
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "darkgrey"),
        legend.position = "nones",
        legend.title = element_blank(),
        rect = element_blank(),
        axis.ticks.length = unit(0, "pt")
      ) 
  })
}
shinyApp(ui = ui, server = server)



