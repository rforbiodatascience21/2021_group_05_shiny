#
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Manhattan-plot, Gravier Data"),
    
    mainPanel(
        h2("This plot show the negative log10 p-value of the gene expression, for 100 random chosen genes in the gravier data set.
           Use the sliderbar to change the significance level. The colors of the genes correspond to a significance level of 0.05")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Value",
                        " p-value",
                        min = 0,
                        max = 1,
                        value = 0.05)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        library("tidyverse")
        data<- readRDS("gravier_data_long_nested.rds")
        
        data %>% 
            ggplot(mapping = aes(x = fct_reorder(gene, neg_log10_p, .desc = TRUE), 
                                 y = neg_log10_p,
                                 color = identified_as)) +
            geom_point() + 
            geom_hline(yintercept = - log10(input$Value), linetype = "dashed") +
            theme_classic(base_size = 8, base_family = "Avenir") +
            theme(legend.position = "bottom", 
                  axis.text.x = element_text(angle = 45 , vjust = 1, hjust = 1)) +
            labs( x = "Gene", y = "Minus log10(p)")
        
        
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
