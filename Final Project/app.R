#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(jsonlite)
library(tidyverse)

makeup <- fromJSON("http://makeup-api.herokuapp.com/api/v1/products.json")
makeup$price <- as.numeric(makeup$price)
b <- makeup %>% distinct(brand) %>% arrange(brand) %>% drop_na()
t <- makeup %>% distinct(product_type) %>% arrange(product_type) %>% drop_na()

ui <- fluidPage(
    
    headerPanel('Fliter Product'),
    sidebarPanel(
        selectInput('product_type', 'Product_Type', c('-',set_names(t$product_type))),
        selectInput('brand', 'Brand', c('-',set_names(b$brand))),
        selectInput('price_greater', 'Price_Greater_Than', c('-',1:77)),
        selectInput('price_less', 'Price_Less_Than', c('-',1:77)),
        textInput('name','Name','Diorshow Mono'),
        width = 3,
        actionButton("act", "Search!")
    ), 
    mainPanel(
        plotOutput('plot1'),
        plotOutput('plot2'),
        tableOutput('table1'),
        tableOutput('table2'),
        width = 9
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    observeEvent(input$price_greater, {
        if (input$price_greater != '-'){
            num <- c(input$price_greater:77)
            updateSelectInput(session,'price_less',choices = c('-',num))
        }
    })
    
    output$plot1 <- renderPlot({
        input$act
        isolate(
            if (input$brand == '-' & input$product_type != '-' & input$price_greater == '-' & input$price_less == '-'){
                makeup %>%
                    filter(product_type == input$product_type) %>%
                    ggplot(aes(x=brand, y = price))+geom_point()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
            }
        )
    })
    
    output$plot2 <- renderPlot({
        input$act
        isolate(
            if (input$brand == '-' & input$product_type != '-' & input$price_greater == '-' & input$price_less == '-'){
                makeup %>%
                    filter(product_type == input$product_type) %>%
                    ggplot(aes(x=brand, y = rating))+geom_point()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
            }
        )
    })
    
    output$table1 <- renderTable({
        input$act
        isolate(
            if (input$brand != '-' & input$product_type == '-' & input$price_greater == '-' & input$price_less == '-'){
                makeup %>%
                    filter(brand == input$brand) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand == '-' & input$product_type != '-' & input$price_greater == '-' & input$price_less == '-'){
                makeup %>%
                    filter(product_type == input$product_type) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand == '-' & input$product_type == '-' & input$price_greater != '-' & input$price_less == '-'){
                makeup %>%
                    filter(price > input$price_greater) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand == '-' & input$product_type == '-' & input$price_greater == '-' & input$price_less != '-'){
                makeup %>%
                    filter(price < input$price_less) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand != '-' & input$product_type != '-' & input$price_greater == '-' & input$price_less == '-'){
            makeup %>%
                filter(brand == input$brand & product_type == input$product_type) %>%  
                select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand != '-' & input$product_type == '-' & input$price_greater != '-' & input$price_less == '-'){
                makeup %>%
                    filter(brand == input$brand & price > input$price_greater) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand != '-' & input$product_type == '-' & input$price_greater == '-' & input$price_less != '-'){
                makeup %>%
                    filter(brand == input$brand & price < input$price_less) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand == '-' & input$product_type != '-' & input$price_greater != '-' & input$price_less == '-'){
                makeup %>%
                    filter(product_type == input$product_type & price > input$price_greater) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand == '-' & input$product_type != '-' & input$price_greater == '-' & input$price_less != '-'){
                makeup %>%
                    filter(product_type == input$product_type & price < input$price_less) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand == '-' & input$product_type == '-' & input$price_greater != '-' & input$price_less != '-'){
                makeup %>%
                    filter(price > input$price_greater & price < input$price_less) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand != '-' & input$product_type != '-' & input$price_greater != '-' & input$price_less == '-'){
                makeup %>%
                    filter(brand == input$brand & product_type == input$product_type & price > input$price_greater) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand != '-' & input$product_type != '-' & input$price_greater == '-' & input$price_less != '-'){
                makeup %>%
                    filter(brand == input$brand & product_type == input$product_type & price < input$price_less) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand != '-' & input$product_type == '-' & input$price_greater != '-' & input$price_less != '-'){
                makeup %>%
                    filter(brand == input$brand & price > input$price_greater & price < input$price_less) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand == '-' & input$product_type != '-' & input$price_greater != '-' & input$price_less != '-'){
                makeup %>%
                    filter(product_type == input$product_type & price > input$price_greater & price < input$price_less) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand != '-' & input$product_type != '-' & input$price_greater != '-' & input$price_less != '-'){
                makeup %>%
                    filter(brand == input$brand & product_type == input$product_type & price > input$price_greater & price < input$price_less) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
        )
    })
    
    output$table2 <- renderTable({
        input$act
        isolate(
            makeup %>%
                filter(tolower(name) == tolower(input$name)) %>%  
                select(brand,name,product_type,price,rating,description)
        )
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
