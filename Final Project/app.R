library(shiny)
library(jsonlite)
library(tidyverse)
library(shinycssloaders)

# Get the endpoint
makeup <- fromJSON("http://makeup-api.herokuapp.com/api/v1/products.json")
makeup$price <- as.numeric(makeup$price)

b <- makeup %>% distinct(brand) %>% arrange(brand) %>% drop_na()
t <- makeup %>% distinct(product_type) %>% arrange(product_type) %>% drop_na()

ui <- fluidPage(
    
    titlePanel('Fliter Product'),
    sidebarLayout(
        sidebarPanel(
            selectInput('product_type', 'Product_Type', c('-',set_names(t$product_type))),
            selectInput('brand', 'Brand', c('-',set_names(b$brand))),
            selectInput('price_greater', 'Price_Greater_Than', c('-',1:77)),
            selectInput('price_less', 'Price_Less_Than', c('-',1:77)),
            textInput('name',' Search Name Directly','Diorshow Mono'),
            width = 3,
            actionButton("act", "Search!")
        ), 
        mainPanel(
            tabsetPanel(
                tabPanel('Price and Rating Difference Among Brands For Selected Product Type',withSpinner(plotOutput('plot1')),withSpinner(plotOutput('plot2'))),
                tabPanel('Proportion of Product Type in Selected Brand', withSpinner(plotOutput('plot3'))),
                tabPanel('Filter Result',withSpinner(tableOutput('table1'))),
                tabPanel('Search by Name',withSpinner(tableOutput('table2')),withSpinner(uiOutput('image')))
            ),
            width = 9
        ),
    )
)

server <- function(input, output,session) {
    
    # Change the input$price_less based on the input$price_greater
    observeEvent(input$price_greater, {
        if (input$brand == '-' & input$product_type != '-' & input$price_greater == '-' & input$price_less == '-'){
            num <- c(input$price_greater:77)
            updateSelectInput(session,'price_less',choices = c('-',num))
        }
    })
    
    # Plot of Price Difference Among Brands For Same Product Type
    output$plot1 <- renderPlot({
        input$act
        isolate(
            if (input$product_type != '-'){
                makeup %>%
                    filter(product_type == input$product_type) %>%
                    ggplot(aes(x=brand, y = price))+
                    geom_point()+
                    theme(axis.text.x = element_text(angle = 90, hjust = 1))
            }
        )
    })
    
    # Plot of Rating Difference Among Brands For Same Product Type
    output$plot2 <- renderPlot({
        input$act
        isolate(
            if (input$product_type != '-'){
                makeup %>%
                    filter(product_type == input$product_type) %>%
                    ggplot(aes(x=brand, y = rating))+
                    geom_point()+
                    theme(axis.text.x = element_text(angle = 90, hjust = 1))
            }
        )
    })
    
    output$plot3 <- renderPlot({
        input$act
        isolate(
            if (input$brand != '-'){
                makeup %>%
                    filter(brand == input$brand) %>%
                    group_by(product_type) %>% 
                    count () %>%
                    ggplot(aes(x = "", y = n, fill = product_type)) +
                    geom_bar(width = 1, stat = "identity", color = "white") +
                    coord_polar("y", start = 0)+
                    theme_void()
            }
        )
    })
    
    # Filter Part based on brand, product_type, price_greater, price_less
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
                    filter(price > as.numeric(input$price_greater)) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand == '-' & input$product_type == '-' & input$price_greater == '-' & input$price_less != '-'){
                makeup %>%
                    filter(price < as.numeric(input$price_less)) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand != '-' & input$product_type != '-' & input$price_greater == '-' & input$price_less == '-'){
            makeup %>%
                filter(brand == input$brand & product_type == input$product_type) %>%  
                select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand != '-' & input$product_type == '-' & input$price_greater != '-' & input$price_less == '-'){
                makeup %>%
                    filter(brand == input$brand & price > as.numeric(input$price_greater)) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand != '-' & input$product_type == '-' & input$price_greater == '-' & input$price_less != '-'){
                makeup %>%
                    filter(brand == input$brand & price < as.numeric(input$price_less)) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand == '-' & input$product_type != '-' & input$price_greater != '-' & input$price_less == '-'){
                makeup %>%
                    filter(product_type == input$product_type & price > as.numeric(input$price_greater)) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand == '-' & input$product_type != '-' & input$price_greater == '-' & input$price_less != '-'){
                makeup %>%
                    filter(product_type == input$product_type & price < as.numeric(input$price_less)) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand == '-' & input$product_type == '-' & input$price_greater != '-' & input$price_less != '-'){
                makeup %>%
                    filter(price > input$price_greater & price < as.numeric(input$price_less)) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand != '-' & input$product_type != '-' & input$price_greater != '-' & input$price_less == '-'){
                makeup %>%
                    filter(brand == input$brand & product_type == input$product_type & price > as.numeric(input$price_greater)) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand != '-' & input$product_type != '-' & input$price_greater == '-' & input$price_less != '-'){
                makeup %>%
                    filter(brand == input$brand & product_type == input$product_type & price < as.numeric(input$price_less)) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand != '-' & input$product_type == '-' & input$price_greater != '-' & input$price_less != '-'){
                makeup %>%
                    filter(brand == input$brand & price > as.numeric(input$price_greater) & as.numeric(price < input$price_less)) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand == '-' & input$product_type != '-' & input$price_greater != '-' & input$price_less != '-'){
                makeup %>%
                    filter(product_type == input$product_type & price > as.numeric(input$price_greater) & as.numeric(price < input$price_less)) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
            else if (input$brand != '-' & input$product_type != '-' & input$price_greater != '-' & input$price_less != '-'){
                makeup %>%
                    filter(brand == input$brand & product_type == input$product_type & price > as.numeric(input$price_greater) & price < as.numeric(input$price_less)) %>%  
                    select(brand,name,product_type,price,rating,description)
            }
        )
    })
    
    # Search Name Part
    output$table2 <- renderTable({
        input$act
        isolate(
            makeup %>%
                filter(tolower(name) == tolower(input$name)) %>%
                select(brand,name,product_type,price,rating,description)
        )
    })
    
    # Images for Product
    l <- reactive({
        unlist(makeup %>%
        filter(tolower(name) == tolower(input$name)) %>%
        select(image_link))
    })
    
    output$image <- renderUI({
        input$act
        isolate(
           tags$img(src = l())
        )
    })
}

shinyApp(ui = ui, server = server)
