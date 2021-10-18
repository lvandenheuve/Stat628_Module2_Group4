#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Body Fat Percentage Calculator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            "*This calulator is only accurate for adult males*",
            sliderInput("age", "Age", value = 30, min = 20, max = 100, step = 1),
            sliderInput("weight", "Weight in LBs:", min = 110, max = 400, value = 200, ticks = FALSE),
            sliderInput("ab_circ", "Abdomen Circumference (inches)", min = 26.50, max = 60, value = 36, step = 0.25, ticks = FALSE),
            actionButton("Submit", icon("fas fa-magic"), label = "Submit")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("sliderValues"),
           h2(textOutput("bodyfat_percent")),
           textOutput("guidance"),
           uiOutput(outputId = "bf_range"),
           textOutput("whitespace"),
           textOutput("contact_info"),
           textOutput("disclaimer"),
           textOutput("contact_emails"),
           textOutput("contact_numbers")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$sliderValues <- renderTable({
        df()
    })
    df <- eventReactive(input$Submit,{
        return(data.frame(
            Name = c("Weight",
                     "Abdomen Circumference"),
            Value = as.character(c(paste(input$weight, "lbs"),
                                   paste(input$ab_circ, "inches"))),
            stringsAsFactors = FALSE))
    })
    myVal <- eventReactive(input$Submit,{
        bodyfat_percent = round(-62.51127 + 2.860768*input$ab_circ - 0.014148*input$weight - 0.00305*input$ab_circ*input$weight, digits = 2)
        bf_text <- paste("Your bodyfat is at", bodyfat_percent, "%")
        return(bf_text)
    })
    user_guidance <- eventReactive(input$Submit,{
        return("Please check with the chart below to determine your bodyfat health category for your age group")
    })
    output$bodyfat_percent <- renderText({
        paste(myVal())
    })
    output$guidance <- renderText({
        user_guidance()
    })
    output$whitespace <- renderText({
        "__________________________________________________________________________________________________________________"
    })
    output$contact_info <- renderText({
        "Contact Information:"
    })
    output$disclaimer <- renderText({
        "Feel free to contact us about questions or concerns via our emails or phone number listed below!"
    })
    output$contact_emails <- renderText({
        "Emails: Luke ~ lvandenheuve@wisc.edu, Shuguang ~ schen777@wisc.edu, Xinyue ~ xzhu357@wisc.edu"
    })
    output$contact_numbers <- renderText({
        "Phone Number: Luke ~ (920) 676-8854, Shuguang ~ (608) 977-4628, Xinyue ~ (608) 724-4080"
    })
    observe({
        input_weight <- input$weight
        starting_val = round(((20+62.51127+0.014148*input_weight)/(2.860768-0.00305*input_weight)))
        ab_min = round(((3+62.51127+0.014148*input_weight)/(2.860768-0.00305*input_weight)))
        ab_max = round(((58+62.51127+0.014148*input_weight)/(2.860768-0.00305*input_weight)))
        updateSliderInput(session = getDefaultReactiveDomain(),"ab_circ", value = starting_val,
                          min = ab_min, max = ab_max, step = 0.25)
    })
    pic_val <- eventReactive(input$Submit,{
        if(input$age>19 && input$age<30) {
            id <- '20_29.png'
        }
        else if(input$age>29 && input$age<40) {
            id <- '30_39.png'
        }
        else if(input$age>39 && input$age<50) {
            id <- '40_49.png'
        }
        else if(input$age>49 && input$age<60) {
            id <- '50_59.png'
        }
        else {
            id <- 'over_60.png'
        }
        return(id)
    })
    output$bf_range <- renderUI({
            img(src=pic_val(), height = '300px')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
