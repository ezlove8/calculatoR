## BAC CalculatoR: a Shiny app for estimating blood alcohol concentration
## Authors: Kayleigh Adamson, Jennifer Fedor, Elijah Lovelace 

# install necessary packages if the user does not already have them installed
#install.packages('shiny')
#install.packages('shinydashboard')
#install.packages('ggplot2')
#install.packages("devtools") 

#devtools necessary to run uber
#devtools::install_github("hadley/httr")
#devtools::install_github("DataWookie/ubeR")


# load necessary packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ubeR)

## uber
UBER_CLIENTID <- 'ZYPpRM5kjc4OqLzz_kSt8b0UzPsxO9kO'
UBER_CLIENTSECRET <- 'KQCMA9wBVxL93sIk4TzqjW2kIAerpQnTviPLGY31'

uber_oauth(UBER_CLIENTID, UBER_CLIENTSECRET)

#current uber price estimate based on u pitt to ppg arena, edit location per request
estimate<- uber_requests_estimate(start_latitude = 40.222, start_longitude = -78.836647,
                                  end_latitude = 40.440110, end_longitude = -80.00320)
names(estimate)
uber<- estimate$pickup_estimate


# create Shiny dashboard interface
body <- dashboardBody(
  fluidRow(
    # plot output of BAC
    box(title = "Here's a plot of your BAC over the next few hours:", width = 12, plotOutput("plot"))),
  
  fluidRow(
    valueBoxOutput("BACBox"),
    valueBoxOutput("RecommendationBox"),
    valueBoxOutput("UberBox")
  ),

  fluidRow(
    # inputs for gender and weight
    box(
      title = "Enter information about you below:", width = 6, solidHeader = TRUE,
      selectInput("sex", "Sex:", selected = "f",
                  c("Female" = 0.66, 
                    "Male" = 0.73)),
      sliderInput("weight", "Body weight (lbs):", value = 100, 
                  min = 50, 
                  max = 350, 
                  step = 1)
    ),
    
    box(
      # inputs for drink information
      title = "Enter information about what you're drinking below:", width = 6, solidHeader = TRUE,
      selectInput("type", "What are you drinking?", selected = "beer",
                  c("Beer" = 0.045,
                    "Wine" = 0.115,
                    "Liquor" = 0.45)),
      sliderInput("amount", "How much (in ounces) are you drinking?", value = 5,
                  min = 0,
                  max = 50,
                  step = 0.5),
      HTML("<I>Helpful hints:</I>"),
      HTML("<ul>
              <li>A shot of liquor is about 1.5 ounces</li>
              <li>A glass of wine is about 6 ounces</li>
              <li>A can of beer is about 12 ounces</li>
              <li>A pint of beer is about 16 ounces</li>
            </ul>"),
      sliderInput("hours", "How long (in hours) have you been drinking?", value = 1,
                  min = 0,
                  max = 24,
                  step = 0.5)
      )
    )
)

# display dashboard in ui
ui <- dashboardPage(
  dashboardHeader(title = HTML("<strong>BAC CalculatoR</strong>")),
  dashboardSidebar(collapsed=TRUE),
  body
)

# define server logic
server <- function(input, output) {
   
est_bac <- function(weight, hours, type, amount, sex) {
  ebac = ((type*amount*5.14)/(weight * sex))-(0.015*hours)
  ifelse(ebac >0, ebac, 0)
}  

bac <- reactive({
  round(est_bac(input$weight, 
                input$hours, 
                as.numeric(input$type), 
                input$amount, 
                as.numeric(input$sex)), 4)
}) 

recommend <- reactive({
  if (as.numeric(est_bac(input$weight, 
                         input$hours, 
                         as.numeric(input$type), 
                         input$amount, 
                         as.numeric(input$sex))) >= 0.2) {
    "Go to the ER"
  } else if (as.numeric(est_bac(input$weight, 
                                input$hours, 
                                as.numeric(input$type), 
                                input$amount, 
                              as.numeric(input$sex))) >= 0.08) {
    "Nope!"
  } else {
    "Sure"
  }
})

  output$BACBox <- renderValueBox({
    valueBox(
      paste0(bac()), "Your BAC", icon = icon("glass-martini-alt"),
      color = "red"
    )
  })
  
  output$RecommendationBox <- renderValueBox({
    valueBox(
      paste0(recommend()), "Should you drive?", icon = icon("beer"),
      color = "blue"
    )
  })
  
  output$UberBox <- renderValueBox({
    valueBox(
      paste0("$", uber), "Estimated Uber fare", icon = icon("wine-glass-alt"), 
      color = "teal"
    )
  })
  
  
  # add BAC calc and plot  
  output$plot <- renderPlot({
    hrs <- seq(0, 24)
  plot_bac <- vector(length = 25)
  for (i in hrs) {
    
    plot_bac[i] <- est_bac(input$weight, 
                                       (input$hours+i), 
                                       as.numeric(input$type), 
                                       input$amount, 
                                       as.numeric(input$sex))
    
  }
    # plot_bac <- sapply(hrs, function(x) est_bac(input$weight, 
    #                                     input$hours, 
    #                                     as.numeric(input$type), 
    #                                     input$amount, 
    #                                     as.numeric(input$sex)
    #                                   
    #                                 ))
    df <-cbind.data.frame(hrs, plot_bac)
    ggplot(df, aes(x = hrs, y = plot_bac)) + 
      geom_line() + 
      
      xlab("Hours") +
      ylab("BAC (%)") +
      geom_line(data = df, color = "black") +
      #geom_text(data = df, label = "Your level", vjust = -1, hjust = 0) +
      geom_ribbon(aes(x = hrs, ymin = 0, ymax = 0.08, linetype = NA, fill = "green"), alpha = 0.2) +
      scale_fill_manual(values=c("green"="green")) +
      theme(legend.position = "none")
  }) 
}


# run the application 
shinyApp(ui = ui, server = server)

