# Load required packages
library(shiny)
library(dplyr)
library(ggplot2)
library(corrplot)
library(Lahman)

# Load batting data from Lahman Baseball Database
batting <- read.csv("Batting.csv") %>% 
  select(playerID,yearID,stint,teamID,lgID,G,AB,R,H,X2B,X3B,HR) %>% 
  rename("Player_ID" = playerID,
         "Year_ID" = yearID,
         "Stint" = stint,
         "Team" = teamID,
         "League" = lgID,
         "Games_Played" = G,
         "At_Bats" = AB,
         "Runs" = R,
         "Hits" = H,
         "Doubles" = X2B,
         "Triples" = X3B,
         "Home_Runs" = HR)
people <- read.csv("People.csv") %>% 
  select(playerID,birthYear,birthMonth,birthDay,birthCountry,birthState,birthCity,nameFirst,nameLast,nameGiven,weight,height,bats,throws,debut,finalGame) %>% 
  rename("Player_ID" = playerID,
         "Birth_Year" = birthYear,
         "Birth_Month" = birthMonth,
         "Birth_Day" = birthDay,
         "Birth_Country" = birthCountry,
         "Birth_State" = birthState,
         "Birth_City" = birthCity,
         "First_Name" = nameFirst,
         "Last_Name" = nameLast,
         "Given_Name" = nameGiven,
         "Weight" = weight,
         "Height" = height,
         "Batting_Hand" = bats,
         "Throwing_Hand" = throws,
         "Debut" = debut,
         "Final_Game" = finalGame)

#Merging both batting and people dataset to get one final dataset
final_data <- merge(batting, people, by = "Player_ID")

# Create new variable for batting average
final_data$Average <- ifelse(is.nan(final_data$Hits / final_data$At_Bats),0,final_data$Hits / final_data$At_Bats)

# Remove missing data
final_data <- na.omit(final_data)

#Create a new variable for age
final_data$Current_Age <- as.integer((Sys.Date() - as.Date(paste(final_data$Birth_Year, final_data$Birth_Month, final_data$Birth_Day, sep = "-"))) / 365.25)
final_data$Debut_Age <- as.integer((Sys.Date() - as.Date(final_data$Debut)) / 365.25)
final_data$FinalGame_Age <- as.integer((Sys.Date() - as.Date(final_data$Final_Game)) / 365.25)
final_data <-  final_data%>% filter(At_Bats > 30)
model <- lm(Average ~ Games_Played +
              At_Bats + 
              Hits, data = final_data %>% filter(At_Bats > 30))

# Create a correlation matrix to identify potential predictors
cor(final_data[,c("Average", "At_Bats", "Hits", "Games_Played")])

# Print summary of model results
summary(model)

# Create scatterplot of predicted versus actual batting average
predicted <- predict(model, newdata = final_data %>% filter(At_Bats > 30))
ggplot(final_data %>% filter(At_Bats > 30), aes(x = Average, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual Batting Average", y = "Predicted Batting Average")

# Define user interface
ui <- fluidPage(
  titlePanel("Influential Factors on Baseball Batting Average"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("options", "Select Options:",
                         choices = c("Games_Played", "At_Bats", "Hits", "Position"),
                         selected = "Games_Played")
    ),
    mainPanel(
      h4("Multiple Linear Regression Model:"),
      verbatimTextOutput("selectedoptions"),
      plotOutput("mlr"),
      plotOutput("rdp")
    )
  )
)

# Define server logic
server <- function(input, output) {
  independent_vars <- reactive({
    input$options
  })
  output$selectedoptions <- renderPrint({
    paste("You have selected the following options:", paste(input$options, collapse = "+ "))
  })
  output$mlr <- renderPlot({

    model <- lm(Average ~., data = final_data[, c("Average", independent_vars())])
    predicted <- predict(model, newdata = final_data %>% filter(At_Bats > 30))
    ggplot(final_data, aes(x = Average, y = predicted)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, color = "red") +
      labs(x = "Actual Batting Average", y = "Predicted Batting Average")
  })
  output$rdp <- renderPlot({
    qqnorm(model$residuals)
    qqline(model$residuals)
  })
}

# Run the app
shinyApp(ui, server)

