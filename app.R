#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(caret)
library(ranger)
library(randomForest)
library(ROCR)

# Read the data
my_data = read_csv("https://data.urbanaillinois.us/resource/afbd-8beq.csv?$limit=999999")

# Data cleaning and feature engineering
my_data = my_data %>% 
  filter(year_of_arrest >= 2000) %>% 
  filter(age_at_arrest > 0) %>% 
  select(incident_number, year_of_arrest, month_of_arrest, 
         crime_category_description, age_at_arrest, arrestee_sex, arrestee_race, 
         arrestee_employment_description, arrestee_home_city, arrestee_home_state) %>% 
  filter(arrestee_sex == "MALE" | arrestee_sex == "FEMALE") %>% 
  drop_na() %>% 
  mutate(homecity = ifelse((arrestee_home_city == "CHAMPAIGN" | arrestee_home_city == "URBANA"), arrestee_home_city, "OTHERS" ))

set.seed(447)

model_data = my_data %>% 
  select(month_of_arrest, crime_category_description, age_at_arrest, arrestee_sex, arrestee_race, arrestee_employment_description, homecity)

## dropping crimes that happened too few
crimes = model_data %>% 
  group_by(crime_category_description) %>% 
  count() %>% 
  filter(n > 500) %>% 
  select(crime_category_description)

m_data = filter(model_data, crime_category_description %in% as.vector(unlist(crimes)))

trainIndex <- createDataPartition(1:nrow(m_data), p = 0.7, list = FALSE)
train <- m_data[trainIndex, ]
test <- m_data[-trainIndex, ]


trControl <- trainControl(method = "cv",
                          number = 2,
                          search = "grid")

set.seed(1234)
# Run the model
rf_default <- train(as.factor(crime_category_description)~.,
                    data = train,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = trControl)

print(rf_default)

# best mtry 8

prediction <-predict(rf_default, test)




# EDA plots
num_summary <- my_data %>% 
  select(where(is.numeric)) %>% 
  summary()

plot1 <- my_data %>% 
  count(year_of_arrest) %>% 
  ggplot(aes(x = year_of_arrest, y = n)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=2) +
  ggtitle("Number of Arrests in Urbana-Champaign since 2000") +
  labs(x = "Year", y = "Number of Arrests")

plot2 <- my_data %>% 
  ggplot(aes(x = year_of_arrest, fill = arrestee_sex)) +
  geom_histogram(binwidth = 0.5, color="#e9ecef", alpha=0.9) +
  ggtitle("Arrestee Sex in Urbana-Champaign since 2000") +
  labs(x = "Year", y = "Number of Arrests")

plot3 <-my_data %>% 
  group_by(year_of_arrest) %>% 
  count(arrestee_race) %>% 
  ggplot(aes(x = year_of_arrest, y = n, fill = arrestee_race)) +
  geom_area(alpha=0.6, linewidth = 0.1, colour = "black") +
  ggtitle("Arrestee Race in Urbana-Champaign since 2000") +
  labs(x = "Year", y = "Number of Arrests")

plot4 <- my_data %>% 
  ## Graph to show proportions of arrestee home city in each year
  group_by(year_of_arrest) %>% 
  count(homecity) %>% 
  ggplot(aes(x = year_of_arrest,  y = n, fill = homecity)) +
  geom_area(alpha=0.6, linewidth = 0.1, colour = "black")  +
  ggtitle("Arrestee Home City in Urbana-Champaign since 2000") +
  labs(x = "Year", y = "Number of Arrests")

plot5 <- my_data %>% 
  ggplot(aes(x = as.factor(year_of_arrest), y = age_at_arrest)) +
  geom_boxplot(fill = "slateblue", alpha = 0.4) +
  ggtitle("Arrestee Age in Urbana-Champaign since 2000") +
  labs(x = "Year", y = "Age of Arrests")

plot6 <- my_data %>% 
  ggplot(aes(x = arrestee_race, fill = arrestee_sex)) +
  geom_bar(position = "stack") +
  labs(title = "Arrestee Gender by Race", x = "Race", y = "Count")

# Model fit



ui <- fluidPage(
  # Title
  titlePanel("Exploring the Urbana-Champaign Arrest Dataset"),
  
  # Sidebar with input
  sidebarLayout(
    sidebarPanel(
      h4("Input for Prediction"),
      numericInput("age_input", "Age at Arrest:", min = 10, max = 100, value = 30),
      selectInput("sex_input", "Arrestee Sex:", choices = c("MALE", "FEMALE"), selected = "MALE"),
      selectInput("race_input", "Arrestee Race:", choices = c("ASIAN", "BLACK", "HISPANIC", "OTHER", "WHITE"), selected = "WHITE")
      ),
    
    
    # Main panel with plots and model fit
    mainPanel(
      h4("Prediction"),
      h4("EDA Plots"),
      plotOutput("plot1"),
      plotOutput("plot2"),
      plotOutput("plot3"),
      plotOutput("plot4"),
      plotOutput("plot5"),
      plotOutput("plot6")
      

    )
  )
)

# Define server logic
# Server function
server <- function(input, output) {
  # Create a reactive expression for the filtered data based on user input
  filtered_data <- reactive({
    # Create a data frame with the user input
    new_data <- data.frame(
      age_at_arrest = input$age_input,
      arrestee_sex = input$sex_input,
      arrestee_race = input$race_input
    )
    
    # Use the trained random forest model to predict the crime category for the new data
    prediction <- predict(rf_default, new_data)
    
    # Return the predicted crime category
    return(prediction)
  })
  
  # Output the predicted crime category to the UI
  output$model_fit <- renderText({
    paste("Predicted Crime Category: ", filtered_data())
  })
  
  # Render plots
  output$plot1 <- renderPlot(plot1)
  output$plot2 <- renderPlot(plot2)
  output$plot3 <- renderPlot(plot3)
  output$plot4 <- renderPlot(plot4)
  output$plot5 <- renderPlot(plot5)
  output$plot6 <- renderPlot(plot6)

  

}



# Run the application 
shinyApp(ui = ui, server = server)
