library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(caret)
library(ranger)
library(randomForest)
library(ROCR)
library(myPackage)

my_data = myPackage::load_data()

my_data = myPackage::process_data(my_data)

plot1 = myPackage::tn_plot(my_data)
plot2 = myPackage::ts_plot(my_data)
plot3 = myPackage::tr_plot(my_data)
plot4 = myPackage::th_plot(my_data)
plot5 = myPackage::ta_plot(my_data)
plot6 = myPackage::sr_plot(my_data)

set.seed(447)

model_data = myPackage::model_data(my_data)
rf_model = myPackage::model(model_data)

# Define server logic
# Server function
server <- function(input, output) {
    # Create a reactive expression for the filtered data based on user input
    filtered_data <- reactive({
        # Create a data frame with the user input
        new_data <- data.frame(
            age_at_arrest = input$age_input,
            arrestee_sex = input$sex_input,
            arrestee_race = input$race_input,
            month_of_arrest = 6,
            arrestee_employment_description = "EMPLOYED",
            homecity = "URBANA"
        )

        # Use the trained random forest model to predict the crime category for the new data
        prediction <- predict(rf_model, new_data)

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
