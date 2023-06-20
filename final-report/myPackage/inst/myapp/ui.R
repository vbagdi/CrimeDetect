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
            textOutput("model_fit"),
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
