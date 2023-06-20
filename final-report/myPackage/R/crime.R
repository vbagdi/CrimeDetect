library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(caret)
library(ranger)
library(randomForest)
library(ROCR)
#' Load dynamic crime data in Champaign
#'
#' load the crime data in Champaign
#'
#' @param
#' @return newly updated crime data in Urbana Open data portal
#' @export
load_data = function() {
    my_data = read_csv("https://data.urbanaillinois.us/resource/afbd-8beq.csv?$limit=999999")
    my_data
}

#' Cleaning the data and feature engineering
#'
#' clean the data and select specific features.
#'
#' @param raw_data orginal data directly from the open data protal
#' @return tidy data that can be used for EDA
#' @export
process_data = function(my_data) {
    new_data = my_data %>%
        ## Use only observation since year 2000
        filter(year_of_arrest >= 2000) %>%
        ## filter age > 0
        filter(age_at_arrest > 0) %>%
        ## select variables
        select(incident_number, year_of_arrest, month_of_arrest,
               crime_category_description, age_at_arrest, arrestee_sex, arrestee_race,
               arrestee_employment_description, arrestee_home_city, arrestee_home_state) %>%
        filter(arrestee_sex == "MALE" | arrestee_sex == "FEMALE") %>%
        drop_na()

    # Check CHAMPAIGN spelling
    unique(str_extract_all(my_data$arrestee_home_city, pattern = "\\bCHAMPA\\w*", simplify = TRUE))

    ## Check URBANA spelling
    unique(str_extract_all(my_data$arrestee_home_city, pattern = "\\bURBA\\w*", simplify = TRUE))

    ## Replace wrong spelling
    my_data$arrestee_home_city = str_replace_all(my_data$arrestee_home_city,
                                                 pattern = "\\bCHAMPA\\w*",
                                                 replacement = "CHAMPAIGN")
    my_data$arrestee_home_city = str_replace_all(my_data$arrestee_home_city,
                                                 pattern = "URBAAN",
                                                 replacement = "URBANA")
    my_data$arrestee_home_city = str_replace_all(my_data$arrestee_home_city,
                                                 pattern = "URBANQA",
                                                 replacement = "URBANA")
    my_data$arrestee_home_city = str_replace_all(my_data$arrestee_home_city,
                                                 pattern = "URBANAI",
                                                 replacement = "URBANA")
    my_data$arrestee_home_city = str_replace_all(my_data$arrestee_home_city,
                                                 pattern = "URBAN",
                                                 replacement = "URBANA")
    my_data$arrestee_home_city = str_replace_all(my_data$arrestee_home_city,
                                                 pattern = "URBANAA",
                                                 replacement = "URBANA")
    ## adding indicator of whether home_city is CHAMPAIGN or URBANA or Others
    new_data = new_data %>%
        mutate(homecity = ifelse((arrestee_home_city == "CHAMPAIGN" | arrestee_home_city == "URBANA"),
                                 arrestee_home_city, "OTHERS" ))

    new_data
}

#' Create visulization about relation between time and number of arrests
#'
#' product plot of number of arrests along the time
#'
#' @param process_data data processed by process_data()
#' @return plot of number of arrests along the time
#' @export
tn_plot= function(process_data) {
    # number of cases versus time (year)
    process_data %>%
        count(year_of_arrest) %>%
        ggplot(aes(x = year_of_arrest, y = n)) +
        geom_line( color="grey") +
        geom_point(shape=21, color="black", fill="#69b3a2", size=2) +
        ggtitle("Number of Arrests in Urbana-Champaign since 2000") +
        labs(x = "Year", y = "Number of Arrests")
}

#' Create visulization to see in each year arrestees sex proportion
#'
#' Graph to see in each year arrestees sex proportion
#'
#' @param process_data data processed by process_data()
#' @return plot of arrestees sex proportion in each year
#' @export
ts_plot = function(process_data) {
    # number of cases versus time (year)
    process_data %>%
        ggplot(aes(x = year_of_arrest, fill = arrestee_sex)) +
        geom_histogram(binwidth = 0.5, color="#e9ecef", alpha=0.9) +
        ggtitle("Arrestee Sex in Urbana-Champaign since 2000") +
        labs(x = "Year", y = "Number of Arrests")
}

#' Create visulization to see in each year arrestee race proportion
#'
#' Graph to see in each year arrestees race proportion
#'
#' @param process_data data processed by process_data()
#' @return plot of arrestees race proportion in each year
#' @export
tr_plot = function(process_data) {
    # number of cases versus time (year)
    process_data %>%
        group_by(year_of_arrest) %>%
        count(arrestee_race) %>%
        ggplot(aes(x = year_of_arrest, y = n, fill = arrestee_race)) +
        geom_area(alpha=0.6, linewidth = 0.1, colour = "black") +
        ggtitle("Arrestee Race in Urbana-Champaign since 2000") +
        labs(x = "Year", y = "Number of Arrests")
}

#' Create visulization to see proportions of arrestee home city in each year
#'
#' Graph to show proportions of arrestee home city in each year
#'
#' @param process_data data processed by process_data()
#' @return plot of arrestees homecity proportion in each year
#' @export
th_plot = function(process_data) {
    # number of cases versus time (year)
    process_data %>%
        group_by(year_of_arrest) %>%
        count(homecity) %>%
        ggplot(aes(x = year_of_arrest,  y = n, fill = homecity)) +
        geom_area(alpha=0.6, linewidth = 0.1, colour = "black")  +
        ggtitle("Arrestee Home City in Urbana-Champaign since 2000") +
        labs(x = "Year", y = "Number of Arrests")
}

#' Create visulization to see age distribution in each year
#'
#' Graph to see age distribution in each year
#'
#' @param process_data data processed by process_data()
#' @return plot of age distribution in each year
#' @export
ta_plot = function(process_data) {
    # number of cases versus time (year)
    process_data %>%
        ggplot(aes(x = as.factor(year_of_arrest), y = age_at_arrest)) +
        geom_boxplot(fill = "slateblue", alpha = 0.4) +
        ggtitle("Arrestee Age in Urbana-Champaign since 2000") +
        labs(x = "Year", y = "Age of Arrests")
}

#' Create visulization to see relationship between arrestee sex and arrestee race
#'
#' Graph to see relationship between arrestee sex and arrestee race
#'
#' @param process_data data processed by process_data()
#' @return plot of sex and race of arrestees
#' @export
sr_plot = function(process_data) {
    # number of cases versus time (year)
    process_data %>%
        ggplot(aes(x = arrestee_race, fill = arrestee_sex)) +
        geom_bar(position = "stack") +
        labs(title = "Arrestee Gender by Race", x = "Race", y = "Count")
}

#' Cleaning the data and select the features for modeling
#'
#' clean the data and select specific features for modeling
#'
#' @param process_data data processed by process_data()
#' @return tidy data that can be used for modeling
#' @export
model_data = function(process_data) {
    model_data = process_data %>%
        select(month_of_arrest, crime_category_description, age_at_arrest,
               arrestee_sex, arrestee_race,
               arrestee_employment_description, homecity)
    ## dropping crimes that happened too few
    crimes = model_data %>%
        group_by(crime_category_description) %>%
        count() %>%
        filter(n > 500) %>%
        select(crime_category_description)

    m_data = filter(model_data, crime_category_description
                    %in% as.vector(unlist(crimes)))
    m_data
}



#' Modeling the model_data using Random forests
#'
#' use random forest to model the data
#'
#' @param process_data data processed by process_data()
#' @return tidy data that can be used for modeling
#' @export
model = function(model_data) {
    set.seed(447)
    trainIndex <- createDataPartition(1:nrow(model_data), p = 0.7, list = FALSE)
    train <- model_data[trainIndex, ]
    test <- model_data[-trainIndex, ]


    trControl <- trainControl(method = "cv",
                              number = 2,
                              search = "grid")


    # Run the model
    rf_mod <- train(as.factor(crime_category_description)~.,
                        data = train,
                        method = "rf",
                        metric = "Accuracy",
                        trControl = trControl)
    rf_mod
}
