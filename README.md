---
title: "Final Project"
author: "Muchlas Amirinanto"
date: "5/22/2019"
output: html_document
---

#1 Introduction
This is an Exploratory Data Analysis for the restaurant New Dynasty in Dupont Circle, Washington DC.

The aim of this challenge is to see if there are relations between the numbers of 8 popular item orders and weather. The data was collected from the restaurant itself. 

The 8 popular item are: Spring Rolls, General Tso Chicken, Indonesian Fried Rice, Chicken with Broccoli, Kung Pao Chicken, Orange Chicken, Chinese Fried Rice, and Lo Mein.

The data comes in the shape of 3 relational files, which are derived from the restaurant's Point Of Sales (POS) software. The training data includes data from January 2017 - November 2018, while the test set includes the month December of 2018.  Please take a note that there are days in which the restaurant closed and have no orders. The training set omit days where the restaurant is closed.

Daily weather data is taken from [National Oceanic and Atmospheric Administration website](https://www.ncdc.noaa.gov/cdo-web/).

#2 Preparation

First, we are going to load libraries needed for this project.

####2.1 Load Libraries
```{r setup, message=FALSE}
library(plyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
```

And then, we are going to load the csv data from the restaurant.

####2.2 Load Data
```{r, cache=TRUE}
menu_items <- read.csv("MenuItems.csv") %>%
  tibble::as.tibble()

order_headers <- read.csv("OrderHeaders.csv") %>%
  tibble::as.tibble()

order_trans <- read.csv("OrderTransactions.csv") %>%
  tibble::as.tibble()

weather <- read.csv("weather.csv") %>%
  tibble::as.tibble()
```

####2.3 Tidying Data

We need to remove unused variables and convert the variables to the right type, as well as doing join for the order data from the restaurant.
```{r, cache=TRUE}
menu_items <- menu_items %>%
  select(ItemID, ItemName, GroupID) %>%
  mutate(ItemID = as.character(ItemID), 
         ItemName = as.character(ItemName),
         GroupID = as.character(GroupID))

order_headers <- order_headers %>%
  select(-SalesTax) %>%
  mutate(OrderID = as.character(OrderID),
         Date = as.Date(Date, "%m/%d/%Y"))

order_trans <- order_trans %>%
  select(-TransactionID, -Price, -ExtendedPrice) %>%
  mutate(OrderID = as.character(OrderID),
         ItemID = as.character(ItemID))
```

We need to combine the data from order header and order transactions to ease further operations.
```{r, cache=TRUE}
orders <- order_headers %>%
  join(order_trans, by = "OrderID")
```

We need to remove unordered items from the data we have. The reason is that the data set contains huge numbers of items that comes from the old menu (never ordered in the year 2017 - 2018).
```{r, cache=TRUE}
menu_items <- menu_items %>%
  filter(ItemID %in% orders$ItemID)
```

The data obtained from the restaurant's POS is messy. The restaurant uses weird naming scheme for all itmes, and have multiple ItemIDs belong to a single food item. And so, to obtain the 8 popular items we targeted, I am going to manually tell the ItemIDs for those items.
- Spring Rolls: 1, 2
- General Tso Chicken: 470, 836, 822
- Indonesian Fried Rice: 44, 1032
- Chicken with Broccoli: 18
- Kung Pao Chicken: 13
- Orange Chicken: 823
- Chinese Fried Rice: 104, 106, 110, 112, 114
- Lo Mein: 168, 170, 174, 176, 178

With those ItemIDs, we are going to trim the orders and menu items dataset to only include the popular items. Also, fix the naming of those items.
```{r, cache=TRUE}
# dataframe of popular items along with their ids
popular_ids <- data.frame(
  "ItemID" = c(1, 2, 
               470, 836, 822,
               44, 1032, 
               18,
               13,
               823,
               104, 106, 110, 112, 114,
               168, 170, 174, 176, 178),
  "ItemName" = c("Spring Rolls", "Spring Rolls",
                 "General Tso Chicken", "General Tso Chicken", "General Tso Chicken",
                 "Indonesian Fried Rice", "Indonesian Fried Rice",
                 "Chicken with Broccoli", 
                 "Kung Pao Chicken", 
                 "Orange Chicken",
                 "Chinese Fried Rice", "Chinese Fried Rice", "Chinese Fried Rice",
                 "Chinese Fried Rice", "Chinese Fried Rice",
                 "Lo Mein", "Lo Mein", "Lo Mein", "Lo Mein", "Lo Mein"))

menu_items <- menu_items %>%
  # selecting only popular items
  filter(ItemID %in% popular_ids$ItemID) %>%
  # removing default name
  select(-ItemName) %>%
  # adding correct name
  join(popular_ids, by = "ItemID")

orders <- orders %>%
  # adding item name
  join(menu_items, by = "ItemID") %>%
  # removing non popular items
  filter(ItemID %in% menu_items$ItemID)
```

####2.4 Weather Data

First we need to see if data obtained from the NOAA contains any NA.
```{r, cache=TRUE}
sum(is.na(weather))
```

As we can see, there are lots of NA in the weather data. And so, we are going to clean up the data by making a single dataset with mean of all available data for every single date throughout 2017 and 2018.
```{r, cache=TRUE}
weather <- weather %>%
  # converting date type
  mutate(Date = as.Date(DATE, "%Y-%m-%d")) %>%
  # selecting time range
  filter(Date <= "2018-12-31") %>%
  group_by(Date) %>%
  # getting a single value for those variables
  summarize(Precip = mean(PRCP, na.rm = TRUE),
            Snow = mean(SNOW, na.rm = TRUE),
            TMax = mean(TMAX, na.rm = TRUE),
            TMin = mean(TMIN, na.rm = TRUE)) %>%
  ungroup()
weather
```

In addition, we also need to know wheter a peticular date is Raining, Snowing, Hot, or Cold. 
Notes: 

- I am taking 50 degrees as the line between hot and cold weather.
- I take Precip > 0.01 as the minimum for it to rain.
```{r, cache=TRUE}
temp_divide <- 50

weather <- weather %>%
  mutate(IsRain = Precip > 0.01,
         # snow if > 0 accumulated
         IsSnow = if_else(is.na(Snow), FALSE, 
                          if_else(Snow > 0, TRUE, FALSE)),
         # cold if (temp max + temp min) / 2 < 50
         Cold = if_else(is.na(TMax) | is.na(TMin), "HOT",
                          if_else((TMax + TMin) / 2 < temp_divide, "COLD", "HOT")))
```

#3 Data Overview

To makes some sense of how the data looks, we are going to take a short look at the data by using the function _summary_ and _glimpse_.

####3.1 Orders
```{r, cache=TRUE}
summary(orders)
glimpse(orders)
```

####3.2 Weather
```{r, cache=TRUE}
summary(weather)
glimpse(weather)
```

#4 Individual Feature Visualisations

Here we have a first look at the distributions of the feature in our individual data files before combining them for a more detailed analysis. This inital visualisation will be the foundation on which we build our analysis.

####4.1 Orders

We start with the number of orders to the popular items. Here we plot the total number of orders per day over the full training time range together with the median number of orders per day of the week and month of the year:

```{r, cache=TRUE}
orders_plot <- orders %>%
  filter(Date <= "2018-11-30") %>%
  group_by(Date, ItemName) %>%
  # summarize sales by the date
  summarize(Sales = sum(Quantity)) %>%
  ggplot(aes(y = Sales, x = Date, col = ItemName)) + 
  geom_line() +
  theme(legend.position = "none") +
  scale_y_log10() +
  facet_wrap(~ ItemName, ncol = 1) +
  theme_minimal()
orders_plot

weekly_orders_plot <- orders %>%
  filter(Date <= "2018-11-30") %>%
  group_by(Date, ItemName) %>%
  # summarize sales by the date
  summarize(Sales = sum(Quantity)) %>%
  mutate(Day = format(Date, "%A")) %>%
  group_by(Day, ItemName) %>%
  # getting mean sales by the day of the week
  summarize(Sales = mean(Sales)) %>%
  ggplot(aes(y = Sales, 
             x = factor(Day, 
                        levels= c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                  "Thursday", "Friday", "Saturday")))) +
  geom_col(position="dodge2", aes(fill=ItemName)) +
  theme_minimal() +
  theme(axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
  labs(y = "Median Number of Orders", x = "Day")
weekly_orders_plot

monthly_orders_plot <- orders %>%
  filter(Date <= "2018-11-30") %>%
  mutate(month_year = format(Date, "%Y-%m"),
         month = months(Date)) %>%
  group_by(month_year, month, ItemName) %>%
  # summarize sales by the month of the year
  summarize(Sales = sum(Quantity)) %>%
  group_by(month, ItemName) %>%
  # getting mean sales by the month
  summarize(Sales = mean(Sales)) %>%
  ggplot(aes(y = Sales,
             x = factor(month, levels = month.name),
             fill = ItemName,
             col = ItemName)) +
  geom_col(position="dodge2", aes(fill=ItemName)) +
  theme_minimal() +
  theme(axis.text.x  = element_text(angle=45, hjust=1)) +
  labs(y = "Median Number of Orders", x = "Month")
monthly_orders_plot

monthly_total_orders_plot <- orders %>%
  filter(Date <= "2018-11-30") %>%
  mutate(month_year = format(Date, "%Y-%m"),
         month = months(Date)) %>%
  group_by(month_year, month) %>%
  # summarize sales by the month of the year
  summarize(Sales = sum(Quantity)) %>%
  group_by(month) %>%
  # getting mean sales of total order of each month
  summarize(Sales = mean(Sales)) %>%
  ggplot(aes(y = Sales,
             x = factor(month, levels = month.name), 
             fill = factor(month, levels = month.name))) +
  geom_col(position="dodge2") +
  theme_minimal() +
  theme(axis.text.x  = element_text(angle=45, hjust=1)) +
  scale_fill_discrete(name = "Month") +
  labs(y = "Median Total Number of Orders", x = "Month")
monthly_total_orders_plot

```

We find:

- Spring Rolls sells more than any other items.
- Sells of most items somewhat fluctuates throughout the year.
- The median total number of sales seem to peak during the second half of the year.

We will be forecasting for the month of December 2018, so let’s look at this time range in our 2018 training data:
```{r, cache=TRUE}
dec2018_plot <- orders %>%
  # select only dec 2018 data
  filter(Date > "2018-11-30") %>%
  group_by(Date, ItemName) %>%
  # summarize sales by the date
  summarize(Sales = sum(Quantity)) %>%
  ggplot(aes(y = Sales, x = Date, fill = ItemName, col = ItemName)) +
  geom_line() +
  geom_smooth(method = lm) +
  theme_minimal()
dec2018_plot

```

Here, we see that going towards the end of the year, sells of most items tend to go down. However, sells during the last week of December 2018 seems unaffected by the [Holiday Break](https://en.wikipedia.org/wiki/School_holiday#Event-based). 

####4.2 Holidays

Since we are going to predict sales in December 2018, let's have a quick look at Christmas. We'll how it affects sales of the popular items, and plot how many sales are for each popular items on that day.
```{r, cache=TRUE}
christmas_plot <- orders %>%
  # only christmas days
  filter(Date == "2018-12-25" | Date == "2017-12-25") %>%
  group_by(Date, ItemName) %>%
  summarize(Sales = sum(Quantity)) %>%
  ggplot(aes(y = Sales, x = factor(Date), fill = ItemName, col = ItemName)) +
  geom_col(position="dodge2") +
  theme_minimal()
christmas_plot
```

We find:

- Sales in December 25 2017 seems to be way higher than sales in December 25 2018 for most items.

It seems that the [2018-2019 Government Shutdown](https://en.wikipedia.org/wiki/2018%E2%80%9319_United_States_federal_government_shutdown) affected the business in major terms.

####4.3 Weather

We are going to see how the temperature changes throughout our data set range in the restaurant's area (Washington DC).
```{r, cache=TRUE}
weather_plot <- weather %>%
  ggplot(aes(x = Date, y = TMax)) +
  geom_line(aes(y = TMax, col = "red")) +
  geom_line(aes(y = TMin, col = "blue")) +
  theme_minimal()
weather_plot
```

And then, we see how many rain and snow days are throughout 2017 and 2018 in Washington DC.
```{r, cache=TRUE}
rain_plot <- weather %>%
  ggplot(aes(IsRain, fill = IsRain)) +
  geom_bar() +
  theme_minimal()
rain_plot

snow_plot <- weather %>%
  ggplot(aes(IsSnow, fill = IsSnow)) +
  geom_bar() +
  theme_minimal()
snow_plot
```

Hot and cold days are worth taking a quick look at.
```{r, cache=TRUE}
cold_plot <- weather %>%
  ggplot(aes(Cold, fill = Cold)) +
  geom_bar() +
  scale_fill_manual(values = c("COLD" = "blue", "HOT" = "red")) +
  theme_minimal()
cold_plot

```

#5 Feature Relations
After looking at the data individually, let's have a look at how the data relate to each others.

####5.1 Orders by Rain Days

We are going to see how rain affects the number of orders in overall and by the day.
```{r, cache=TRUE}
order_rain <- orders %>%
  join(weather, by = "Date") %>%
  group_by(Date, ItemName, IsRain) %>%
  summarize(Sales = sum(Quantity))

order_rain %>%
  ggplot(aes(IsRain, Sales, color = IsRain)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_minimal()
```

Overall, rain days don’t have any impact on the average number of orders. As so often, more information is hidden in the details.

```{r, cache=TRUE}
order_rain %>%
  mutate(Day = format(Date, "%A")) %>%
  group_by(Day, IsRain) %>%
  summarize(Mean_Sales = mean(Sales)) %>%
  ggplot(aes(x = factor(Day, 
                        levels= c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                  "Thursday", "Friday", "Saturday")),
             y = Mean_Sales, 
             col = IsRain)) +
  geom_point(size = 5) +
  theme(axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
  labs(y = "Median Number of Orders", x = "Day") +
  theme_minimal()
```

While rain days has some positive impact on the average number of orders, there is a decrease in order number during saturday raining days.

####5.2 Orders by Snow Days

We are going to see how snow affects the number of orders in overall and by the day.
```{r, cache=TRUE}
order_snow <- orders %>%
  join(weather, by = "Date") %>%
  group_by(Date, ItemName, IsSnow) %>%
  summarize(Sales = sum(Quantity))

order_snow %>%
  ggplot(aes(IsSnow, Sales, fill = IsSnow)) +
  geom_boxplot() +
  scale_y_log10()
```

Overall, snow days don’t have any impact on the average number of orders. As so often, more information is hidden in the details.

```{r, cache=TRUE}
order_snow %>%
  mutate(Day = format(Date, "%A")) %>%
  group_by(Day, IsSnow) %>%
  summarize(Mean_Sales = mean(Sales)) %>%
  ggplot(aes(x = factor(Day, 
                        levels= c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                  "Thursday", "Friday", "Saturday")),
             y = Mean_Sales, 
             col = IsSnow)) +
  geom_point(size = 5) +
  theme(axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
  labs(y = "Median Number of Orders", x = "Day") +
  theme_minimal()
```

While snow days has some positive impact on the average number of orders on Fridays, there is a decrease in order number during Mondays.

####5.3 Orders by Cold Days

We are going to see how cold temperature affects the number of orders in overall and by the day.
```{r, cache=TRUE}
order_cold <- orders %>%
  join(weather, by = "Date") %>%
  group_by(Date, ItemName, Cold) %>%
  summarize(Sales = sum(Quantity))

order_cold %>%
  ggplot(aes(Cold, Sales, fill = Cold)) +
  geom_boxplot() +
  scale_fill_manual(values = c("COLD" = "blue", "HOT" = "red")) +
  scale_y_log10()
```

Overall, cold days don’t have any impact on the average number of orders. As so often, more information is hidden in the details.

```{r, cache=TRUE}
order_cold %>%
  mutate(Day = format(Date, "%A")) %>%
  group_by(Day, Cold) %>%
  summarize(Mean_Sales = mean(Sales)) %>%
  ggplot(aes(x = factor(Day, 
                        levels= c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                  "Thursday", "Friday", "Saturday")),
             y = Mean_Sales, 
             col = Cold)) +
  geom_point(size = 5) +
  theme(axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
  labs(y = "Median Number of Orders", x = "Day") +
  scale_color_manual(values = c("COLD" = "blue", "HOT" = "red")) +
  theme_minimal()
```

While cold early week days has some negative impact on the average number of orders, there is noticable increase in order number from Tuesday to Saturday.

#6 Fitting Models

In this section, we are going to test the hypothesis that Rain, Snow, Cold, or Hot has a relationship with the number of sales using Linear Regression.

####6.1Correlation

Correlation is a statistical measure that suggests the level of linear dependence between two variables, that occur in pair. A value closer to 0 suggests a weak relationship between the variables. A low correlation (-0.2 < x < 0.2) probably suggests that much of variation of the response variable (Y) is unexplained by the predictor (X), in which case, we should probably look for better explanatory variables.

First, we make a dataframe to do this analysis.
```{r}
# making a dataframe for fitting purpose (with weather data)
orders_fit <- orders %>%
  join(weather, by = "Date")
```

Then, we do the correlation calculation.

- Rain
```{r}
# making Rain days dataframe with sales
order_rain <- orders_fit %>%
  group_by(Date, IsRain) %>%
  summarize(Sales = sum(Quantity))

# calculating correlation
cor(order_rain$Sales, order_rain$IsRain)
```

- Snow
```{r}
# making Snow days dataframe with sales
order_snow <- orders_fit %>%
  group_by(Date, IsSnow) %>%
  summarize(Sales = sum(Quantity))

# calculating correlation
cor(order_snow$Sales, order_snow$IsSnow)
```

- Cold
```{r}
# making Cold days dataframe with sales
order_cold <- orders_fit %>%
  mutate(Cold = ifelse(Cold == "COLD", TRUE, FALSE)) %>%
  group_by(Date, Cold) %>%
  summarize(Sales = sum(Quantity))

# calculating correlation
cor(order_cold$Sales, order_cold$Cold)
```

Findings:

- All variables have a low correlation value.
- Snow has weak relationship with the number of sales.
- Rain has the highes correlation number, which indicates that it is the variable with the strongest relationship with the number of sales.

####6.2 Linear Model

Next, we are going to build the linear model of the data. 

To do this, we are going to use the _lm()_ function.

- Rain
```{r}
# Making linear model based on Rain days
summary(lm(Sales ~ IsRain, data = order_rain))
```

Findings:

1. The p-value is small enough that we can say that Rain days are very likely to relate with the number of sales.
2. The $R^2$ value is somewhat small, so it may not fit the data very well.

- Snow
```{r}
# Making linear model based on Snow days
summary(lm(Sales ~ IsSnow, data = order_snow))
```

Findings:

1. The p-value is too large that we can say that Snow days are very unlikely to relate with the number of sales.
2. The $R^2$ value is very small, so it does not fit the data very well at all.

- Cold
```{r}
# Making linear model based on Cold days
summary(lm(Sales ~ Cold, data = order_cold))
```

Findings:

1. The p-value is too large that we can say that Snow days are very unlikely to relate with the number of sales.
2. The $R^2$ value is very small, so it does not fit the data very well at all.

[Read More about Linear Regression here](http://r-statistics.co/Linear-Regression.html).

#7 Predictors

We are going to make predictors for the number of sales of an item (Chinese fried rice) using Linear Regression.

Before that, we need to separate the orders data into both training data (from January 2017 - November 2018) and test data (December 2018).
```{r}
# making dataset with all weather condition, 
# and sales based on the dates
orders_predict <- orders %>%
  join(weather, by = "Date") %>%
  filter(ItemName == "Spring Rolls") %>%
  group_by(Date, IsRain, IsSnow, Cold) %>%
  summarize(Sales = sum(Quantity))

# splitting the data into training and test
training_data <- orders_predict %>%
  filter(Date <= "2018-11-30")

glimpse(training_data)

test_data <- orders_predict %>%
  filter(Date > "2018-11-30")

glimpse(test_data)
```

Since we know from previous section that Rain days have the strongest statistical relationship with number of sales (compared to snow and cold), we are going to use this to make the predictor.

To make linear model prediction on the data, it is pretty simple.

First, we need to use the _lm_ function to make a model.
```{r}
# model training data
lm_rain <- lm(Sales ~ IsRain, data = training_data)
summary(lm_rain)
```

Then, we predict the sales between from the model to the test data.
```{r}
# predict distance
sales_pred_rain <- predict(lm_rain, test_data)
sales_pred_rain
```

After that, we are going to calculate a simple correlation between the actual Sales number and the predicted Sales.
```{r}
# making actual prediction dataframe
actuals_preds <- data.frame(cbind(actuals = test_data$Sales, predicteds = sales_pred_rain))

# calculating correlation accuracy
cor(actuals_preds)
```

It seems that the accuracy is really low for our models (ouch). This may very well be the impact of Government Shutdown during the month of December 2018.

To check for accuracy of our predictors, we are goingt to calculate the Min Max Accuracy and Mean Absolute Percentage Error (MAPE).

Min Max Accuracy tells how far off a model' predictions are. [From here](https://adataanalyst.com/machine-learning/guide-for-linear-regression-using-python-part-2/). MAPE is a measure of prediction accuracy of a forecasting method. [Read more about MAPE here](https://en.wikipedia.org/wiki/Mean_absolute_percentage_error).

Anyway, here is the formula: 
$$MinMaxAccuracy=mean(\frac{min(actuals, predicteds)}{max(actuals, predicteds)})$$
$$MAPE=mean(\frac{abs(predicteds - actuals)}{actuals)})$$

```{r}
# calculating min max accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy

# calculating mape
mape <- mean(
  abs((actuals_preds$predicteds - actuals_preds$actuals)) 
  / actuals_preds$actuals)  
mape
```

We can see how the prediction looks compared to the actual in the following graph.
```{r}
actuals_preds %>%
  mutate(date = row_number()) %>%
  ggplot(aes(x = factor(date))) +
  geom_point(aes(y = actuals, col = "actuals")) +
  geom_point(aes(y = predicteds, col = "predicteds")) +
  labs(x = "Date", y = "Sales") +
  scale_colour_manual(name = "Sales\nCategory", values = c("red", "blue"), labels = c("Actual", "Predicted"))
```

Interestingly, our model almost correctly predicted the sale on Three Dates while failing miserably on every other date.

#8 Conclusion

While it is interesting to see how the sales of the popular items in the restaurant, there seems to be low relevance between weather condition and the number of sales.

Also, using linear regression we are able to see that not a single weather condition affect sales in an impactful way. 

This leads to a failure in making a useful predictor for the number of sales.

Maybe in the future, predicting sales for this restaurant should be done with other variables.
