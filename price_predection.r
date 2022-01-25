---
title: "Air Bnb Price Prediction"
author: "Bilwashree"
date: "8/2/2021"
output:
  pdf_document: default
  pdf: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r loadPackages, warning=FALSE, message=FALSE, results='hide'  }
if(!require("pacman")) install.packages("pacman")
pacman::p_load(equisse, forecast,tidyverse, gplots, GGally, mosaic, mosaicData, scales, mapproj, mlbench, data.table, ggcorrplot, leaps, corrplot,gridExtra,MASS, caret, ggplot2, knitr, ggmap, grid, lattice, ROSE, randomForest, dplyr, rpart, rpart.plot, readr, bit64)
```

##Read in initial Data

```{r Read In Data, results='hide'}
# Read in each data set and add a column called city with that city name
  austin.df <- fread("austin_listings.csv")
  nola.df <- fread("nola_listings.csv")
  austin.df$city <- "Austin"
  nola.df$city <- "New Orleans"
  nashville.df <- fread("nashville_listings.csv")
  nashville.df$city <- "Nashville"
  asheville.df <- fread("asheville_listings.csv")
  asheville.df$city <- "Asheville"
  chicago.df <- fread("chicago_listings.csv")
  chicago.df$city <- "Chicago"
  denver.df <- fread("denver_listings.csv")
  denver.df$city <- "Denver"
  la.df <- fread("la_listings.csv")
  la.df$city <- "Los Angeles"
  portland.df <- fread("portland_listings.csv")
  portland.df$city <- "Portland"
  seattle.df <- fread("seattle_listings.csv")
  seattle.df$city <- "Seattle"
  dc.df <- fread("dc_listings.csv")
  dc.df$city <- "Washington DC"
  boston.df <- fread("boston_listings.csv")
  boston.df$city <- "Boston"
  columbus.df <- fread("columbus_listings.csv")
  columbus.df$city <- "Columbus"
  nyc.df <- fread("nyc_listings.csv")
  nyc.df$city <- "New York City"
  sanfran.df <- fread("sanfrancisco_listings.csv")
  sanfran.df$city <- "San Francisco"
  twincities.df <- fread("twincities_listings.csv")
  twincities.df$city <- "Twin Cities"
  jerseycity.df <- fread("jerseycity_listings.csv")
  jerseycity.df$city <- "Jersey City"
  hawaii.df <- fread("hawaii_listings.csv")
  hawaii.df$city <- "Hawaii"

# Merge dataframes
  ab.df <- rbind(austin.df, nola.df, nashville.df, asheville.df, chicago.df, denver.df, la.df, portland.df, seattle.df,            dc.df, boston.df, columbus.df, nyc.df, sanfran.df, twincities.df, jerseycity.df, hawaii.df) 

# remove individual data frames from environment
  rm(austin.df, nola.df, nashville.df, asheville.df, chicago.df, denver.df, la.df, portland.df, seattle.df, dc.df, ... =      boston.df, columbus.df, nyc.df, sanfran.df, twincities.df, jerseycity.df, hawaii.df) 
  
# Create a copy of the data in data table form
  ab.dt <- as.data.table(ab.df) 
```

##Clean Data

```{r Clean Data}
# Remove columns we will definitely not use: scrape_id, last_scraped, host thumbnail url, host picture url,
# neighborhood_group_cleansed, bathrooms(empty)
  ab.dt <- ab.dt[,-c(3,4,19,20,29,35,43:49,55,68)] 

# Clean individual columns
  ab.dt <- ab.dt[!(host_name == "" | is.na(host_name)), ] # remove no host name listings
  ab.dt <- ab.dt[price>10] # no free listings
  ab.dt <- ab.dt[availability_365>0] # Remove inactive properties
  ab.dt <- ab.dt[!(name == "" | is.na(name)), ] # remove no name listings
  ab.dt[is.na(ab.dt$review_scores_accuracy), c(47:53,59)] <- 0 #replace N/As with 0, these listings haven't received any reviews yet

# Clean property_type to a reasonable number of factors, calling anything that isn't a traditional dwelling "Alternative"
  ab.dt$property_type <- factor(ab.dt$property_type)
  levels(ab.dt$property_type)
  
  levels(ab.dt$property_type) <- c("Alternative","Alternative","Alternative","Alternative","Alternative",
        "Alternative","Alternative","Alternative","Alternative","Alternative","Entire House/Apartment",
        "Hotel","Entire House/Apartment","Entire House/Apartment","Entire House/Apartment","Entire House/Apartment",
        "Entire House/Apartment","Entire House/Apartment","Entire House/Apartment","Entire House/Apartment",
        "Entire House/Apartment","Entire House/Apartment","Entire House/Apartment","Entire House/Apartment",
        "Entire House/Apartment","Hotel","Entire House/Apartment","Entire House/Apartment","Entire House/Apartment",
        "Entire House/Apartment","Alternative","Entire House/Apartment","Alternative","Alternative","Alternative",
        "Alternative","Alternative","Private Room","Private Room","Alternative","Private Room","Alternative",
        "Private Room","Alternative","Private Room","Alternative","Alternative","Alternative","Private Room",
        "Private Room","Private Room","Alternative","Private Room","Alternative","Alternative","Private Room", 
        "Private Room","Private Room", "Private Room", "Private Room", "Alternative", "Alternative", "Alternative",
        "Alternative","Private Room","Private Room","Private Room","Hotel","Private Room","Alternative","Private Room",
        "Alternative","Private Room", "Alternative", "Private Room", "Alternative", "Alternative", "Alternative",
        "Private Room","Hotel", "Hotel", "Private Room", "Hotel", "Hotel", "Hotel", "Private Room", "Shared Room",
        "Shared Room","Shared Room", "Hotel", "Alternative", "Hotel", "Shared Room", "Shared Room", "Alternative",
        "Shared Room","Shared Room", "Alternative", "Shared Room", "Alternative", "Alternative", "Shared Room",
        "Shared Room","Shared Room","Shared Room","Hotel","Shared Room","Alternative", "Alternative","Alternative",
        "Shared Room","Hotel","Shared Room", "Alternative", "Shared Room", "Shared Room", "Shared Room", "Alternative",
        "Alternative","Alternative","Alternative","Alternative","Alternative","Alternative","Alternative",
        "Alternative")

# Clean bathrooms_text column to leave only numerical values
  ab.dt$bathrooms_text <- gsub("[a-zA-Z ]", "", ab.dt$bathrooms_text) 
  ab.dt$bathrooms_text <- as.numeric(ab.dt$bathrooms_text) #Cast as numeric
  ab.dt$bathrooms_text[is.na(ab.dt$bathrooms_text)] <- 0 #replace NAs with 0 (mostly "Alternative" listings)

# Cast remaining columns as appropriate data types where they are not already identified as such
  ab.dt$room_type <- factor(ab.dt$room_type)
  ab.dt$host_since <- as.Date(ab.dt$host_since, "%m/%d/%y")
  ab.dt$host_response_time <- as.factor(ab.dt$host_response_time)
  ab.dt$last_review <- as.Date(ab.dt$last_review, "%m/%d/%y")
  ab.dt$host_acceptance_rate <- as.numeric(ab.dt$host_acceptance_rate)
  ab.dt$host_response_rate <- as.numeric(ab.dt$host_response_rate)
  ab.dt[is.na(ab.dt$host_response_rate), c(14:15)] <- 0
  ab.dt$host_is_superhost <- as.factor(ab.dt$host_is_superhost)
  ab.dt$host_has_profile_pic <- as.factor(ab.dt$host_has_profile_pic)
  ab.dt$host_identity_verified <- as.factor(ab.dt$host_identity_verified)
  ab.dt$instant_bookable <- as.factor(ab.dt$instant_bookable)
  ab.dt$city <- as.factor(ab.dt$city)

# check for any remaining missing values  
  missingvalues = (sum(is.na(ab.dt)))
  print("Check for Remaining Missing Values")
  print(missingvalues)
```

###Exploratory Data Analysis
The following charts provide the distributions for each relevant variable.

```{r}
# price distribution
barplot(ab.dt$price,col="red",
        xlab="PRICE",ylab="PRICE counts",main="PRICE distribution", limits = c(0,1000))
count1 <- ab.dt[, .N, by=.(price)]
```

```{r}
# reviews_per_month distribution
barplot(ab.dt$reviews_per_month,col="red",
       xlab="reviews_per_month",ylab="reviews_per_month counts",main="reviews_per_month distribution")
count4 <- ab.dt[, .N, by=.(reviews_per_month)]
```

```{r}
#  availability_365 distribution
barplot(ab.dt$availability_365, width=10, horiz = TRUE,
        xlab="availability_365 Counts",ylab="availability_365",main="availability_365 distribution", ylim = c(0,400), xpd = FALSE, axes=TRUE)
```

```{r US Map}

locations.dt<-ab.dt
levels(locations.dt$city)<- c("NC", "TX", "MA", "FL", "MA", "IL", "NV", "OH", "CO", "HI", "NJ", "CA", "TN", "LA", "NY", "CA", "CA", "OR", "RI", "OR", "CA", "CA", "CA", "CA", "CA", "WA", "MN", "D.C.")
count.dt <- locations.dt[, .N, by=.(city)]
mUSMap(data=count.dt, key ="city", fill="N")
count.dt
```


```{r Room Type}
## Room_type
options(scipen = 999)
plot(ab.dt$room_type)

plot(x = ab.dt$room_type, y = ab.dt$price)
#Based on this, remove Shared room as they are so rare they don't have much impact on our analysis
ab.dt <- ab.dt[!(room_type == "Shared room"), ] 
ab.dt$room_type <- factor(ab.dt$room_type) #relevel so there are only three levels
```


```{r Property Type}
plot(ab.dt$property_type)
plot(x = ab.dt$property_type, y = ab.dt$price)
summary(ab.dt$property_type)
ab.dt$property_type <- factor(ab.dt$property_type) #remove Shared room factor which now has 0 records
```

### price vs city
```{r}
ggplot(ab.dt) +
geom_point(aes(x = city, y = price), color = "tomato2", alpha = 0.5) +  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) +
ggtitle("city vs price")
```


## Price vs number of reviews
```{r}
##price vs no_of_reviews
ggplot(ab.dt) +
geom_point(aes(x = number_of_reviews, y = price), color = "tomato2", alpha = 0.5) +
ggtitle("number_of_reviews vs price")
```

##Correlation Plot

```{r correlation plot}

corr.Ab <- ab.dt[,c(34,14,18,19,29,30,31,32,35,41,42,47,48)]
corr.Ab <- na.omit(corr.Ab)
## calculate correlation table
corr.mat <- cor(corr.Ab)
print(corr.mat)
## create correlation plot
corr.plot <- ggcorrplot (corr.mat, lab = TRUE, colors = c("white","#e9b7ce","#FF1F1F"),title = "Correlation Matrix")
print(corr.plot)

```

##Choose and Partition Data

```{r Choose and Partition Data}
#Make a copy of the data table
ab1.dt <- ab.dt
# select variables for regression
ab1.dt <-ab.dt [, c(10,13:16,21,22,27,28,29,30,31,32,34:36,41,42,44,46:54,59,60)]

missingvalues = (sum(is.na(ab1.dt)))
print("Check for Remaining Missing Values")
print(missingvalues)

set.seed(123)
sample<- sample.int(n=nrow(ab1.dt),size=floor(round(0.70*nrow(ab1.dt))), replace = F)
train.dt <- ab1.dt[sample, ]
train.dt <- na.omit(train.dt)
valid.dt <- ab1.dt[-sample, ]
valid.dt <- na.omit(valid.dt)
nrow(train.dt)
nrow(valid.dt)
```

##Simple Linear Regression

```{r Simple Linear Regression}
lm.original <- lm(price ~ . , data=train.dt)
summary(lm.original)
plot(lm.original)
```

Observe that data does not meet assumptions of normality so a log transformation is chosen

###Log and Partition Data

```{r Log and Partition}

ab.log <-ab1.dt [, c(3,4,10:19,21:27,29)] #Remove Categorical and Date Variables
ab.log <- 1+ab.log
ab.log <- log(ab.log)
ab.log$host_since <- ab1.dt$host_since
ab.log$host_response_time <- ab1.dt$host_response_time
ab.log$host_is_superhost <- ab1.dt$host_is_superhost
ab.log$host_has_profile_pic <- ab1.dt$host_has_profile_pic
ab.log$host_identity_verified <- ab1.dt$host_identity_verified
ab.log$property_type <- ab1.dt$property_type
ab.log$room_type <- ab1.dt$room_type #Add them back in
ab.log$last_review <- ab1.dt$last_review
ab.log$instant_bookable<- ab1.dt$instant_bookable
ab.log$city <- ab1.dt$city

ab.log <- na.omit(ab.log)
missingvalues = (sum(is.na(ab.log)))
print("Check for Remaining Missing Values")
print(missingvalues)
set.seed(123)
sample<- sample.int(n=nrow(ab.log),size=floor(round(0.70*nrow(ab.log))), replace = F)
log.train <- ab.log[sample, ]
log.valid <- ab.log[-sample, ]
nrow(log.train)
nrow(log.valid)
```

##Logged Data - Simple Linear Regression

```{r Logged Linear Regression}
lm <- lm(price ~ ., data = log.train)
summary(lm)
```

Observe the data much more closely fits the assumptions for linear regression

##Stepwise Selection

```{r Stepwise Selection}
lm2 <- lm(price ~ ., data = log.train)
step.lm <- step(lm2, direction = "both")
summary(step.lm)

predict.step <- predict(step.lm,log.valid)
accuracy(predict.step, log.valid$price)
```

##Backwards Regression

```{r}
bselect.lm <- step(lm2, direction = "backward")
summary(bselect.lm)  # Which variables were dropped?
bselect.lm.pred <- predict(bselect.lm, log.valid)
accuracy(bselect.lm.pred, log.valid$price)
```

##Forward Regression

```{r}
# create model with no predictors
lm.null <- lm(price~1, data = log.train)

# use step() to run forward regression.
fselect.lm <- step(lm.null, scope=list(lower=lm.null, upper=lm2), direction = "forward")
summary(fselect.lm)
fselect.lm.pred <- predict(fselect.lm, log.valid)
accuracy(fselect.lm.pred, log.valid$price)
```

##Cross Validation

```{r}
# Set Custom Control Parameters
tr <- trainControl(method = "repeatedcv", 
                          number = 10, repeats = 3,
                          verboseIter = TRUE)
```

##Ridge Regression
```{r}
colSums(is.na(ab.log))
ridgeReg <- train(price~., log.train, method = 'glmnet',
                  tuneGrid = expand.grid(alpha = 0, lambda = seq(0.0001, 1, length = 5)),
                  trControl = tr)
# print results
print(ridgeReg)

 # plot results
plot(ridgeReg)
plot(ridgeReg$finalModel, xvar = 'lambda', lwd =1.4, label = TRUE)
plot(varImp(ridgeReg, scale = TRUE))
```

##Lasso Regression
```{r}
lassoReg <- train(price~., log.train, method = 'glmnet',
               tuneGrid = expand.grid(alpha = 1, lambda = seq(0.0001, 0.3, length = 10)),
               trControl = tr)

  # print results
print(lassoReg)

 # plot results
plot(lassoReg)
plot(lassoReg$finalModel, xvar = 'lambda', lwd =1.4, label=TRUE)
plot(varImp(lassoReg, scale = TRUE))
```


##Elastic-Net Regression
```{r}
enetReg <- train(price~., log.train, method = 'glmnet',
               tuneGrid = expand.grid(alpha = seq(0, 1, length = 10),
                          lambda = seq(0.0001, 0.3, length = 10)),
               trControl = tr)
  # print best-tuned results
enetReg$bestTune

 # plot results
plot(enetReg)  # alpha is the mixing parameter and lambda is the regularization parameter
plot(enetReg$finalModel, xvar = 'lambda', lwd =1.4, label=TRUE)
plot(varImp(enetReg, scale = TRUE))
```

##Comparison

```{r}
# create a list of above models
model_list <- list(Ridge = ridgeReg,
Lasso = lassoReg,
ElasticNet = enetReg)

compare <- resamples(model_list)
# Compare summary of models
summary(compare)

# Plot errors from two of the four above models
xyplot(compare, model = c("Ridge", "Lasso"),metric = 'RMSE')
```
