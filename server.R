library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(data.table)
library(dtplyr)
library(Metrics)
library(reshape2)
library(forecast)

data <- read.csv('D:\\UW\\2nd semester\\Adv R\\project\\raw_series.csv')

data <- melt(data, id = c('Country', 'Variable'))
data <- dcast(data, Country+variable~Variable)
data[data==":"] <- NA
data <- data %>% na.omit()
data <- lapply(data, gsub, pattern=",", replacement="")
data_clean <- as.data.frame(data)

data_clean[ ,c('Acquisitions.less.disposals.of.valuables', 'Changes.in.inventories', 
               'Changes.in.inventories.and.acquisitions.less.disposals.of.valuables',
               'External.balance...Goods', 'External.balance...Services', 'External.balance.of.goods.and.services',
               'Statistical.discrepancy..expenditure.approach.','Statistical.discrepancy..income.approach.',
               'Statistical.discrepancy..production.approach.')] <- list(NULL)
data_clean <- lapply(data_clean, gsub, pattern=",", replacement="")
data_clean <- lapply(data_clean, gsub, pattern="X", replacement="")
data_clean <- lapply(data_clean, gsub, pattern="Q1", replacement="01.01")
data_clean <- lapply(data_clean, gsub, pattern="Q2", replacement="04.01")
data_clean <- lapply(data_clean, gsub, pattern="Q3", replacement="07.01")
data_clean <- lapply(data_clean, gsub, pattern="Q4", replacement="10.01")
sapply(data_clean, class)

data_clean <- as.data.frame(data_clean)
len <- length(data_clean)
#convert columns to a proper data type
data_clean$variable <- as.Date(data_clean$variable, format = "%Y.%m.%d")
for (c in c(3:len)) {
  data_clean[c] = as.numeric(unlist(data_clean[c]))
}

#data_clean[2:1546, 3:len] <- as.data.frame(diff(log(as.matrix(data_clean[3:len]))))
#data_clean[1, 3:len] <- 0

timeCV <- function(dep, ind1, ind2, ctry, size=0.7, cv=4) {
  data_cv <- data_clean %>% filter(Country == ctry)
  num_cv <- length(data_cv$variable) - round(length(data_cv$variable)*size) #definte number of cross-validations
  
  rmse_total <- vector()
  for (j in c(1:cv)) {
    rmse_vec <- vector() #output
    for (i in c(1:(num_cv-cv))) {
      lim <- round(length(data_cv$variable)*size)+i #define train set size (70% of the dataset)
      data_train <- ts(data_cv[1:lim,],frequency=4,start=c(1995,1), end = c(2020,4))
      data_test <- data_cv[(lim+j),]
      model <- tslm(paste(dep, '~', ind1, '+', ind2), data = data_train)
      data_pred = predict(model, data_test[c(ind1,ind2)]) 
      x <- rmse(data_test$Compensation.of.employees, data_pred)
      rmse_vec <- c(rmse_vec, x)
      mean(rmse_vec)
    }
    rmse_total <- c(rmse_total, mean(rmse_vec))
  }
  return(rmse_total)
}

server <- function(input, output, session) {
  output$prn <- renderPrint({
    if (input$show_size) {
      dim(data_clean)
    } else {
    "Select checkpoint to see the output"  
      }
  })
  
  output$prn2 <- renderPrint({
    if (input$show_cols) {
     colnames(data_clean)
    } else {
      "Select checkpoint to see the output"   
    }
  })
  
  output$prn3 <- renderPrint({
    if (input$show_sum) {
      summary(data_clean)
    } else {
      "Select checkpoint to see the output"   
    }
  })

  output$prn4 <- renderPrint({
    if (input$show_na) {
      sort(apply(data_clean, 2, function(col)sum(is.na(col))/length(col)))
    } else {
      "Select checkpoint to see the output"   
    }
  })
  
  output$plot1 <- renderPlot({
    data_c <- data_clean %>% filter(Country == input$country1)
    plot(data_c[, match(input$variable1, colnames(data_clean))], data_c[, match(input$variable2, colnames(data_clean))], xlab = input$variable1, ylab = input$variable2)
  })

  output$plot2 <- renderPlot({
    data_c <- data_clean %>% filter(Country == input$country1)
    plot(data_c[, match(input$variable1, colnames(data_clean))], type = 'l', col = 'red', 
         ylim = c(0,max(max(data_c[, match(input$variable1, colnames(data_clean))]),
                        max(data_c[, match(input$variable2, colnames(data_clean))]))),
         xlab = 'periods', ylab = 'values')
    lines(data_c[, match(input$variable2, colnames(data_clean))], col = 'green')
    legend(1, max(max(data_c[, match(input$variable1, colnames(data_clean))]),
                  max(data_c[, match(input$variable2, colnames(data_clean))])), 
           legend=c(input$variable1, input$variable2),
           col=c("red", "green"), lty=1, cex=0.8)
    
  })
  
  output$plot3 <- renderPlot({
    plot(timeCV(dep = input$dep, ind1 = input$ind1, ind2 = input$ind2, ctry=input$ctry, size = input$size, cv=input$cv),
         type = 'l', xlab = 'Period', ylab = 'RMSE', col = 'red', 
         ylim=range(c(timeCV(dep = input$dep, ind1 = input$ind1, ind2 = input$ind2, ctry=input$ctry, 
                             size = input$size, cv=input$cv),
                      timeCV(dep = input$dep, ind1 = input$ind3, ind2 = input$ind4, ctry=input$ctry, 
                             size = input$size, cv=input$cv)+500)))
    lines(timeCV(dep = input$dep, ind1 = input$ind3, ind2 = input$ind4, ctry=input$ctry, size = input$size, cv=input$cv),
          col = 'blue')
    legend(1, 500+max(max(timeCV(dep = input$dep, ind1 = input$ind1, ind2 = input$ind2, ctry=input$ctry, 
                          size = input$size, cv=input$cv)),
                   max(timeCV(dep = input$dep, ind1 = input$ind3, ind2 = input$ind4, ctry=input$ctry, 
                          size = input$size, cv=input$cv))),
           legend=c('Model 1', 'Model 2'),
           col=c("red", "blue"), lty=1, cex=0.8)
  
  })
}