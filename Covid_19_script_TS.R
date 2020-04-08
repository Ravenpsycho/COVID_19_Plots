library(forecast); library(reshape2); library(dplyr)
library(plotly); library(htmlwidgets)

old_wd = getwd()
setwd("./ressources/")
download.file("https://github.com/daenuprobst/covid19-cases-switzerland/archive/master.zip", 
              destfile = "zipdata.zip")
unzip(zipfile = "zipdata.zip", )
setwd(old_wd)

og_df<- data.frame(read.csv("./ressources/covid19-cases-switzerland-master/covid19_cases_switzerland_openzh.csv"))
df <- data.frame(read.csv("./ressources/covid19-cases-switzerland-master/covid19_cases_switzerland_openzh.csv"))

# Determining most relevant cantons AKA '> 500 cases on 06.04.2020'
rel <- which(df[as.character(df$Date) == "2020-04-06",] > 500)
cc <- complete.cases(df[,rel])
lastcomplete <- which(cc)[length(which(cc))]

# Reshaping the dataframe to keep only relevant rows and columns
df <- df[1:lastcomplete,c(1, 28)]
colnames(df) <- c("Date", "Recorded Cases")

# Creating the daily timeseries that will be fed to the model
tsCH <- ts(df$'Recorded Cases', 
           start = c(2020, as.numeric(format(as.Date(df$Date)[1], "%j")))
           , frequency = 365)

# Creating the model
daystopredict <- 7  # <<-- This tells the number of days you wish to predict
CHmod <- auto.arima(tsCH, seasonal = F)
CHpred <- forecast(CHmod, h = daystopredict)

# Reshaping the Date variable to match observed + predicted periods
dateseries <- as.Date(df$Date[1]) + c(0:(nrow(df)+daystopredict-1))
extra_rows <- (nrow(df)+1):(nrow(df)+daystopredict)
df[extra_rows,] <- NA
df$Date <- dateseries

# Integrating the model's result into the data frame
df[extra_rows, "Prediction"] <- CHpred$mean
df[extra_rows, "upper 80"] <- CHpred$upper[,1]
df[extra_rows, "upper 95"] <- CHpred$upper[,2]
df[extra_rows, "lower 80"] <- CHpred$lower[,1]
df[extra_rows, "lower 95"] <- CHpred$lower[,2]

#Building the graph
df_plot <- melt(df, id.vars = "Date")
cols <- c("Recorded Cases" = "blue3", "Prediction" = "red3", 
          "upper 80" = "steelblue", "upper 95" = "steelblue2",
          "lower 80" = "steelblue", "lower 95" = "steelblue2")
graphtitle <- paste(Sys.Date(), 
                    "Prediction model using ARIMA (0,2,0).",
                    "Data source: corona-data.ch")
plo <- plot_ly(data = df_plot, x = ~Date, y = ~value, 
               type = "scatter", mode = "lines+markers", color = ~variable,
               colors = c("blue3", "red3", "steelblue", 
                          "steelblue2", "steelblue", "steelblue2")) %>%
    layout(title = list(text =graphtitle), 
           yaxis = list(title = "Number of confirmed cases"))

# Outputting the plot and the results
old_wd <- getwd()
setwd("./plots/plotly/")
filename <- paste0(Sys.Date(), "_(", as.character(daystopredict),
                   "-days_prediction)_ARIMA_Model.html")
saveWidget(as_widget(plo), filename)
setwd(old_wd)
