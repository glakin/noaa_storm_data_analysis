library(ggplot2)
library(dplyr)

#Read the data
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",temp)
df <- read.csv(bzfile(temp))
unlink(temp)

df$casualties <- (df$FATALITIES + df$INJURIES)

unique(df$EVTYPE)

eventTypes <- df %>% 
    group_by(EVTYPE) %>% 
    summarize(casualties=sum(casualties), 
              count = n(),
              max_casualties = max(casualties))

p <- ggplot(df, aes(x=EVTYPE, y=casualties))
p + geom_boxplot()
