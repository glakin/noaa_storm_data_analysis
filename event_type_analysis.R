library(ggplot2)
library(dplyr)

# Read the data
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",temp)
df <- read.csv(bzfile(temp))
unlink(temp)

# Define casualties as fatalities + injuries
df$casualties <- (df$FATALITIES + df$INJURIES)

# calculate property damage in $ using PROPDMGEXP
df$propDmgCoeff <- 1
df[df$PROPDMGEXP == 1, ]$propDmgCoeff <- 10
df[df$PROPDMGEXP %in% c('h', 'H', 2), ]$propDmgCoeff <- 100
df[df$PROPDMGEXP %in% c('k', 'K', 3), ]$propDmgCoeff <- 1000
df[df$PROPDMGEXP == 4, ]$propDmgCoeff <- 10000
df[df$PROPDMGEXP == 5, ]$propDmgCoeff <- 100000
df[df$PROPDMGEXP %in% c('m', 'M', 6), ]$propDmgCoeff <- 1000000
df[df$PROPDMGEXP == 7, ]$propDmgCoeff <- 10000000
df[df$PROPDMGEXP == 8, ]$propDmgCoeff <- 100000000
df[df$PROPDMGEXP %in% c('B',9), ]$propDmgCoeff <- 1000000000
df$propDmgCost <- df$PROPDMG * df$propDmgCoeff

# calculate crop damage in $ using CROPDMGEXP
df$cropDmgCoeff <- 1
df[df$CROPDMGEXP %in% c('h', 'H', 2), ]$cropDmgCoeff <- 100
df[df$CROPDMGEXP %in% c('k', 'K', 3), ]$cropDmgCoeff <- 1000
df[df$CROPDMGEXP %in% c('m', 'M', 6), ]$cropDmgCoeff <- 1000000
df[df$CROPDMGEXP %in% c('B',9), ]$cropDmgCoeff <- 1000000000
df$cropDmgCost <- df$CROPDMG * df$cropDmgCoeff

df$cost <- df$propDmgCost + df$cropDmgCost

northeast = c('ME', 'NH', 'VT', 'MA', 'CT', 'RI', 'NY', 'NJ', 'PA')
south = c('DE', 'MD', 'DC', 'WV', 'VA', 'KY', 'NC', 'TN', 'SC', 'GA', 'FL', 'AL', 'MS',
          'LA', 'AR', 'OK', 'TX')
midwest = c('OH', 'MI', 'IN', 'IL', 'WI', 'MN', 'IA', 'MO', 'KS', 'NE', 'SD', 'ND')
west = c('NM', 'CO', 'AZ', 'UT', 'WY', 'MT', 'ID', 'NV', 'WA', 'OR', 'CA', 'AK', 'HI')

df$region <- 'Other'
df[as.character(df$STATE) %in% northeast, ]$region <- 'Northeast'
df[as.character(df$STATE) %in% south, ]$region <- 'South'
df[as.character(df$STATE) %in% midwest, ]$region <- 'Midwest'
df[as.character(df$STATE) %in% west, ]$region <- 'West'

ggplot(df, aes(x=region, y=casualties)) + geom_boxplot()

unique(df$EVTYPE)

eventTypes <- df %>% 
    group_by(EVTYPE, region) %>% 
    summarize(count = n(),
              sumCasualties = sum(casualties),
              avgCasualties = mean(casualties),
              maxCasualties = max(casualties),
              medianCasualties = median(casualties),
              sumFatalities = sum(FATALITIES),
              avgFatalities = mean(FATALITIES),
              maxFatalities = max(FATALITIES),
              medianFatalities = median(FATALITIES),
              sumCost = sum(cost),
              avgCost = mean(cost),
              maxCost = max(cost),
              medianCost = median(cost))
eventTypes <- data.frame(eventTypes)

#topEventTypes <- eventTypes[ntile(eventTypes$sumCasualties,100) == 100, ]
topCostNortheast <- eventTypes[eventTypes$region == 'Northeast', ] %>% 
    filter(!is.na(sumCost)) %>%
    slice_max(order_by = sumCost, n = 5, with_ties = FALSE)
topCostSouth <- eventTypes[eventTypes$region == 'South', ] %>% 
    filter(!is.na(sumCost)) %>%
    slice_max(order_by = sumCost, n = 5, with_ties = FALSE)
topCostMidwest <- eventTypes[eventTypes$region == 'Midwest', ] %>% 
    filter(!is.na(sumCost)) %>%
    slice_max(order_by = sumCost, n = 5, with_ties = FALSE)
topCostWest <- eventTypes[eventTypes$region == 'West', ] %>% 
    filter(!is.na(sumCost)) %>%
    slice_max(order_by = sumCost, n = 5, with_ties = FALSE)

topCost = rbind(topCostNortheast, topCostSouth, topCostMidwest, topCostWest)

topCasualtiesNortheast <- eventTypes[eventTypes$region == 'Northeast', ] %>% 
    filter(!is.na(sumCasualties)) %>%
    slice_max(order_by = sumCasualties, n = 5, with_ties = FALSE)
topCasualtiesSouth <- eventTypes[eventTypes$region == 'South', ] %>% 
    filter(!is.na(sumCasualties)) %>%
    slice_max(order_by = sumCasualties, n = 5, with_ties = FALSE)
topCasualtiesMidwest <- eventTypes[eventTypes$region == 'Midwest', ] %>% 
    filter(!is.na(sumCasualties)) %>%
    slice_max(order_by = sumCasualties, n = 5, with_ties = FALSE)
topCasualtiesWest <- eventTypes[eventTypes$region == 'West', ] %>% 
    filter(!is.na(sumCasualties)) %>%
    slice_max(order_by = sumCasualties, n = 5, with_ties = FALSE)

topCasualties = rbind(topCasualtiesNortheast, topCasualtiesSouth, topCasualtiesMidwest, topCasualtiesWest)



# Plot casualty data

ggplot(topCasualties, aes(x=EVTYPE, y=sumCasualties)) +
    geom_bar(stat="identity") +
    facet_wrap(region ~ ., scales="free") +
    xlab("Event Type") +
    ylab("Total Casualties")

ggplot(topCasualties, aes(x=EVTYPE, y=avgCasualties)) +
    geom_bar(stat="identity") +
    facet_wrap(region ~ ., scales="free") +
    xlab("Event Type") +
    ylab("Average Casualties")

ggplot(topCasualties, aes(x=EVTYPE, y=sumFatalities)) +
    geom_bar(stat="identity") +
    facet_wrap(region ~ ., scales="free") +
    xlab("Event Type") +
    ylab("Total Fatalities")

ggplot(topCasualties, aes(x=EVTYPE, y=avgFatalities)) +
    geom_bar(stat="identity") +
    facet_wrap(region ~ ., scales="free") +
    xlab("Event Type") +
    ylab("Average Fatalities")

# Plot cost data

ggplot(topCost, aes(x=EVTYPE, y=sumCost/1000000000)) +
    geom_bar(stat="identity") +
    facet_wrap(region ~ ., scales="free") +
    xlab('Event Type') +
    ylab('Total Cost (Billions)')

ggplot(topCost, aes(x=EVTYPE, y=avgCost)) +
    geom_bar(stat="identity") +
    facet_wrap(region ~ ., scales="free") +
    xlab('Event Type') +
    ylab('Average Cost')



p <- ggplot(df, aes(x=EVTYPE, y=casualties))
p + geom_boxplot()
