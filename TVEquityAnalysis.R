# TV Equity Analysis

# read in data

data<-read.csv("TVEquity.csv")

# Tidy data

names<-names(data)

names<-gsub("Avg_", "", names)
names<-gsub("_", "", names)
names<-tolower(names)
names<-gsub("^tom", "topofmind", names)

names(data)<-names

View(data)


# Calculate adstock value for TV impressions vs. total awareness

x<-data$impressions
y<-data$totalawareness

## set range of possible adstock rates

rate <- seq(.00, 1, .01)

## create a function to find rate that produces the highest correlation with total awareness

adstock_func <- function(rate){
  cor(y, as.numeric(filter(x,filter=rate, method='recursive')))
}

## to get correlations

correlation <- sapply(rate, adstock_func)

#rate and correlation matrix

rate_cor <- data.frame(rate, correlation)

## To get rate with the max correlation

bestrate<-subset(rate_cor, correlation==max(correlation))[,1]


# Apply bestrate to impressions

n<-nrow(data)

data$asimp<-numeric(n)

data$asimp[1]<-data$impressions[1]

for(i in 2:n){data$asimp[i] = data$impressions[i] + data$asimp[i-1]*bestrate}

View(data)

# Regression to check things out

m1<-lm(totalawareness~asimp, data=data)

summary(m1)

plot(m1)


# Add in the creatives as factors

m2<-lm(totalawareness~asimp+crossanyroad+guesstheprice+howhotdoyoulikeit+jeffhisfriends+newneighbors+regularspeedchase+rubenthereceders+sizzlingsausagegrill+smokeygoodness, data=data)

summary(m2)

plot(m2)



