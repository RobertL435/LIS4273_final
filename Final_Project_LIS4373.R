library(dplyr)
library(tm)
library(corrgram)

#read in dataset
bank_reviews <- read.csv("data/Banks.csv")


#remove NA like values and replace with 0
bank_reviews$like[is.na(bank_reviews$like)] <- 0

#convert all text to lowercase
bank_reviews$text <- tolower(bank_reviews$text)

#remove numbers from the text review
bank_reviews$text <- removeNumbers(bank_reviews$text)

review_sample <- dplyr::sample_n(bank_reviews, size = 1000)

#correlation testing
cor(review_sample$star, review_sample$like, method = "pearson") #Pearson Correlation

cor(review_sample$star, review_sample$like, method = "spearman") #Spearman Correlation

cor.test(review_sample$star, review_sample$like, method = "pearson") #Pearson Correlation

#linear regression
model <- lm(review_sample$like ~ review_sample$star, review_sample)

summary(model)

#visualization
plot(review_sample$star, review_sample$like, main="STAR RATING VS LIKES",
     xlab="STARS", ylab="LIKES")

