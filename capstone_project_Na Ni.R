library(tidyverse)
library(corrplot)
library(scales)
library(caret)
library(writexl)
setwd("~/Desktop/capstone project")
movies = read.csv("IMDb movies.csv")
rating = read.csv("IMDb ratings.csv")



##Select specific columns from rating dataframe
rating_filter= rating %>% select("imdb_title_id","males_allages_avg_vote","males_allages_votes","females_allages_avg_vote","females_allages_votes","us_voters_rating","non_us_voters_rating")



##Merge 2 dataframs and call the new one movies_ra
movies_ra= merge(movies,rating_filter,by="imdb_title_id")



##gender voting difference
movies_ra = movies_ra %>% 
  mutate(genderdifference = males_allages_avg_vote - females_allages_avg_vote,
         femalevotes_per = females_allages_votes/(females_allages_votes + males_allages_votes)*100)
movies_gender = movies_ra %>% slice_max(abs(genderdifference), n=15) %>% select("title","year","genre","country","description","males_allages_avg_vote","females_allages_avg_vote",
                                                                                "genderdifference")
write_xlsx(movies_gender,"~/Desktop/data ucla extension 2020 fall/gender vote difference_Na Ni.xlsx")


##Calculation
sum(movies_ra$votes)
sum(movies_ra$males_allages_votes,na.rm=TRUE)
sum(movies_ra$females_allages_votes,na.rm=TRUE)
mean(movies_ra[,"avg_vote"])
mean(movies_ra[,"females_allages_avg_vote"],na.rm=TRUE)
mean(movies_ra[,"males_allages_avg_vote"],na.rm=TRUE)
str(movies_ra)

## Plot Distribution of Female Votes percentage
ggplot(movies_ra, aes(x=femalevotes_per)) +
  geom_histogram(alpha=0.9) +
  labs(x = "Female Voting Percentage", y = "Movie Count", title = "Distribution of Female Voting Percentage Among Total Votes") +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 17))

##Remove useless column "original_title" and new calculated two columns above.
movies_ra = movies_ra[,-c(3,29,30)]


## Plot number of movies along the year
ggplot(movies_ra, aes(year)) +
  geom_bar() +
  labs(x = "Year movie was released", y = "Movie Count", title = "Histogram of Movie released") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 15))

## Plot Distribution of Voting Score
ggplot(movies_ra, aes(avg_vote)) +
  geom_bar() +
  labs(x = "Voting score", y = "Count", title = "Distribution of Voting Score") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 17))


##filter out the single country data along with average vote and plot in tableau
movie_country = filter(movies, !grepl("\\,", country))
movie_country = movie_country %>% select(country,avg_vote)
table(movie_country$country)
movie_country %>% count(country, sort = TRUE) #top1: USA:28511
movie_country %>% count(country, sort = TRUE) %>% top_n(12) 
movie_country = movie_country %>% filter(country=="USA"|country=="India"|country=="UK"|country=="Japan"|
                                           country=="France"|country=="Italy"|country=="Canada"|country=="Germany"|
                                           country=="Turkey"|country=="Hong Kong"|country=="Spain"|country=="South Korea")
write_xlsx(movie_country,"~/Desktop/data ucla extension 2020 fall/movie_country_Na Ni.xlsx")


##Filter out the country that whatever contains USA and create a new dataframe
usmovie = subset(movies_ra, grepl("USA", country), select=c(2:27))


#There's one 2019 data called "TV movie 2019", change it to 2019
table(usmovie$year)
usmovie$year=gsub("TV Movie 2019", 2019, usmovie$year)
table(usmovie$year)

#change year to numeric
usmovie$year = as.numeric(usmovie$year)

#Organize the year into decades
decades = seq(1890,2020, by = 10)
usmovie$decade = decades[findInterval(usmovie$year, decades)]
#Convert the decade into factor
usmovie$decade = factor(usmovie$decade)
table(usmovie$decade)

#Add a column of how many types of genre in a movie
usmovie = usmovie %>% 
  mutate(type_genre = sapply(strsplit(usmovie$genre, ","), length))




##sentiment analysis
require(devtools)
library(sentimentr)
library(stringr)
library(tidytext)
library(tm)
library(gmodels)
library(textdata)
str(usmovie)
mytext = get_sentences(usmovie$description)
head(mytext)
df = sentiment_by(mytext)
usmovie$sentiment = df$ave_sentiment
top_sen = usmovie %>% slice_max(sentiment, n = 10) %>% select("title","description","sentiment","avg_vote")
min_sen = usmovie %>% slice_min(sentiment, n = 10) %>% select("title","description","sentiment","avg_vote")
write_xlsx(top_sen,"~/Desktop/data ucla extension 2020 fall/Top 10 sentiment score_Na Ni.xlsx")
write_xlsx(min_sen,"~/Desktop/data ucla extension 2020 fall/Lowest 10 sentiment score_Na Ni.xlsx")

#wordcloud
library(wordcloud)
library(RColorBrewer)
# Create doc
doc = Corpus(VectorSource(movies_ra$description))
# Look at doc
doc[[1]][1]
#Conversion to Lowercase
doc = tm_map(doc, PlainTextDocument)
doc = tm_map(doc, tolower)
#Removing Punctuation
doc = tm_map(doc, removePunctuation)
#Remove stopwords
doc = tm_map(doc, removeWords, stopwords("english"))
# Stemming
doc = tm_map(doc, stemDocument)
# Eliminate white spaces
doc = tm_map(doc, stripWhitespace)
doc[[1]][1] 
DTM <- TermDocumentMatrix(doc)
mat <- as.matrix(DTM)
f <- sort(rowSums(mat),decreasing=TRUE)
dat <- data.frame(word = names(f),freq=f)
head(dat, 5)
set.seed(100)
#Plot all the 100 frequent words
wordcloud(words = dat$word, freq = dat$freq, random.order=FALSE, max.words = 100,colors=brewer.pal(8, "Dark2"))

#Generating positive and negative sentiment
# take all the phrases
doc1 <-tibble(phrases =doc$content)

# add an id, from 1 to n
doc1$ID <- row.names(doc1)

# split all the words
tidy_docs <- doc1 %>% unnest_tokens(word, phrases)

bing_word_counts <- tidy_docs %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts

#Words that contribute to positive and negative sentiment in description
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment", y = NULL)

#create comparison cloud
library(reshape2)
tidy_docs %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)



##Take Budget/gross income into calculation and run linear regression
#Remove all the rows with NA and in other currencies of "usa_gross_income", "budget" column, final new dataset is usmovie1
usmovie1 = filter(usmovie, grepl("\\$", usa_gross_income))
usmovie1$usa_gross_income = gsub("\\$","",usmovie1$usa_gross_income)
usmovie1 = filter(usmovie1, grepl("\\$", budget))
usmovie1$budget = gsub("\\$","",usmovie1$budget)

#Convert budget and gross income to number
usmovie1$budget = as.numeric(usmovie1$budget)
usmovie1$usa_gross_income  = as.numeric(usmovie1$usa_gross_income )

# Correlation between average voting score and gross income
plot(usmovie1$usa_gross_income, usmovie1$avg_vote, col="blue", lwd=2, ylab="average voting score", xlab="gross income",cex.main=1.25, cex.lab=1.5, cex.axis=0.75)
abline(lm(avg_vote ~ usa_gross_income , data=usmovie1), col="red") 

# Correlation between average voting score and year
plot(usmovie1$year, usmovie1$avg_vote, col="blue", lwd=2, ylab="average voting score", xlab="year",cex.main=1.25, cex.lab=1.5, cex.axis=0.75) 
abline(lm(avg_vote ~ year , data=usmovie1), col="red")

# Correlation between  budget and usa gross income
plot(usmovie1$usa_gross_income, usmovie1$budget , col="blue", lwd=2, ylab="budget", xlab="gross income",cex.main=1.25, cex.lab=1.5, cex.axis=0.75)    
abline(lm(budget  ~ usa_gross_income , data=usmovie1), col="red") 

# Correlation between average vote and duration
plot(usmovie1$duration, usmovie1$avg_vote, col="blue", lwd=2, ylab="average voting score", xlab="duration",cex.main=1.25, cex.lab=1.5, cex.axis=0.75)    
abline(lm(avg_vote ~ duration , data=usmovie1), col="red") 

# Correlation between average vote and budget
plot(usmovie1$budget, usmovie1$avg_vote, col="blue", lwd=2, ylab="average voting score", xlab="budget",cex.main=1.25, cex.lab=1.5, cex.axis=0.75)    
abline(lm(avg_vote  ~ budget , data=usmovie1), col="red") 


#Selecting all numeric values and create new dataframe usmovie1_subset
usmovie1_subset = subset(usmovie1, select = c(year, duration, avg_vote,
                                           votes, budget, usa_gross_income,
                                           reviews_from_users, males_allages_avg_vote,
                                           females_allages_avg_vote, us_voters_rating, non_us_voters_rating, type_genre, sentiment))
str(usmovie1_subset)

#See the correlation in usmovie1_subset dataset
corrplot(cor(usmovie1_subset, use="complete.obs"),type="lower", method="number")

#Remove extremely correlated variables which are greater than 0.95
usmovie1_subset1 = subset(usmovie1_subset, select = -c(males_allages_avg_vote, 
                                                       females_allages_avg_vote,us_voters_rating,non_us_voters_rating))
#See the correlation again in new dataset usmovie1_subset1
corrplot(cor(usmovie1_subset1, use="complete.obs"),type="lower", method="number")

#Run linear regression
fit1 = lm(avg_vote ~ ., data=usmovie1_subset1)
summary(fit1)

##tree model
library(tree)
likehood = factor(ifelse(usmovie1_subset1$avg_vote<5,"Dislike","Like"))
usmovie_tree = data.frame(usmovie1_subset1,likehood)
head(usmovie_tree)
str(usmovie_tree)
table(usmovie_tree$likehood)
usmovie_tree=tree(likehood~.-avg_vote,usmovie_tree)
summary(usmovie_tree)
plot(usmovie_tree)
text(usmovie_tree, pretty=0, cex=0.8)
usmovie_tree

write_xlsx(usmovie1_subset1,"~/Desktop/data ucla extension 2020 fall/usmovie subset_Na Ni.xlsx")

##Organize the vote score into sections
table(usmovie1$avg_vote)
votesection = seq(0,10, by = 2)
usmovie1$votesection = votesection[findInterval(usmovie1$avg_vote, votesection)]
#Convert the votesections into factor
usmovie1$votesection = factor(usmovie1$votesection)
str(usmovie1)
table(usmovie1$votesection)

#Separate genre
usmovie1= usmovie1 %>% separate(genre, c("genre1", "genre2", "genre3"), ",")

#See how many unique values for each genre column
unique(usmovie1$genre1)
table(usmovie1$genre1)
usmovie1$genre1=gsub("Musical", "Music", usmovie1$genre1)
usmovie1$genre1=gsub("Documentary", "History", usmovie1$genre1)

unique(usmovie1$genre2)
table(usmovie1$genre2)
usmovie1$genre2=gsub("Musical", "Music", usmovie1$genre2)

unique(usmovie1$genre3)
table(usmovie1$genre3)
usmovie1$genre3=gsub("Musical", "Music", usmovie1$genre3)

str(usmovie1)


# Some simple EDA
# votesection against catergorial variables

ggplot(usmovie1, aes(x=decade,fill=votesection))+ geom_bar()+ theme_bw() +theme(text = element_text(size = 15)) 
ggplot(usmovie1, aes(x=genre1,fill=votesection))+ geom_bar()+ theme_bw() +theme(text = element_text(size = 16)) 
ggplot(na.omit(usmovie1), aes(x=genre2,fill=votesection))+ geom_bar()+ theme_bw() +theme(text = element_text(size = 16)) 
ggplot(na.omit(usmovie1), aes(x=genre3,fill=votesection))+ geom_bar()+ theme_bw() +theme(text = element_text(size = 16)) 

# votesection against numeric variables
ggplot(usmovie1, aes(x=votesection, y=budget, fill=votesection)) + geom_violin()+
  geom_boxplot(width=0.1, fill="white") + labs(title="Budget") + ylim(0,200000000)+theme(text = element_text(size = 16)) 
ggplot(usmovie1, aes(x=votesection, y=usa_gross_income, fill=votesection)) + geom_violin()+
  geom_boxplot(width=0.1, fill="white") + labs(title="Gross Income") + ylim(0,100000000)+theme(text = element_text(size = 16)) 
ggplot(usmovie1, aes(x=votesection, y=reviews_from_users, fill=votesection)) + geom_violin()+
  geom_boxplot(width=0.1, fill="white") + labs(title="Total reviews") + ylim(0,2500)+theme(text = element_text(size = 16)) 
ggplot(usmovie1, aes(x=votesection, y=type_genre, fill=votesection)) + geom_violin()+
  geom_boxplot(width=0.1, fill="white") + labs(title="Genre") + ylim(1,3)+theme(text = element_text(size = 16)) 

#Catogrize genres by numbers
pattern = c("Action","Adventure","Animation","Biography","Comedy","Crime","Drama","Family","Fantasy","Film-Noir","History","Horror","Music","Mystery",
            "Romance","Sci-Fi","Sport","Thriller","War","Western")
replacement = as.character(1:20)

for (i in seq_along(pattern)) {
  usmovie1$genre1 <- gsub(pattern[i], replacement[i], usmovie1$genre1)
}

for (i in seq_along(pattern)) {
  usmovie1$genre2 <- gsub(pattern[i], replacement[i], usmovie1$genre2)
}

for (i in seq_along(pattern)) {
  usmovie1$genre3 <- gsub(pattern[i], replacement[i], usmovie1$genre3)
}
str(usmovie1)

usmovie1$genre1 = as.numeric(usmovie1$genre1)
usmovie1$genre2 = as.numeric(usmovie1$genre2)
usmovie1$genre3 = as.numeric(usmovie1$genre3)
usmovie1$type_genre= factor(usmovie1$type_genre)

#Each Genre's average voting score
usmovie_genre = usmovie1 %>% select("title","genre1","genre2","genre3","avg_vote")
action = usmovie_genre %>% filter(genre1==1 | genre2==1| genre3==1)
mean(action[,"avg_vote"])

adventure = usmovie_genre %>% filter(genre1==2 | genre2==2| genre3==2)
mean(adventure[,"avg_vote"])

animation= usmovie_genre %>% filter(genre1==3 | genre2==3| genre3==3)
mean(animation[,"avg_vote"])

biography= usmovie_genre %>% filter(genre1==4 | genre2==4| genre3==4)
mean(biography[,"avg_vote"])

comedy= usmovie_genre %>% filter(genre1==5 | genre2==5| genre3==5)
mean(comedy[,"avg_vote"])

crime= usmovie_genre %>% filter(genre1==6 | genre2==6| genre3==6)
mean(crime[,"avg_vote"])

drama= usmovie_genre %>% filter(genre1==7 | genre2==7| genre3==7)
mean(drama[,"avg_vote"])

family= usmovie_genre %>% filter(genre1==8 | genre2==8| genre3==8)
mean(family[,"avg_vote"])

fantasy= usmovie_genre %>% filter(genre1==9 | genre2==9| genre3==9)
mean(fantasy[,"avg_vote"])

film_noir= usmovie_genre %>% filter(genre1==10 | genre2==10| genre3==10)
mean(film_noir[,"avg_vote"])

history= usmovie_genre %>% filter(genre1==11 | genre2==11| genre3==11)
mean(history[,"avg_vote"])

horror= usmovie_genre %>% filter(genre1==12 | genre2==12| genre3==12)
mean(horror[,"avg_vote"])

music= usmovie_genre %>% filter(genre1==13 | genre2==13| genre3==13)
mean(music[,"avg_vote"])

mystery= usmovie_genre %>% filter(genre1==14 | genre2==14| genre3==14)
mean(mystery[,"avg_vote"])

romance= usmovie_genre %>% filter(genre1==15 | genre2==15| genre3==15)
mean(romance[,"avg_vote"])

sci_fi= usmovie_genre %>% filter(genre1==16 | genre2==16| genre3==16)
mean(sci_fi[,"avg_vote"])

sport= usmovie_genre %>% filter(genre1==17 | genre2==17| genre3==17)
mean(sport[,"avg_vote"])

thriller= usmovie_genre %>% filter(genre1==18 | genre2==18| genre3==18)
mean(thriller[,"avg_vote"])

war= usmovie_genre %>% filter(genre1==19 | genre2==19| genre3==19)
mean(war[,"avg_vote"])
western= usmovie_genre %>% filter(genre1==20 | genre2==20| genre3==20)
mean(western[,"avg_vote"])

genre_mean = data.frame (genre  = c("Action","Adventure","Animation","Biography","Comedy","Crime","Drama","Family","Fantasy","Film-Noir","History","Horror","Music","Mystery",
                                    "Romance","Sci-Fi","Sport","Thriller","War","Western"),
                  meanscore = c(mean(action[,"avg_vote"]), mean(adventure[,"avg_vote"]), mean(animation[,"avg_vote"]),mean(biography[,"avg_vote"]), mean(comedy[,"avg_vote"]), mean(crime[,"avg_vote"]),
                                mean(drama[,"avg_vote"]), mean(family[,"avg_vote"]), mean(fantasy[,"avg_vote"]),mean(film_noir[,"avg_vote"]),mean(history[,"avg_vote"]),mean(horror[,"avg_vote"]),
                                mean(music[,"avg_vote"]), mean(mystery[,"avg_vote"]), mean(romance[,"avg_vote"]), mean(sci_fi[,"avg_vote"]), mean(sport[,"avg_vote"]), mean(thriller[,"avg_vote"]),
                                mean(war[,"avg_vote"]), mean(western[,"avg_vote"])),
                  count = c(1557,1168,309,387, 2679, 1259,3411,439,520,4,170,679,275,559,1148,471,177,1035,114,56))
ggplot(genre_mean, aes(x=genre, y=meanscore)) + geom_bar(stat = "identity",width=0.5) + geom_hline(yintercept = 7,col="red") + coord_flip()

write_xlsx(genre_mean,"~/Desktop/data ucla extension 2020 fall/genre_mean_Na Ni.xlsx")

#change genre1/2/3 to factor
usmovie1$genre1 = factor(usmovie1$genre1)
usmovie1$genre2 = factor(usmovie1$genre2)
usmovie1$genre3 = factor(usmovie1$genre3)
usmovie1$type_genre= factor(usmovie1$type_genre)
str(usmovie1)

# Logistic model:factor:votesection
logit0 = glm(votesection ~ genre1 ,usmovie1, family="binomial")
summary(logit0) 

logit1 = glm(votesection ~ genre1 + genre2 ,usmovie1, family="binomial")
summary(logit1) 

logit2 = glm(votesection ~ genre1 + genre2 + genre3 ,usmovie1, family="binomial")
summary(logit2) 

logit3 = glm(votesection ~ decade + duration + budget + votes ,usmovie1, family="binomial")
summary(logit3) #AIC: 99.003

logit4 = glm(votesection ~ year + duration + budget + votes + type_genre,usmovie1, family="binomial")
summary(logit4)  #best: 98.704


#logit4 is the best model, create dataframe based on that
usmovie2 = subset(usmovie1, select = c(year, duration, votes, budget, type_genre, votesection))

#remove NA rows
colSums(sapply(usmovie2, is.na))
usmovie2 = na.omit(usmovie2)

#use the dataset to create a partition (60% training 40% testing)
set.seed(100)
fullset <- createDataPartition(usmovie2$votesection, p=0.60, list=FALSE)
# select 60% of data to train the models
trainset <- usmovie2[fullset,]
# select 40% of the data for testing
testset <- usmovie2[-fullset,]
testset1 <- testset[,-8]
dim(trainset)
dim(testset)

library(lattice)
library(e1071)
library(rattle)
library(arm)
library(xgboost)
library(kernlab)

## 10-fold Cross-Validation
##
control = trainControl(method="cv", number=10)
metric = "Accuracy"
# There are other metrics, such as "RMSE" or "ROC", which need more complex setting.

# Linear Discriminant Analysis (LDA)
set.seed(100)
fit.lda = train(votesection~., data=trainset, method="lda", metric=metric, trControl=control)
fit.lda
# Classfication and Regression Trees (CART)
set.seed(100)
fit.cart = train(votesection~., data=trainset, method="rpart", metric=metric, trControl=control)
fit.cart
# k-Nearest Neighbors (KNN)
set.seed(100)
fit.knn = train(votesection~., data=trainset, method="knn", metric=metric, trControl=control)
fit.knn
# Bayesian Generalized Linear Model 
set.seed(100)
fit.logi = train(votesection~., data=trainset, method="bayesglm", metric=metric, trControl=control)
fit.logi
# Support Vector Machines (SVM) --> a long long time
set.seed(100)
fit.svm = train(votesection~., data=trainset, method="svmRadial", metric=metric, trControl=control)
fit.svm
# Random Forest
set.seed(100)
fit.rf = train(votesection~., data=trainset, method="rf", metric=metric, trControl=control)
fit.rf
# Gradient Boosting Machines/XGBoost
set.seed(100)
fit.xgb = train(votesection~., data=trainset, method="xgbLinear", metric=metric, trControl=control)
fit.xgb

# Select Best Model
# summarize accuracy of models
results = resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, logi=fit.logi, svm=fit.svm, rf=fit.rf, xgb=fit.xgb))
summary(results) # Random Forest is the best

# Testset Prediction
# Test set accuracy: 71.4%
testset1$votesection = predict(fit.rf, newdata=testset1)
mean(testset1$votesection==testset$votesection)
table(testset1$votesection,testset$votesection)

#ROC and optimal cutoff point: The ROC - curve is defined for the classification of two groups,
#so what multiclass makes is to compute the classification for "one group against the rest". multiclass.roc function doesn't allow you to represent the curves
library(pROC)
pred = predict(fit.rf, type = "prob", trainset)
pred.1 = data.matrix(pred)
rf.roc = multiclass.roc(response = trainset$votesection, pred.1, levels = c(0,2,4,6,8) )
plot(rf.roc, legacy.axes = TRUE, print.auc.y = 1, print.auc = TRUE) #doesn't work!
coords(a, "best", "threshold")

