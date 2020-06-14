if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(hexbin)) install.packages("hexbin", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(dummies)) install.packages("dummies", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

download.file("https://raw.githubusercontent.com/souravdutta20/CYO_Project/master/Bank_Personal_Loan_Modelling.csv","Bank_Personal_Loan_Modelling.csv")

Bank_data<- read.csv("Bank_Personal_Loan_Modelling.csv")

 head(Bank_data)

# Check observation & Variables 
dim(Bank_data)
# To see top 6 records
head(Bank_data)
# See the end 6 record
tail(Bank_data)

# to check missing data
Bank_data[!complete.cases(Bank_data),]

# To check variables' class
lapply(Bank_data,class)
str(Bank_data)
# How many unique customers present in database 
n_distinct(Bank_data$ID)

# get Descriptive Statistics
summary(Bank_data)

# checking customers with Age<=18 and Age>65 (As Bank doesn't want to target them)

data_less18_ge65<-Bank_data%>% filter(Age<=18 | Age> 65)
nrow(data_less18_ge65)

# Removing customers with age less than 18 and greater than 65
Bank_data <- subset(Bank_data,Age>18 & Age< 65)
Bank_data %>% filter(Age<=18 | Age> 65)
nrow(Bank_data)

# checking customers with negative experience 

negative_exp<-Bank_data %>% filter(Experience<0)
nrow(negative_exp)

# Removing those 52 rows (issue with data having experience less than 0)
Bank_data <- subset(Bank_data,Experience>0)
nrow(Bank_data)
Bank_data %>% filter(Experience<0)

# Removing ZIP.Code from data, as this is not going to be considered as a predictor 
dim(Bank_data)
Bank_data <- Bank_data %>% select(-ZIP.Code)
dim(Bank_data)

# Deleting rows with missing values if any

Bank_data<- na.omit(Bank_data)
dim(Bank_data)

# Changing the class of variables
Bank_data$Personal.Loan <- as.factor(Bank_data$Personal.Loan)
Bank_data$Securities.Account <-as.factor(Bank_data$Securities.Account)
Bank_data$CD.Account <-as.factor(Bank_data$CD.Account)
Bank_data$Online <-as.factor(Bank_data$Online)
Bank_data$CreditCard<-as.factor(Bank_data$CreditCard)
Bank_data$Family<-as.factor(Bank_data$Family)
Bank_data$Education<-as.factor(Bank_data$Education)
str(Bank_data)
summary(Bank_data)

# Plot 

# 1. Personal Loan Distribution
    
  Bank_data %>% group_by(Personal.Loan)%>% 
summarise(Customer_count=n())%>% ggplot(aes(Personal.Loan,Customer_count,fill=Personal.Loan))+
geom_bar(stat="identity")+ggtitle("Distribution of Customers opted for Personal Loan") 
  
  # 2. Personal Loan  & Education Distribution 
  
  Bank_data %>% group_by(Personal.Loan,Education)%>% 
    summarise(Customer_count=n())%>% ggplot(aes(Education,Customer_count,fill=Personal.Loan))+
    geom_bar(stat="identity")+ggtitle("Distribution of Customers opted for Personal Loan and Education") 
  #Graduate & Advanced/Professional have opted for loans compared to undergraduate

  
  # 3. Personal Loan  & Income Distribution
  
  Bank_data %>% group_by(Personal.Loan,Income)%>% 
    summarise(Customer_count=n())%>% ggplot(aes(Income,Customer_count,fill=Personal.Loan))+
    geom_bar(stat="identity")+ggtitle("Distribution of Customers opted for Personal Loan and Income") 
  # Higher income (>$100K) group opted for Personal Loan
  
  Bank_data %>%ggplot(aes(Personal.Loan,Income,fill=Personal.Loan))+geom_boxplot()+ggtitle("Distribution of Customers opted for Personal Loan and Income")
  
  
  # 4. Personal Loan and CCAvg Distribution
  Bank_data %>% group_by(Personal.Loan,CCAvg)%>% 
    summarise(Customer_count=n())%>% ggplot(aes(Personal.Loan,Customer_count,fill=CCAvg))+
    geom_bar(stat="identity")+ggtitle("Distribution of Customers opted for Personal Loan and CCAvg") 
  
  Bank_data %>%ggplot(aes(Personal.Loan,CCAvg, fill=Personal.Loan))+geom_boxplot()+ggtitle("Distribution of Customers opted for Personal Loan and CCAvg") 
  
  #5.Personal Loan and CD.Account Distribution
  
  Bank_data %>%ggplot(aes(Personal.Loan,CD.Account, fill=Personal.Loan))+geom_boxplot()+ggtitle("Distribution of Customers opted for Personal Loan and CD.Account") 
  
  Bank_data %>% group_by(Personal.Loan,CD.Account)%>% 
    summarise(Customer_count=n())%>% ggplot(aes(Personal.Loan,Customer_count,fill=CD.Account))+
    geom_bar(stat="identity")+ggtitle("Distribution of Customers opted for Personal Loan and CCAvg") 
  
  
  # 6. Family and Personal.Loan
  Bank_data %>% group_by(Personal.Loan,Family)%>% 
    summarise(Customer_count=n())%>% ggplot(aes(Family,Customer_count,fill=Personal.Loan))+
    geom_bar(stat="identity")+ggtitle("Distribution of Customers opted for Personal Loan and Family") 
  # Family size doesn't have any impact
  
  # 7. Online & Personal.Loan
  Bank_data %>% group_by(Personal.Loan,Online)%>% 
    summarise(Customer_count=n())%>% ggplot(aes(Online,Customer_count,fill=Personal.Loan))+
    geom_bar(stat="identity")+ggtitle("Distribution of Customers opted for Personal Loan and Online") 
  # Customers having online accounts comparatively opted for Personal loan
  
  #8. Credit card & Personal Loan
  Bank_data %>% group_by(Personal.Loan,CreditCard)%>% 
    summarise(Customer_count=n())%>% ggplot(aes(CreditCard,Customer_count,fill=Personal.Loan))+
    geom_bar(stat="identity")+ggtitle("Distribution of Customers opted for Personal Loan and Online") 
  
  # Customers having credit card has less personal Loan
  
  # 9. Family , Income and personal Loan
  
  Bank_data %>%ggplot(aes(Family,Income, fill=Personal.Loan))+geom_boxplot()+ggtitle("Distribution of Customers opted for Personal Loan and CD.Account") 
  
  # Family with income less than $100k is more unlikely to take the loan
  
  # Correlation Matrix
  
  cor_dat <- Bank_data %>% select(Age, Experience, Income, CCAvg, Mortgage)
  
  head(cor_dat)
  

  temp <- cor_dat %>% select(one_of("Age", "Experience", "Income", "CCAvg", "Mortgage" 
                                   )) %>% as.matrix()
  M <- cor(temp, use = "pairwise.complete.obs")
  
  corrplot(M, order = "hclust", addrect = 2, type = "lower", col = brewer.pal(n = 8, name = "RdBu"))
  
  #Observation: Income and CCAvg are highly correlated and Age and Experience also highly correlated


  cor(data.frame(x = Bank_data$Income, y = Bank_data$CCAvg))
  cor(data.frame(x = Bank_data$Age, y = Bank_data$Experience))
  
  get_cor <- function(df){
    m <- cor(df$x, df$y, use="pairwise.complete.obs");
    eq <- substitute(italic(r) == cor, list(cor = format(m, digits = 2)))
    as.character(as.expression(eq));
  }
  

  #Age vs Experience
  cor_dat %>%
    ggplot(aes(Age, Experience)) + stat_bin_hex(bins = 20) + scale_fill_distiller(palette = "Spectral") +
    stat_smooth(method = "lm", color = "orchid", size = 1) +
    annotate("text", x = 40, y = 1.5, label = get_cor(data.frame(x = cor_dat$Age, y = cor_dat$Experience)), 
             parse = TRUE, color = "red", size = 4) + ylab("Experience") + xlab("Age")
  
  # Income vs CCAvg
  cor_dat %>%
    ggplot(aes(Income, CCAvg)) + stat_bin_hex(bins = 20) + scale_fill_distiller(palette = "Spectral") +
    stat_smooth(method = "lm", color = "orchid", size = 1) +
    annotate("text", x = 100, y = 3.5, label = get_cor(data.frame(x = cor_dat$Income, y = cor_dat$CCAvg)), 
             parse = TRUE, color = "red", size = 4) + ylab("CCAvg") + xlab("Income")
  


  # Dummies for Family and Education  
  
  
  Bank_data_new<- dummy.data.frame(Bank_data,name=c("Family","Education"),sep=".")
  
  str(Bank_data_new)
  
  Bank_data_new$Family.1<- as.factor(Bank_data_new$Family.1)
  Bank_data_new$Family.2<- as.factor(Bank_data_new$Family.2)
  Bank_data_new$Family.3<- as.factor(Bank_data_new$Family.3)
  Bank_data_new$Family.4<- as.factor(Bank_data_new$Family.4)
  Bank_data_new$Education.1<- as.factor(Bank_data_new$Education.1)
  Bank_data_new$Education.2<- as.factor(Bank_data_new$Education.2)
  Bank_data_new$Education.3<- as.factor(Bank_data_new$Education.3)
  
  str(Bank_data_new)
  dim(Bank_data_new)
 
  # Removing ID as this is not going to use in our Model
  Bank_data_new <- Bank_data_new %>% select(-ID)
  dim(Bank_data_new)
  
  # Test and Train data set creation
  test_index <- createDataPartition(y = Bank_data_new$Personal.Loan, times = 1, p = 0.2, 
                                    list = FALSE)
  train_set <- Bank_data_new[-test_index,]
  test_set <- Bank_data_new[test_index,]
  
  dim(train_set)
  dim(test_set)
  head(train_set)
  
# Model 1: Logistic Regression
  
 y<- train_set$Personal.Loan
  fit_glm <- glm(y~Age+Experience+Income+CCAvg+Family.1+Family.2+Family.3+Family.4+Education.1+Education.2+Education.3+Mortgage+Securities.Account+CD.Account+Online+CreditCard, data=train_set, family="binomial")
  p_hat_logistic <- predict(fit_glm, test_set,type = "response")
  y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 1, 0))
  confusionMatrix(data = y_hat_logistic, reference = test_set$Personal.Loan)
  confusionMatrix(data = y_hat_logistic, reference = test_set$Personal.Loan)$overall["Accuracy"]
  
  summary(fit_glm)
  
  anova(fit_glm,test="Chisq")
  
  train_set %>% ggplot(aes(Age+Experience+Income+CCAvg, color = Personal.Loan)) + geom_density()
  
  model_result<-tibble(method = "Logistic Regression", Accuracy = confusionMatrix(data = y_hat_logistic, reference = test_set$Personal.Loan)$overall["Accuracy"]
  )
  
  model_result
  
  
  # Model 2: K-nearest neighbors (kNN)
  
  # k=1
  knn_fit<-knn3(y~Age+Experience+Income+CCAvg+Family.1+Family.2+Family.3+Family.4+Education.1+Education.2+Education.3+Mortgage+Securities.Account+CD.Account+Online+CreditCard,data=train_set,k=1)
  
  y_hat_knn<-predict(knn_fit,test_set,type="class")
  
  confusionMatrix(data = y_hat_knn, reference = test_set$Personal.Loan)
  
  confusionMatrix(data = y_hat_knn, reference = test_set$Personal.Loan)$overall["Accuracy"]
  
  #k=400
  knn_fit<-knn3(y~Age+Experience+Income+CCAvg+Family.1+Family.2+Family.3+Family.4+Education.1+Education.2+Education.3+Mortgage+Securities.Account+CD.Account+Online+CreditCard,data=train_set,k=400)
  
  y_hat_knn<-predict(knn_fit,test_set,type="class")
  
  confusionMatrix(data = y_hat_knn, reference = test_set$Personal.Loan)
  
  confusionMatrix(data = y_hat_knn, reference = test_set$Personal.Loan)$overall["Accuracy"]
  
  # optimum k value selection
  
  ks<-seq(1,400,2)
  
  accuracy<-map_df(ks,function(k){
    
    knn_fit<-knn3(y~Age+Experience+Income+CCAvg+Family.1+Family.2+Family.3+Family.4+Education.1+Education.2+Education.3+Mortgage+Securities.Account+CD.Account+Online+CreditCard,data=train_set,k=k)
    
    y_hat_knn<-predict(knn_fit,test_set,type="class")
    
    test_error<-confusionMatrix(data = y_hat_knn, reference = test_set$Personal.Loan)$overall["Accuracy"]
    tibble(test=test_error)
  }
    
    )
  # pick the k that maximizes accuracy using the estimate built on test data
  ks[which.max(accuracy$test)]
  max(accuracy$test)
  
  model_result <- bind_rows (model_result ,tibble(method = "K-nearest neighbors (kNN)", Accuracy = max(accuracy$test)))
  model_result
  
  # Model 3:RandomForest


  fit <- randomForest(y~., data = train_set) 
  plot(fit)  
# General model
  train_rf <- randomForest(Personal.Loan ~ ., data=train_set)
  confusionMatrix(predict(train_rf, test_set), test_set$Personal.Loan)$overall["Accuracy"]
  model_result <- bind_rows (model_result ,tibble(method = "RandomForest General", Accuracy = confusionMatrix(predict(train_rf, test_set), test_set$Personal.Loan)$overall["Accuracy"]))
  model_result
  
  # Model 4:Rborist
  
  # use cross validation to choose parameter
  train_rf_2 <- train(Personal.Loan ~ .,
                      method = "Rborist",
                      tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                      data = train_set)
  confusionMatrix(predict(train_rf_2, test_set), test_set$Personal.Loan)$overall["Accuracy"]
  
  
  control <- trainControl(method="cv", number = 5, p = 0.8)
  grid <- expand.grid(minNode = c(1,5) , predFixed = c(10, 15, 25, 35, 50))
  train_rf <-  train(Personal.Loan ~ .,
                     data=train_set,
                     method = "Rborist",
                     nTree = 50,
                     trControl = control,
                     tuneGrid = grid,
                     nSamp = 5000)
  
  ggplot(train_rf)
  train_rf$bestTune
  
  # best fit
  grid <- expand.grid(minNode = train_rf$bestTune$minNode , predFixed = train_rf$bestTune$predFixed)
  
  train_rf_best <-  train(Personal.Loan ~ .,
                     data=train_set,
                     method = "Rborist",
                     nTree = 1000,
                     trControl = control,
                     tuneGrid = grid
                     )
  
  confusionMatrix(predict(train_rf_best, test_set), test_set$Personal.Loan)$overall["Accuracy"]
  
  model_result <- bind_rows (model_result ,tibble(method = "Rborist Grid tune", Accuracy =   confusionMatrix(predict(train_rf_best, test_set), test_set$Personal.Loan)$overall["Accuracy"]))
  model_result
  
  
  # Model 5:rpart
  train_rpart <- train(Personal.Loan ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data = train_set)
  ggplot(train_rpart)
  
  plot(train_rpart$finalModel, margin = 0.1)
  text(train_rpart$finalModel, cex = 0.75)
  
  confusionMatrix(predict(train_rpart, test_set), test_set$Personal.Loan)$overall["Accuracy"]
  
  model_result <- bind_rows (model_result ,tibble(method = "rpart Decision Tree", Accuracy =   confusionMatrix(predict(train_rpart, test_set), test_set$Personal.Loan)$overall["Accuracy"]))
  model_result
  
    