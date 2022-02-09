## Data Preprocessing

#For the character data

#(1) remove the "Player", "Team" because they are almost unique identifiers;
#(2) remove the "ht" , "Role" column because they are skewed, thus variance is low;
#(3) Remove "Conference" too, with too much dummy variables considering the parsimony,
#(4) Keep "yr" because they can be approximately seen as uniformly distributed;
#(5) Convert "num" into numeric data


library("dplyr")  
df_cha <- select_if(df, is.character)
colnames(df_cha)

df$num=as.numeric(df$num)

length(unique(df$Player))
length(unique(df$Team))

charcolumn=c("Conference", "yr", "ht", "Role")

library("ggplot2")
for (col in charcolumn){
  print(ggplot(df, aes(x = df[,col])) +
  geom_bar()+ ggtitle(col)+labs(x = col)+
  theme_update(plot.title = element_text(hjust = 0.5))+coord_flip())
}

df_cha<-subset (df_cha, select = -c(num,Player, Team,ht ,Role, Conference))

#(1) remove column "Rec.Rank", "dunksmade..dunksmade.dunksmiss.", and "pick" bacause they have high NAN rate, which are respectively 0.69,0.541, and 1.
#(2) For the rest of the NAN value, using Median to replace them.

round(colSums(is.na(df))/nrow(df),2)

df_num <- select_if(df, is.numeric)

threshold<-0.2 
df_num<-df_num %>% 
  select(where(~mean(is.na(.))< threshold))

df_num<-df_num %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))

#Drop FT_per, twoP_per, TP_per, year, ast.tov, rimmade..rimmade.rimmiss., midmade..midmade.midmiss., dporpag, oreb, stl, oreb, stl, blk because they have variances less than 1

df_num %>% summarise_if(is.numeric, var)
df_num<-subset (df_num, select = -c(FT_per, twoP_per, TP_per, year, ast.tov, rimmade..rimmade.rimmiss., midmade..midmade.midmiss., dporpag, oreb, stl, oreb, stl, blk))
  
# Drop the column with multicollinearity

options(max.print=1000)
cor(df_num)
df_num<-subset (df_num, select = -c(TPA, dunksmiss.dunksmade, dbpm, drtg, obpm, gbpm, dgbpm, obpm, stops, Minutes.., twoPM, twoPA, mp, dreb, FTM, rimmade.rimmiss, midmade.midmiss,Effective.field.goal.., True.shooting.., adjoe))

df_clean<-cbind(df_cha, df_num)

library(car)
lm_try<-lm(pts~.,data=df_clean)
vif(lm_try)

## Now we have the clean data!!!

## Split Dataset

set.seed(123)
row.number <- sample(1:nrow(df_clean), 0.15*nrow(df_clean))
train = df_clean[row.number,]
test = df_clean[-row.number,]
dim(train)
dim(test)

# remove the less significant feature
# model2 = update(model1, ~.-Offensive.rating-TO_per-blk_per-stl_per-ftr-pfr-pid-dunksmade)
# summary(model2)

model1 = lm(pts~., data=train)
summary(model1)

pred_in <- predict(model1, newdata = train)
error_in <- train$pts - pred_in
RMSE_in <- sqrt(mean(error_in^2))
RMSE_in

pred_out <- predict(model1, newdata = test)

# In sample, the $R^2$ for model on the training model is 0.9428, and the RMSE is 1.141253.

error_out <- test$pts - pred_out
SS.residual<-sum((error_out)^2)
SS.total<-sum((test$pts - mean(test$pts))^2)
test.rsq <- 1 - SS.residual/SS.total  
test.rsq
RMSE_out <- sqrt(mean(error_out^2))
RMSE_out

# Out of sample,the $R^2$ is 0.9263722, is lower than the one results from the in sample $R^2$. Based on RMSE = 1.250131, we can conclude that on an average predicted value will be off by 1.250131 from the actual value.
#25 features are used in the model.

## 5 fold Cross Validation

library(caret)

train_control<-trainControl(method = "cv", number = 5,
                          savePredictions=TRUE)
model_cv <- train(pts ~., data = train, method = "lm", 
                  trControl = train_control)

# Predict overall socre
model_cv$pred
# Each fold's R-square
model_cv$resample
# 5-fold CV's average R-square
model_cv

# After the cross validation, the average $R^2$ of the "test" set on the training data is 0.9331989, which is lower than the $R^2$ of the whole training data 0.9428. It shows that using cross validation, it helps us to know the true performance of model, prevents our model from overfitting the training dataset, which is also the reason the $R^2$ is less.
# The problem of the cross-validation is that it is time consuming when fitting K, and it can be unstable: when performing CV on many different samples.

## Lasso Regression

library(glmnet)

x_train<- model.matrix(pts~., train)[,-1]
y_train<-as.matrix(train$pts) 

grid = 10^seq(1, -2, length = 100)
lasso_mod = glmnet(x_train, 
                   y_train, 
                   alpha = 1, 
                   lambda = grid) # Fit lasso model on training data

plot(lasso_mod)  
lambda_cv<- cv.glmnet(x_train, y_train,nfolds = 5)
best_lambda<-lambda_cv$lambda.1se
best_lambda
#lambda_cv$lambda.min
#coef(lambda_cv, lambda_cv$lambda.1se)
plot(lambda_cv)

best_model <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda)
coef(best_model)

x_test<- model.matrix(pts~., test)[,-1]
pred_lasso <- predict(best_model, s = best_lambda, newx = x_test)

error_lasso <- test$pts - pred_lasso

SS.residual_lasso<-sum((error_lasso)^2)
SS.total_lasso<-sum((test$pts - mean(test$pts))^2)

test.rsq <- 1 - SS.residual_lasso/SS.total_lasso
test.rsq

RMSE_lasso <- sqrt(mean(error_lasso^2))
RMSE_lasso


















