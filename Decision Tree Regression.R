library(aplore3)
library(rpart)
library(rpart.plot)
df <- mtcars
df.len = dim(mtcars)[1]
part.pct = 0.50
training.rows = sample(1:df.len, round(df.len*part.pct))
training.set = df[training.rows,]
test.set = df[-training.rows,]
runs = 6
rmse.test = rep(0,runs)
rmse.train = rep(0,runs)
for (i in 1:runs) {
  
  tree.fit = rpart(disp~., data=training.set, method = "anova", 
                   control = rpart.control(minsplit=i, minbucket = 1, cp = 0))
  #                 control = rpart.control(cp = 0.001) )
  rpart.plot(tree.fit)
  
  # Calc training error
  rmse.train[i] = sqrt(sum((training.set$disp - predict(tree.fit, newdata = training.set))^2))/dim(training.set)[1]
  
  # Calc test error (unseen data/hold out)
  rmse.test[i] = sqrt(sum((test.set$disp - predict(tree.fit, newdata = test.set))^2))/dim(test.set)[1]
}
ymin = min(c(rmse.test),(rmse.train))
ymax = max(c(rmse.test),(rmse.train))


plot(1:runs, rmse.train, col="blue", ylim=c(ymin, ymax), type='l')
points(1:runs, rmse.test, col="red", type='l')


