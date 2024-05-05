library(aplore3)
library(rpart)
library(rpart.plot)
df <- burn1000
df.len = dim(df)[1]
part.pct = 0.50
minsplits = c(1,2,3,4,5,6,7,8,9,10,12,14,16,18,20,22,24,26,28,30,40, 50)
runs = length(minsplits)
misclass.test = rep(0,runs)
misclass.train = rep(0,runs)
number.of.trials = 20
for (i in 1:runs) {
  train.wrong = 0
  test.wrong = 0
  for (j in 1:number.of.trials) {  
    
    training.rows = sample(1:df.len, round(df.len*part.pct))
    training.set = df[training.rows,]
    test.set = df[-training.rows,]
    tree.fit = rpart(death~age+gender+tbsa+race+flame+inh_inj, data=training.set, method = "class", 
                     control = rpart.control(minsplit=minsplits[i], minbucket = 1))
    
    train.wrong = train.wrong + sum(training.set$death != predict(tree.fit, newdata = training.set, type="class"))
    test.wrong = test.wrong + sum(test.set$death != predict(tree.fit, newdata = test.set, type="class"))
  }
  
  # Calc training error
  misclass.train[i] = train.wrong/(number.of.trials*dim(training.set)[1])
  
  # Calc test error (unseen data/hold out)
  misclass.test[i] = test.wrong/(number.of.trials*dim(test.set)[1])
}
ymin = min(c(misclass.test),(misclass.train))
ymax = max(c(misclass.test),(misclass.train))


plot(minsplits, misclass.train, col="blue", type='b', ylim=c(ymin,ymax), ylab="Misclassification Rate", xlab="Min Splits")
grid()
points(minsplits, misclass.test, col="red", type='b')


