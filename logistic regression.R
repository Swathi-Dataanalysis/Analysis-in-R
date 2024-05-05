library(aplore3)
df <- burn1000
lr.fit = glm(death~age+gender+race+tbsa+inh_inj+flame, data=df, 
             family=binomial(link = "logit"))
summary(lr.fit)
predict(lr.fit, newdata = df[1,])
# verify g(x)
-7.695153+(26.6*0.082890)-0.201494+(25.3*0.089345)+0.582578-0.701389
# change to response
predict(lr.fit, newdata = df[1,], type="response")
# lets add g(x) and prob to all rows
df$g.of.x = predict(lr.fit, newdata = df)
df$prob = predict(lr.fit, newdata = df, type="response")
# reverse engineer g(x) using probability and fancy math
df$ln.calc = log(df$prob/(1-df$prob))
#calculate what g(x) would be if they were not male
df$g.of.x.non.male = df$g.of.x + ifelse(df$gender == "Male", 0.201494, 0)
# recalculate probability based on new non male g(x)
df$prob.non.male = exp(df$g.of.x.non.male)/(1+exp(df$g.of.x.non.male))
# calculate change in probabilities
df$diff.non.male = df$prob - df$prob.non.male

