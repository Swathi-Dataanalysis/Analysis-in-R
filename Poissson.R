library(datasets)

# sample poisson distributions with lambdas of 1, 3, 5.5, and 8
plot(table(rpois(100000, 8)), type="l", col="blue", ylim=c(0, 75000), ylab="Occurs", xlab="Number",
     main="Poisson Distributions With Different Lambdas")
grid()
points(table(rpois(100000, 5.5)), type="l", col="green")
points(table(rpois(100000, 3)), type="l", col="red")
points(table(rpois(100000, 1)), type="l", col="purple")
legend("topright", legend = c("8", "5.5","3","1"), fill=c("blue","green","red","purple"))

mean(rpois(100000, 8))
mean(rpois(100000, 5.5))
mean(rpois(100000, 3))
mean(rpois(100000, 1))


df <- warpbreaks
poisson.fit <- glm(breaks ~ wool + tension, df, family = poisson(link = "log"))
summary(poisson.fit)
pred.response <- predict(poisson.fit, 
                         newdata = data.frame(wool=c('B', 'A', 'B', 'A'), tension = c('L','L','L','H')), 
                         type="response")
pred.raw <- predict(poisson.fit, 
                    newdata = data.frame(wool=c('B', 'A', 'B', 'A'), tension = c('L','L','L','H')))


