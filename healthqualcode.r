library(car)
library(dplyr)
library(lmtest)

data=read.table("http://anson.ucdavis.edu/~azari/sta138/final.dat",header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

model.null = glm(dav ~ 1, data=data, family = binomial (link="logit"))

model.full = glm(dav ~ pcs +mcs+ beck+ pgend+ age+ educat, data=data, family = binomial (link="logit"))

model.final = glm(dav ~ mcs + beck + pgend + age + educat, data = data, family = binomial (link = "logit"), na.action(na.omit))

pear.stdresid = resid( model.final, type = "pearson") / sqrt( 1 - lm.influence( model.final)$hat)       

summary(model.null)
summary(model.final)

step(model.null, scope = list(upper=model.full), direction="both", test="Chisq", data=data)

hist(data[which(data[,1]==0), 3], main = "Dist of Mental SF-36 Score for No Diagnosis", xlab = "Mental SF-36 Score")

hist(data[which(data[,1]==1), 3], main = "Dist of Mental SF-36 Score for Yes Diagnosis", xlab = "Mental SF-36 Score")

hist(data[which(data[,1]==0), 4], main = "Dist of Beck Score for No Diagnosis", xlab = "Beck Score")

hist(data[which(data[,1]==1), 4], main = "Dist of Beck Score for Yes Diagnosis", xlab = "Beck Score")

Anova(model.final, type="II", test="Wald")

anova(model.final, model.null, test="Chisq")

plot(pear.stdresid, main="Dispersion of Std. Residuals", ylab="Std. Residuals")