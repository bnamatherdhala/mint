# Load the data.

mint <- read.csv("file:///C:/Users/bharatwaja/Desktop/mint.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

# Build the training/validate/test datasets.

set.seed(10001) 
# A pre-defined value is used to reset the random seed so that results are repeatable.
names(train)
[1] "Userid"                  "Profile_creation_time"  
[3] "Number.of.Bank.Accounts" "Monthly.Spend"          
[5] "Monthly.Income"          "Number_of_logins"       
[7] "Id"                      "Offerid"                
[9] "Offer.time_stamp"        "Event"                  
[11] "Merchant"  

nobs <- nrow(mint) # 71272 observations 

train = mint[sample(nrow(mint),40000),] # 40000 observations
validate = mint[sample(nrow(mint),20000),] # 20000 observations
test = mint[sample(nrow(mint),15000),] # 15000 observations

# The 'Hmisc' package provides the 'describe' function.

library(Hmisc, quietly=TRUE)
describe(train)

# to check if there are any missing values
is.na(Monthly.Income)
is.na(Monthly.Spend)

# Generate a description of the dataset.


# The following variable selections have been noted.

input <- c("Userid", "Profile_creation_time", "Number.of.Bank.Accounts", "Monthly.Spend",
     "Monthly.Income", "Number_of_logins", "Id", "Offerid",
     "Offer.time_stamp", "Merchant")

numeric <- c("Number_of_logins", "Id", "Offerid")

categoric <- c("Userid", "Profile_creation_time", "Number.of.Bank.Accounts", "Monthly.Spend",
     "Monthly.Income", "Offer.time_stamp", "Merchant")

target  <- "Event"
risk    <- NULL
ident   <- NULL
ignore  <- NULL
weights <- NULL


library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(mint[train, c(input, risk, target)])
summary(mint[train, c(input, risk, target)])

# Remap variables. 

# Transform into a numeric.

  train[["TNM_Monthly.Spend"]] <- as.numeric(train[["Monthly.Spend"]])

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

#view the relations between response and predictors
 plot(Event~Offerid,data = train)
> plot(Event~.,data = train)
> plot(Event~Number_of_logins,Offerid, data = train)
Error in plot.xy(xy, type, ...) : invalid plot type
> plot(Event~Number_of_logins, data = train)
              
> plot(Event~Monthly.Spend,data = train)


# Correlations work for numeric variables only.

cor <- cor(train[train, numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

ord <- order(cor[1,])
cor <- cor[ord, ord]

# Display the actual correlations.

print(cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation mint.csv using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))


# Transform variables by rescaling. 

# The 'reshape' package provides the 'rescaler' function.

library(reshape, quietly=TRUE)

# Rescale Event.

train[["R01_Event"]] <- train[["Event"]]

# Rescale to [0,1].


range01 <- function(x){(x-min(x))/(max(x)-min(x))}
R01_Event = range(Event)

Event1 = range01(Event)



# Perform Test 

# Use the fBasics package for statistical tests.

library(fBasics, quietly=TRUE)

# Perform the test.

ks2Test(na.omit(train[train[["R01_Event"]] == "0", "Offerid"]), na.omit(train[train[["R01_Event"]] == "1", "Offerid"]))





#plot the response vs predictors

plot(Event~I(Number_of_logins^2))
plot(Event~Number_of_logins)
# Regression model 

# Build a Regression model.
fit = lm(formula = Event ~ Number_of_logins * Offerid*Number.of.Bank.Accounts, data = train)

# Confidence intervals for the model
confint(fit)


fit = lm(formula = Event ~ Number_of_logins * Offerid, data = train)



fit = glm(Event1~Number_of_logins,data = train, family=binomial(link="logit"))

Call:
lm(formula = Event ~ Number_of_logins * Offerid, data = train)



glm <- glm(R01_Event ~ .,
    data=crs$dataset[crs$train, c(crs$input, crs$target)],
    family=binomial(link="logit"))

# Generate a textual view of the Linear model.

print(summary(glm))
cat(sprintf("Log likelihood: %.3f (%d df)\n",
            logLik(glm)[1],
            attr(logLik(glm), "df")))
cat(sprintf("Null/Residual deviance difference: %.3f (%d df)\n",
            glm$null.deviance-crs$glm$deviance,
            glm$df.null-crs$glm$df.residual))
cat(sprintf("Chi-square p-value: %.8f\n",
            dchisq(glm$null.deviance-crs$glm$deviance,
                   glm$df.null-crs$glm$df.residual)))
cat(sprintf("Pseudo R-Square (optimistic): %.8f\n",
             cor(crs$glm$y, crs$glm$fitted.values)))
cat('\n==== ANOVA ====\n\n')
print(anova(crs$glm, test="Chisq"))
cat("\n")

# Time taken: 6.66 secs


# ... and predict data on validation data-set
prediction = predict(fit,validate, type = "response")


# ----- Predict submission dataset ---------------------------------------------

submissionData = data.frame(ID = validate$Userid, Event = prediction)
submissionFile <- paste0("glm", format(Sys.time(), "%Y-%m-%d-%H:%M:%S"), ".csv")


write.csv(submissionData, file="C:/Users/bnamatherdhala/Desktop/submissionFile", row.names=FALSE)










