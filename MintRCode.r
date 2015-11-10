# Load the data.
crs$dataset <- read.csv("file:///C:/Users/bharatwaja/Desktop/mint.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
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

crs$input <- c("Number.of.Bank.Accounts", "Monthly.Spend", "Monthly.Income", "Number_of_logins",
     "Id", "Offerid", "Merchant", "Offer.time_Stamp",
     "time_Stamp")

crs$numeric <- c("Number.of.Bank.Accounts", "Monthly.Spend", "Monthly.Income", "Number_of_logins",
     "Id", "Offerid", "Offer.time_Stamp")

crs$categoric <- c("Merchant", "time_Stamp")

crs$target  <- "R01_Event"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("Userid", "Profile_creation_time", "Offer.time_stamp", "Event")
crs$weights <- NULL

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
    sub=paste("Bharat", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))


# Transform variables by rescaling. 

# The 'reshape' package provides the 'rescaler' function.

library(reshape, quietly=TRUE)

# Rescale Event.

train[["R01_Event"]] <- train[["Event"]]

# Rescale to [0,1].


range01 <- function(x){(x-min(x))/(max(x)-min(x))}
R01_Event = range(Event)

Event1 = range01(Event)

##resclae currency
convertCurrency <- function(currency) {
  +     currency1 <- sub('$','',as.character(currency),fixed=TRUE)
  +     currency2 <- as.numeric(gsub('\\,','',as.character(currency1))) 
  +     currency2
  + }
train2$Monthly.Income = convertCurrency(train2$Monthly.Spend)

train2$Monthly.Spend = convertCurrency(train2$Monthly.Spend)

## Bar Plot 

# Generate the summary data for plotting.

ds <- rbind(summary(na.omit(crs$dataset[crs$sample,]$time_Stamp)),
    summary(na.omit(crs$dataset[crs$sample,][crs$dataset[crs$sample,]$R01_Event=="0",]$time_Stamp)),
    summary(na.omit(crs$dataset[crs$sample,][crs$dataset[crs$sample,]$R01_Event=="1",]$time_Stamp)))

# Sort the entries.

ord <- order(ds[1,], decreasing=TRUE)

# Plot the data.

bp <-  barplot2(ds[,ord], beside=TRUE, ylab="Frequency", xlab="time_Stamp", ylim=c(0, 18613), col=colorspace::rainbow_hcl(3))

# Add the actual frequencies.

text(bp, ds[,ord]+620, ds[,ord])

# Add a legend to the plot.

legend("topright", bty="n", c("All","0","1"),  fill=colorspace::rainbow_hcl(3))

# Add a title to the plot.

title(main="Distribution of time_Stamp (sample)\nby R01_Event",
    sub=paste("Bharat", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# Perform Test 

# Use the fBasics package for statistical tests.

library(fBasics, quietly=TRUE)

# Perform the test.

ks2Test(na.omit(train[train[["R01_Event"]] == "0", "Offerid"]), na.omit(train[train[["R01_Event"]] == "1", "Offerid"]))


#plot the response vs predictors

plot(Event~I(Number_of_logins^2))
plot(Event~Number_of_logins)
# Regression model 

# Build a  logisticRegression model.

# Confidence intervals for the model
confint(fit)





fit = glm(Event1~Number_of_logins,data = train, family=binomial(link="logit"))




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
-------------------------------------------------------------
  library(pROC)
> g = roc(Event~prob, data = train2)
> plot(g)

Call:
  roc.formula(formula = Event ~ prob, data = train2)

Data: prob in 37392 controls (Event 0) < 2608 cases (Event 1).
Area under the curve: 0.6464
--------------------------------------------------------------------------
# ... and predict data on validation data-set
prediction = predict(fit,validate, type = "response")

#...ROC plots
 prob = predict(fit5,type = c("response"))
 g = roc(Event~prob, data = validate)
 plot(g)
 
 
 
 # Evaluate model performance. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the glm model on mint2.csv [validate].

crs$pr <- predict(crs$glm, type="response", newdata=crs$dataset[crs$validate, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$R01_Event)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL


pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Linear mint2.csv [validate] R01_Event")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# ----- Predict validate dataset ---------------------------------------------
# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$R01_Event)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL
pred <- prediction(crs$pr[-miss.list], no.miss)
performance(pred, "auc")


submissionData = data.frame(ID = validate$Userid, Event = prediction)
submissionFile <- paste0("glm", format(Sys.time(), "%Y-%m-%d-%H:%M:%S"), ".csv")


write.csv(submissionData, file="C:/Users/bnamatherdhala/Desktop/submissionFile", row.names=FALSE)










