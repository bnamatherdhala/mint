# Load the data.

crs$dataset <- read.csv("file:///C:/Users/bharatwaja/Desktop/mint.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

# Build the training/validate/test datasets.

set.seed(crv$seed) 
# A pre-defined value is used to reset the random seed so that results are repeatable.

crv$seed <- 42 
crs$nobs <- nrow(crs$dataset) # 71272 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 49890 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 10690 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 10692 observations


# The following variable selections have been noted.

crs$input <- c("Userid", "Profile_creation_time", "Number.of.Bank.Accounts", "Monthly.Spend",
     "Monthly.Income", "Number_of_logins", "Id", "Offerid",
     "Offer.time_stamp", "Merchant")

crs$numeric <- c("Number_of_logins", "Id", "Offerid")

crs$categoric <- c("Userid", "Profile_creation_time", "Number.of.Bank.Accounts", "Monthly.Spend",
     "Monthly.Income", "Offer.time_stamp", "Merchant")

crs$target  <- "Event"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL


library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)])

# Remap variables. 

# Transform into a numeric.

  crs$dataset[["TNM_Monthly.Spend"]] <- as.numeric(crs$dataset[["Monthly.Spend"]])

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$sample, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation mint.csv using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))


# Transform variables by rescaling. 

# The 'reshape' package provides the 'rescaler' function.

library(reshape, quietly=TRUE)

# Rescale Event.

crs$dataset[["R01_Event"]] <- crs$dataset[["Event"]]

# Rescale to [0,1].

if (building)
{
  crs$dataset[["R01_Event"]] <-  rescaler(crs$dataset[["Event"]], "range")
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R01_Event"]] <- (crs$dataset[["Event"]] - 1.000000)/abs(2.000000 - 1.000000)
}

# Perform Test 

# Use the fBasics package for statistical tests.

library(fBasics, quietly=TRUE)

# Perform the test.

ks2Test(na.omit(crs$dataset[crs$dataset[["R01_Event"]] == "0", "Offerid"]), na.omit(crs$dataset[crs$dataset[["R01_Event"]] == "1", "Offerid"]))


# The following variable selections have been noted.

crs$input <- c("Number_of_logins", "Offerid", "Merchant", "TNM_Monthly.Income",
     "TNM_Monthly.Spend", "TNM_Number.of.Bank.Accounts", "TNM_Offer.time_stamp")

crs$numeric <- c("Number_of_logins", "Offerid", "TNM_Monthly.Income", "TNM_Monthly.Spend",
     "TNM_Number.of.Bank.Accounts", "TNM_Offer.time_stamp")

crs$categoric <- "Merchant"

crs$target  <- "R01_Event"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("Userid", "Profile_creation_time", "Number.of.Bank.Accounts", "Id", "Offer.time_stamp", "Event")
crs$weights <- NULL


# Regression model 

# Build a Regression model.

crs$glm <- glm(R01_Event ~ .,
    data=crs$dataset[crs$train, c(crs$input, crs$target)],
    family=binomial(link="logit"))

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat(sprintf("Log likelihood: %.3f (%d df)\n",
            logLik(crs$glm)[1],
            attr(logLik(crs$glm), "df")))
cat(sprintf("Null/Residual deviance difference: %.3f (%d df)\n",
            crs$glm$null.deviance-crs$glm$deviance,
            crs$glm$df.null-crs$glm$df.residual))
cat(sprintf("Chi-square p-value: %.8f\n",
            dchisq(crs$glm$null.deviance-crs$glm$deviance,
                   crs$glm$df.null-crs$glm$df.residual)))
cat(sprintf("Pseudo R-Square (optimistic): %.8f\n",
             cor(crs$glm$y, crs$glm$fitted.values)))
cat('\n==== ANOVA ====\n\n')
print(anova(crs$glm, test="Chisq"))
cat("\n")

# Time taken: 6.66 secs







