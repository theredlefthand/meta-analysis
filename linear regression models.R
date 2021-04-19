# ==============================================================================
# R-Code - simulation of data
# date of creation: March 2020
# authors: Julius Fenn
# ==============================================================================
rm(list=ls()); dev.off()

############################################################################
# load packages
############################################################################
# wenn Pakete nicht bereits installiert sind, wird die Funktion diese installieren und aktivieren
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
## Fehlerbehandlung Pakete in R base installieren
# options(repos="https://CRAN.R-project.org")

usePackage("MASS")
usePackage("Rcmdr")


# usePackage("performance") # assessment of Regression Models Performance -> ICC
# usePackage("stargazer") # Tabellen erstellen
# usePackage("ggplot2")
# usePackage("lattice")
# usePackage("dplyr") # pipe operators
# rm(usePackage)


############################################################################
# package: article sthda, different packages
# http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/
############################################################################

##################
# by MASS package
##################
### linear regression model
# Fit the full model
rmna_three <- na.omit(three)

full.model <- lm(rmna_three$mean_valence ~., data = rmna_three)
summary(full.model)

# Stepwise regression model
help(stepwise)
step.model <- stepAIC(object = full.model, direction = "both",
                      trace = TRUE)
summary(step.model)


### GLM
example(birthwt)
three.glm <- glm(num_nodes ~ ., family = gaussian, data = rmna_three)
three.step <- stepAIC(object = three.glm, direction = "both",
                        trace = TRUE)
three.step$anova
summary(three.step)

##################
# leaps package
##################
usePackage("leaps")
models <- regsubsets(num_nodes ~ ., data = rmna_three, nvmax = 5,
                     method = "seqrep")
summary(models)


##################
# caret package
##################
usePackage("caret")

# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model.k <- train(num_nodes ~., data = rmna_three,
                    method = "leapBackward",
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)
step.model.k$results
step.model.k$bestTune
summary(step.model.k$finalModel)
coef(step.model.k$finalModel, 4)
