library(data.table)
library(tidyverse)
library(Hmisc)

prop <- fread('~/properties_2016.csv')
train <- fread('~/train_2016_v2.csv')

##################################################

dontKeep <- c("architecturalstyletypeid", "buildingclasstypeid", "threequarterbathnbr", 
              "finishedfloor1squarefeet", "finishedsquarefeet6", "finishedsquarefeet13",
              "finishedsquarefeet15", "finishedsquarefeet50", "fireplaceflag",
              "rawcensustractandblock", "storytypeid",
              "typeconstructiontypeid", "propertyzoningdesc"
              )

impMode <- c("bathroomcnt", "bedroomcnt", "calculatedbathnbr", "fullbathcnt"
             )

impZero <- c("basementsqft", "fireplacecnt", "garagecarcnt",
             "garagetotalsqft", "poolcnt", "pooltypeid10",
             "pooltypeid2", "pooltypeid7", "poolsizesum",
             "yardbuildingsqft17", "yardbuildingsqft26"
             )

impRand <- c("airconditioningtypeid", "heatingorsystemtypeid", "numberofstories",
             "buildingqualitytypeid")

prop$taxdelinquencyyear <- ifelse(prop$taxdelinquencyyear > 15, prop$taxdelinquencyyear + 1900, 
                                                                prop$taxdelinquencyyear + 2000)

prop$decktypeid <- as.factor(ifelse(is.na(prop$decktypeid) == TRUE, 0, 1))

prop$hashottuborspa <- as.factor(ifelse(prop$hashottuborspa == "", 0, 1))

prop$propertycountylandusecode <- as.factor(prop$propertycountylandusecode)

prop$propertylandusetypeid <- as.factor(prop$propertylandusetypeid)

prop$taxdelinquencyflag <- as.factor(prop$taxdelinquencyflag)

##################################################

imp_zero <- function(x) {
  ifelse(is.na(x) == TRUE, 0, x)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

imp_mode <- function(x) {
  ifelse(is.na(x) == TRUE, Mode(x[!is.na(x)]), x)
}

#prop <- prop %>% select(-one_of(dontKeep))

prop <- prop %>% mutate_at(.vars = vars(impZero), .funs = imp_zero)

prop <- prop %>% mutate_at(.vars = vars(impMode), .funs = imp_mode)

prop <- prop %>% mutate_at(.vars = vars(impRand), .funs = function(x)impute(x, "random"))



prop <- prop %>% mutate(bathbed = bathroomcnt/bedroomcnt,
                        rmsfratio = (bathroomcnt + bedroomcnt)/calculatedfinishedsquarefeet,
                        sqratio = calculatedfinishedsquarefeet/lotsizesquarefeet,
                        taxsfratio = taxvaluedollarcnt / calculatedfinishedsquarefeet,
                        gar = (garagetotalsqft+lotsizesquarefeet)/calculatedfinishedsquarefeet
)

test <- inner_join(prop, train, by = 'parcelid')

test$transactiondate <- NULL

gbmModel <- gbm(logerror ~ .-(parcelid), distribution="gaussian", interaction.depth=8,
                n.cores=7, n.trees = 1000, cv.folds = 2,
                shrinkage = 0.001, data = test[tr,])

mean(gbmModel$cv.error)

summary(gbmModel)

gbmTrainPredictions <- predict.gbm(object=gbmModel, newdata=test[-tr,], n.trees=1000, type="response")

mean((gbmTrainPredictions-test[-tr,]$logerror)^2)


model <- gbm(logerror ~ ., distribution="gaussian", interaction.depth=8,
             n.cores=7, n.trees = 1000, cv.folds = 2,
             shrinkage = 0.001, data = test)

pred <- predict.gbm(object = model, newdata = prop, ntrees = 1000, type = 'response')

submission <- read.csv('~/sample_submission.csv')
colnames(submission) <- c( "ParcelId","201610" , "201611" , "201612" , "201710" , "201711" , "201712" )
submission[2:7] <- pred

fwrite(submission, "submission.csv")
