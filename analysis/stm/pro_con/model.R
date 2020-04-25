library(stm)        # Package for sturctural topic modeling
library(igraph)     # Package for network analysis and visualisation
library(stmCorrViz) # Package for hierarchical correlation view of STMs
library(dplyr)
library(LDAvis)
library(tidyverse)
library(tidytext)

# install.packages('stm')
# install.packages('igraph')
# install.packages('stmCorrViz')
# install.packages('dplyr')
# install.packages('LDAvis')
# install.packages('tidyverse')
# install.packages('tidytext')

### Data Reading
proFilePath = "~/Desktop/dm_project/data/prepx/pro_doc_sampled_6.csv"
conFilePath = "~/Desktop/dm_project/data/prepx/con_doc_sampled_6.csv"
proData <- read.csv(proFilePath) 
conData <- read.csv(conFilePath)


### Data Preprocessing
proProcessed <- textProcessor(proData$Px_Texts, metadata = proData, lowercase = FALSE,
                           removestopwords = FALSE, removenumbers = FALSE, removepunctuation = FALSE,
                           stem = FALSE, wordLengths = c(3, Inf), sparselevel = 1,
                           language = "en", verbose = TRUE, onlycharacter = FALSE,
                           striphtml = TRUE, customstopwords = NULL, v1 = FALSE)
conProcessed <- textProcessor(conData$Px_Texts, metadata = conData, lowercase = FALSE,
                              removestopwords = FALSE, removenumbers = FALSE, removepunctuation = FALSE,
                              stem = FALSE, wordLengths = c(3, Inf), sparselevel = 1,
                              language = "en", verbose = TRUE, onlycharacter = FALSE,
                              striphtml = TRUE, customstopwords = NULL, v1 = FALSE)

proPx <- prepDocuments(proProcessed$documents, proProcessed$vocab, proProcessed$meta)
conPx <- prepDocuments(conProcessed$documents, conProcessed$vocab, conProcessed$meta)

# proPx["docs.removed"]
# conPx["docs.removed"]

#### Training STM model

# Diagnosis

# sk_4_20 <- searchK(conPx$documents, conPx$vocab, K=c(4:20), 
#                    prevalence =~ Job_Status, data=conPx$meta, 
#                    max.em.its = 100)
# 
# sk_4_20_pro <- searchK(proPx$documents, proPx$vocab, K=c(4:20), 
#                    prevalence =~ Job_Status, data=proPx$meta, 
#                    max.em.its = 100)
# plot(sk_4_20)
# summary(sk)

# print(length(sk_all[["results"]][["lbound"]]))
# df_sk_4_20_pro <- data.frame("num_topics" = sk_4_20_pro[["results"]][["K"]], 
#                          "exclus" = sk_4_20_pro[["results"]][["exclus"]], 
#                          "semcoh" = sk_4_20_pro[["results"]][["semcoh"]], 
#                          "heldout" = sk_4_20_pro[["results"]][["heldout"]],
#                          "residual" = sk_4_20_pro[["results"]][["residual"]],
#                          "bound" = sk_4_20_pro[["results"]][["bound"]],
#                          "lbound" = sk_4_20_pro[["results"]][["lbound"]]
# ) 
# write.csv(df_sk_4_20_pro,"~/Desktop/R_js/data1/diagnosis/pro_diag_4_20_gt3.csv")


##Pros
num_topic = 20
modelPro20 <- stm(proPx$documents, proPx$vocab, K=num_topic, prevalence=~Job_Status, 
                  data=proPx$meta, init.type="Spectral", 
                  seed=42)

modelPro20_cov <- estimateEffect(formula = 1:num_topic ~Job_Status, 
                                 stmobj = modelPro20, metadata = proPx$meta, 
                                 uncertainty = "Global")


#Cons
num_topic = 20
modelCon20 <- stm(conPx$documents, conPx$vocab, K=num_topic, prevalence=~Job_Status, 
                  data=conPx$meta, init.type="Spectral", 
                  seed=42)

modelCon20_cov <- estimateEffect(formula = 1:num_topic ~ Job_Status, 
                                 stmobj = modelCon20, metadata = conPx$meta, 
                                 uncertainty = "Global")


### Saving the model
save.image('~/Desktop/R_js/data/stm-pro_con.RData')


### Load the model
load('~/Desktop/R_js/data/stm-pro_con.RData') 


# Model Analysis
model = modelCon20

beta <- tidy(model) #prob that each word is generated from the topic
write_csv(beta, path="~/Desktop/R_js/data1/beta/con20_beta.csv")

gamma <- tidy(model, matrix='gamma')
write_csv(gamma, path="~/Desktop/R_js/data1/gamma/con20_gamma.csv")



