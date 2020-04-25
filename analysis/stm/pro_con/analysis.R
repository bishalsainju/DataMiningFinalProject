library(stm)        # Package for sturctural topic modeling
library(igraph)     # Package for network analysis and visualisation
library(stmCorrViz) # Package for hierarchical correlation view of STMs
library(dplyr)
library(LDAvis)

# Loading Data
load('~/Desktop/R_js/data/stm-pro_con.RData')


#  Model Analysis
num_topic = 17
model = modelCon17
model_cov = modelCon17_cov
dataPx = conPx


# LDA viz
toLDAvis(model, dataPx$documents, R=30 )


#Summary
summary(model)
# plot(model, type="labels")
# labelTopics(model)
# plot(model, type="perspectives")
plot(model, type="summary")



# Topic Quality
topicQuality(model, dataPx$documents, xlab="Semantic Coherence", 
            ylab="Exclusivity", labels=1:num_topic)


# Topic Correlations
corr <- topicCorr(poliblogPrevFit, method="simple", cutoff=.2)
plot(corr, vlabels = c(1:num_topic))


# Topic Proportions
plot(model, type="summary")


# Covariate Effect
plot(model_cov, covariate = "Job_Status", topics = model_cov$topics,
     model = model, method="difference", xlab="Former ............ Current",
     main="Effect of Former vs Current(Negative Feedback)",
     cov.value1 = 1, cov.value2 = 0, labeltype = "custom", custom.labels = 1:num_topic)
summary(model_cov)



