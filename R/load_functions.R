class_seq <- function(data) {
  
  data <- data %>% 
    group_by(psc_cat) %>%
    mutate(class = as.numeric(factor(psc)))
  
  data <- data %>% 
    subset(!(psc_cat == "74"|psc_cat == "L"|psc_cat == "K"|psc_cat == "S"))
  return(data)
  
}

normalize_metrics <- function(values) {
  # normalize to [0,1]
  columns <- base::subset(values, select = 2:ncol(values))
  values <- base::data.frame(
    values["topics"],
    base::apply(columns, 2, function(column) {
      scales::rescale(column, to = c(0, 1), from = range(column))
    })
  )
  return(values) }


optimal_topics <- function(dtm){
result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 30, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009","Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1234),
  mc.cores = 4L, #make sure this is appropriate number of cores you wish to use
  verbose = TRUE
)

return(result)
}

tidyDTM <- function(text.df, sparse){
  sw <- add_row(stop_words, word = c("igf","ot", "ct"), lexicon = c("SMART", "SMART", "SMART"))
  
  #word counts
  word_counts <- text.df %>%
    unnest_tokens(word, description) %>%
    anti_join(sw) %>%
    count(document, word, sort = TRUE) %>%
    ungroup()
  
  #cast dtm
  dtm <- word_counts %>%
    cast_dtm(document, word, n)
  
  dtm$dimnames$Terms <- gsub("function", "functio", dtm$dimnames$Terms)
  
  dtmNoSparse <- removeSparseTerms(dtm, sparse)
  
  return(dtmNoSparse)
}

explDTM <- function(text.df) {
  
  sw <- add_row(stop_words, word = c("igf","ot", "ct"), lexicon = c("SMART", "SMART", "SMART"))
  
  #word counts
  word_counts <- text.df %>%
    unite(document, psc, document) %>% 
    unnest_tokens(word, description) %>%
    anti_join(sw) %>%
    count(document, word, sort = TRUE) %>%
    ungroup()
  
  #cast dtm
  dtm <- word_counts %>%
    cast_dtm(document, word, n)
  
  dtm$dimnames$Terms <- gsub("function", "functio", dtm$dimnames$Terms)
  
  return(dtm)
  
}

tidyDTMidf <- function(text.df, sparse){
  sw <- add_row(stop_words, word = c("igf","ot", "ct"), lexicon = c("SMART", "SMART", "SMART"))
  
  #word counts
  word_counts <- text.df %>%
    unnest_tokens(word, description) %>%
    anti_join(sw) %>%
    count(document, word, sort = TRUE) %>%
    ungroup()
  
  word_counts <- word_counts %>% 
    bind_tf_idf(document, word, n)
  
  #cast dtm
  dtm <- word_counts %>%
    cast_dtm(document, word, tf_idf)
  
  dtm$dimnames$Terms <- gsub("function", "functio", dtm$dimnames$Terms)
  
  dtmNoSparse <- removeSparseTerms(dtm, sparse)
  
  return(dtmNoSparse)
}

removeDuplicates <- function(dtm, data){
  #remove rows not in dtm
  duplicates <- data[duplicated(data$document),]
  data <- data[!(duplicated(data$document)),]
  data <- data[(data$document %in% dtm$dimnames$Docs),]
  return(data)
}


dataMatrix <- function(dtm, data) {
  dtm.mat <- as.data.frame(as.matrix(dtm))
  dtm.mat$targetCat <- as.factor(data$class[match(rownames(dtm.mat), data$document)])
  #dtm.mat$targetCat <- as.factor(data$class)
  return(dtm.mat)
}

trainSplit <- function(dtm.mat) {
  p=0.7
  #holdout
  train.idx <- sample(nrow(dtm.mat),ceiling(nrow(dtm.mat) * p))
  return(train.idx)}
testSplit <- function(dtm.mat, train.idx){
  test.idx <- (1:nrow(dtm.mat))[-train.idx]
  return(test.idx)
}
dtmCat <- function(dtm.mat){
  #targets
  dtm.cat <- dtm.mat[,"targetCat"]
  return(dtm.cat)
}

dtmMatNl <- function(dtm.mat){
  dtm.mat.nl <- dtm.mat[, !colnames(dtm.mat) %in% "targetCat"]
  return(dtm.mat.nl)
}


doKNNCV <- function(dtm.mat.nl, dtm.cat, train.idx, test.idx){
  t1 <- Sys.time()
  knn.cross <- tune.knn(x = dtm.mat.nl, y = dtm.cat, k = 1:20, l = 0, tunecontrol=tune.control(sampling = "cross"), cross=10)
  k <- as.numeric(knn.cross$best.parameters[1,])
  pred.model <- knn(dtm.mat.nl[train.idx,],dtm.mat.nl[test.idx,],dtm.cat[train.idx], k = k, use.all = TRUE)

  Test_Obs <- dtm.cat[test.idx]
  Predicted <- pred.model
  
  conf <- table(Predicted, Test_Obs)
  
  f.conf <- confusionMatrix(conf)
  #stats <- f.conf$overall
  time <- Sys.time() - t1
  stats <- list(f.conf$overall, knn.cross, f.conf, pred.model, time)
  return(stats)
}


doKKNN <- function(dtm.mat,train.idx, test.idx){
  t1 <- Sys.time()
  train.kknn <- train.kknn(targetCat~., dtm.mat, kmax = 25, kernel = c("rectangular", "triangular", "epanechnikov", "gaussian", "rank", "optimal"))
  k <- as.numeric(train.kknn$best.parameters$k)
  kernel <- train.kknn$best.parameters$kernel
  pred.model <- kknn(targetCat~., dtm.mat[train.idx,], dtm.mat[test.idx,], k = k, kernel = kernel)
  
  Test_Obs <- dtm.mat$targetCat[test.idx]
  Predicted <- pred.model$fitted.values
  
  conf <- table(Predicted, Test_Obs)
  
  f.conf <- confusionMatrix(conf)
  #stats <- f.conf$overall
  time <- Sys.time() - t1
  stats <- list(f.conf$overall, pred.model, f.conf, k, kernel, time)
  return(stats)
}

doKKNNprev <- function(dtm.mat,dtm.cat, train.idx, test.idx, k){
  knn.pred <- kknn(targetCat~., dtm.mat[train.idx,], dtm.mat[test.idx, ], k = k)
  conf.mat <- table("Predictions" = knn.pred$fitted.values,Actual = dtm.cat[test.idx])
  accuracy <- sum(diag(conf.mat)/length(test.idx) *100)
  return(accuracy)
}


doKNN <- function (dtm.mat, dtm.mat.nl, train.idx, test.idx, dtm.cat, k) {
  knn.pred <- knn(dtm.mat.nl[train.idx,],dtm.mat.nl[test.idx,],dtm.cat[train.idx], k = k, use.all = TRUE)
  #accuracy
  conf.mat <- table("predictions" = knn.pred,Actual = dtm.cat[test.idx])
  accuracy <- sum(diag(conf.mat)/length(test.idx) *100)
  return(accuracy)
}

vectKNN <- function(dtm.mat, dtm.mat.nl, train.idx, test.idx, dtm.cat){
iterations = 5
variables = 3
Acc_K <- matrix(ncol=variables, nrow=iterations)
for(k in 1:iterations){
  t1 <- Sys.time()
  Acc_K[k,2] <- k
  Acc_K[k,1] <- doKNN(dtm.mat, dtm.mat.nl, train.idx, test.idx, dtm.cat, k)
  diff <- Sys.time() -t1
  Acc_K[k,3] <- diff
  
}
KNN.time <- mean(Acc_K[,3])
Acc_K <- as.data.frame(Acc_K)
colnames(Acc_K) <- c("Accuracy", "K", "Time")
return(Acc_K)
}

doRFtune <- function(dtm.mat.nl, dtm.cat) {
  tune.rf <- tuneRF(dtm.mat.nl,dtm.cat, doBest = TRUE, trace = FALSE, plot = FALSE)
  return(tune.rf$mtry)
}

doRF <- function (dtm.mat, train.idx, test.idx, n, m) {
  t1 <- Sys.time()
  
  model <- randomForest(targetCat~.,data = dtm.mat, subset = train.idx, ntree = n, mtry = m, importance = TRUE)
  pred.model <- predict(model, dtm.mat[test.idx,])
  
  Test_Obs <- dtm.mat[test.idx,]$targetCat
  Predicted <- pred.model
  
  conf <- table(Predicted, Test_Obs)
  
  f.conf <- confusionMatrix(conf)
  #stats <- f.conf$overall
  time <- Sys.time() - t1
  stats <- list(f.conf$overall, model, f.conf, pred.model, time)
  return(stats)
}

AccStats <- function(model, dtm.mat, test.idx){
  pred.model <- predict(model, dtm.mat[test.idx,])
  
  Test_Obs <- dtm.mat[test.idx,]$targetCat
  Predicted <- pred.model
  
  conf <- table(Predicted, Test_Obs)
  
  f.conf <- confusionMatrix(conf)
  
  return(f.conf$overall)
}

doRFerr <- function (dtm, dtm.mat, train.idx, n) {
  tree.fit <- randomForest(targetCat~.,data = dtm.mat, subset = train.idx, ntree = n, importance = TRUE)
  error <- as.data.frame(tree.fit$err.rate)
  return(mean(error))
}

doSVM <- function (dtm.mat, cost, gamma, kernel) {
  svm.fit <- svm(targetCat~.,dtm.mat,kernel = kernel, cost = 10, gamma = 1)
  return(svm.fit)
}

doSVMerr <- function(dtm.mat, test.idx){
  t1 <- Sys.time()
  tune.out <- tune(svm, targetCat~., data = dtm.mat, kernel = "linear",
                  ranges = list(cost = c(0.0001, 0.001, 0.01, 0.1, 1, 10),
                                scale = FALSE))
  model <- tune.out$best.model
  #SVMerror <- tune.out$best.performance
  #return(SVMerror)
  pred.model <- predict(model, dtm.mat[test.idx,])
  
  Test_Obs <- dtm.mat[test.idx,]$targetCat
  Predicted <- pred.model
  
  conf <- table(Predicted, Test_Obs)
  
  f.conf <- confusionMatrix(conf)
  time <- Sys.time() - t1
  stats <- list(f.conf$overall, model, f.conf, pred.model, time)
  return(stats)
}


rfplot.error <-function(randomForest.fit) {
  # Get OOB data from plot and coerce to data.table
  oobData <- as.data.table(randomForest.fit$err.rate)
  
  # Define trees as 1:ntree
  oobData[, trees := .I]
  
  # Cast to long format
  oobData2 <- melt(oobData, id.vars = "trees")
  setnames(oobData2, "value", "error")
  
  # Plot using ggplot
  plt <- ggplot(data = oobData2, aes(x = trees, y = error, color = variable)) + geom_line() 
  
  return(plt)
}

rfplot.importance <- function(randomForest.fit){
  data_frame(var = rownames(importance(randomForest.fit)),
             MeanDecreaseGini = importance(randomForest.fit)[,1]) %>% 
    top_n(20, MeanDecreaseGini) %>% 
    mutate(var = fct_reorder(var,MeanDecreaseGini, fun = median)) %>% 
    ggplot(aes(var, MeanDecreaseGini)) +
    geom_point() +
    coord_flip() +
    labs(title = "Gini Importance of Variable Terms",
         subtitle = "Bagging",
         x= NULL,
         y = "Average decrease in the Gini Index")
}

ldafun <- function(dtm, k) {
  lda <- LDA(dtm, k, control = list(seed = 1234))
  return(lda)
}

topic_terms <- function(topics.beta) {
  top_terms <- topics.beta %>% 
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  plt <- top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
  return(plt)
  }

tidybeta <- function(lda){
  
  #extract topic betas
  topics <- tidy(lda, matrix = "beta")
  
  return(topics)
}

tidygamma <- function(lda){
  
  #extract topic betas
  topics <- tidy(lda, matrix = "gamma") %>% 
    separate(document, c("psc_cat", "psc"), sep = "_", convert = TRUE)
  
  return(topics)
}

gammaPlots <- function(gamma) {
  plt <- gamma %>%
    mutate(psc_cat = reorder(psc_cat, gamma * topic)) %>%
    ggplot(aes(factor(topic), gamma)) +
    geom_boxplot() +
    facet_wrap(~ psc_cat)
  
  return(plt)
}


LDAclassify <- function(gamma){
  
  #classification
  
  classifications <- gamma %>%
    group_by(psc_cat, psc) %>%
    top_n(1,gamma) %>%
    ungroup()
  return(classifications)
} 

LDAtopics <- function(classifications) {
  topics <- classifications %>%
    count(psc_cat, topic) %>% 
    group_by(psc_cat) %>% 
    top_n(1,n) %>% 
    ungroup() %>% 
    transmute(consensus = psc_cat, topic)
  return(topics)
}

misclass <- function(classifcations) {
  
  class <- classifications %>%
    inner_join(topic, by = "topic") %>%
    filter(psc_cat != consensus)
  return(class)
}


LDAconfusion <- function(lda, dtm) {
  #confusion matrix
  
  assignments <- augment(lda, data = dtm)
  
  assignments <- assignments %>%
    separate(document, c("psc_cat", "psc"), sep = "_", convert = TRUE) %>%
    inner_join(topics, by = c(".topic" = "topic"))
  
  plt <- assignments %>%
    count(psc_cat, consensus, wt = count) %>%
    group_by(psc_cat) %>%
    mutate(percent = n / sum(n)) %>%
    ggplot(aes(consensus, psc_cat, fill = percent)) +
    geom_tile() +
    scale_fill_gradient2(high = "red", label = percent_format()) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid = element_blank()) +
    labs(x = "psc_cat words were assigned to",
         y = "psc_cat words came from",
         fill = "% of assignments")
  return(plt)
}