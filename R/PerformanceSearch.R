#' Perform cross-validation for a given model and given dataset
#'
#' This function performs cross validation for a given model and a given dataset and returns
#' the AUC (or chosen metric - not supported yet) and ROC curve for the best model found on a test set.
#'
#' A seed is set internally for reproducibility across runs. The data is split into training/testing set
#' and then the training set is split into folds. The same folds are created at each run ensuring model
#' performance is comparable across models and not affected by random chance.
#'
#' @param model Choice of model to be trained, current supported options are: "xgboost", "svm" and "glm"
#' @param data Data to be used for model training. Must be passed whole (not training/testing) as the splits
#' happens internally
#' @param outcome Response variable of choice. Must be one of the columns in data.
#' @param kfolds Number of folds for k-fold cross validation. (default = 5)
#' @param train.proportion Proportion of data to be kept for training (default = 0.8)
#'
#' @return A plot of the ROC curve from test data and the corresponding AUC value
#'
#' @examples
#' data(iris)
#' iris = iris %>% dplyr::filter(Species!="virginica") %>%
#'   dplyr::mutate(Species = as.numeric(Species)-1)
#' Analysis("glm",iris, outcome = "Species")
#'
#' @export
Analysis = function(model, data, outcome = 'CVD_status', kfolds = 5, train.proportion = 0.8){

  if (!outcome %in% colnames(data)){
    stop(paste("Column", outcome, "not available in the dataset provided"))
  }

  #reproducibility
  set.seed(1247)
  tr.index = sample(1:nrow(data), size = train.proportion*nrow(data))
  X = dplyr::select(data, -outcome)
  y = dplyr::select(data, outcome)

  y.train = y[tr.index,1]
  X.train = X[tr.index,]

  y.test = y[-tr.index,1]
  X.test = X[-tr.index,]

  ## set up for CV
  folds <- cut(seq(1,nrow(X.train)),breaks=kfolds,labels=FALSE)

  if (model == "xgboost"){
    X.train = data.matrix(X.train)
    X.test = data.matrix(X.test)
    y.train = as.numeric(y.train)
    y.test = as.numeric(y.test)
    train = xgboost::xgb.DMatrix(data = X.train, label= y.train)
    test <- xgboost::xgb.DMatrix(data = X.test, label = y.test)

    xgb.folds = list()
    for (fold.id in 1:kfolds){
      xgb.folds[[length(xgb.folds)+1]] = which(folds==fold.id)
    }

    # nrounds is basically number of trees in forest
    # this is basically a grid search
    best_auc = 0
    best_eta = 0
    best_depth = 0
    best_nround = 0
    for (nround in c(5,10,20)){
      #Apparently one should only tweak nrounds realistically or maybe yes
      # https://www.kaggle.com/c/otto-group-product-classification-challenge/discussion/13053
      # https://stackoverflow.com/questions/35050846/xgboost-in-r-how-does-xgb-cv-pass-the-optimal-parameters-into-xgb-train
      # many other things to optimise: https://rdrr.io/cran/xgboost/man/xgb.train.html
      for (max_depth in c(3,5,10,15)){
        for (eta in c(0.1, 0.5, 1)){

          # if the nfold is too tiny this function may give an error because the dataset
          # fed into the model only contains negative samples aka (controls)

          model.cv = xgboost::xgb.cv(data = train, folds = xgb.folds, nrounds = nround, objective = "binary:logistic",
                                     metrics = list("auc"), eta = eta, max_depth = max_depth, verbose = F)

          if (max(model.cv[["evaluation_log"]][["test_auc_mean"]]) > best_auc){
            best_eta = eta
            best_depth = max_depth
            best_nround = nround
            best_auc = max(model.cv[["evaluation_log"]][["test_auc_mean"]])
          }
        }
      }
    }

    best.model = xgboost::xgb.train(data = train, nrounds = best_nround, objective = "binary:logistic",
                                    eta = best_eta, max_depth = best_depth, eval_metric = "auc")
    prdct = stats::predict(best.model, newdata = test)
    pred.objct <- ROCR::prediction(prdct, xgboost::getinfo(test,'label'))
    auc = ROCR::performance(pred.objct, "auc")
    perf = ROCR::performance(pred.objct, "tpr", "fpr")
    ROCR::plot(perf, main = model)
    graphics::abline(a = 0, b = 1, lty = 2)
    return(auc@y.values[[1]])

  }

  else if (model == "svm"){
    best_auc = 0
    best_kernel = 0
    best_cost = 0
    iter = 1
    for (kernel in c("linear","radial")){
      for(cost in c(1,5,10,15)){
        ######### CV
        auc.list = c()
        print(paste("Iteration ", as.character(iter)))
        iter = iter + 1
        for (i in 1:5){
          valIndexes <- which(folds==i)
          ValData <- X.train[valIndexes, ]
          nonValData <- X.train[-valIndexes, ]

          ValOutcome = y.train[valIndexes]
          nonValOutcome = y.train[-valIndexes]


          # details for this syntax :
          #  https://stackoverflow.com/questions/9028662/predict-maybe-im-not-understanding-it
          md.svm = e1071::svm(nonValOutcome ~ . , data = data.frame(nonValOutcome, nonValData),
                              kernel = kernel, cost = cost)
          prdct = stats::predict(md.svm, newdata =  ValData)
          pred.objct = ROCR::prediction(prdct, ValOutcome)
          auc = ROCR::performance(pred.objct, "auc")
          auc.list = append(auc.list, auc@y.values[[1]])
        }
        mean.auc = mean(auc.list)
        if (mean.auc>best_auc){
          best_auc = mean.auc
          best_kernel = kernel
          best_cost = cost
        }
      }
    }
    print(typeof(best_auc))
    print(best_auc)
    bst.svm = e1071::svm(y.train~., data = data.frame(y.train, X.train),
                         kernel = best_kernel, cost = best_cost)
    cat(paste0("Best params from SVM training:\n\t kernel: ", best_kernel,
                "\n\t C-value: ", best_cost,
                "\n\t AUC: ", best_auc,"\n"))
    best.prdct = stats::predict(bst.svm, X.test)
    pred.objct = ROCR::prediction(best.prdct, y.test)
    auc = ROCR::performance(pred.objct, "auc")
    performance = ROCR::performance(pred.objct, "tpr", "fpr")
    ROCR::plot(performance)
    graphics::abline(a = 0, b = 1, lty = 2)
    graphics::title(model)
    return(auc@y.values[[1]])
  }

  else if (model == "glm"){
    best_auc = 0
    best_kernel = 0
    best_cost = 0

    X.train = data.matrix(X.train)
    y.train = as.numeric(y.train)

    X.test = data.matrix(X.test)
    y.test = as.numeric(as.matrix(y.test))


    mod.cv = glmnet::cv.glmnet(X.train, y.train, foldid = folds,
                               type.measure = "auc", family = "binomial")
    cat(paste0("Best regularisation parameters for glm model (based on 1SE rule):\n",
               "\tlambda1SE: ",mod.cv$lambda.1se,"\n"))
    best.prdct = stats::predict(mod.cv, s = "lambda.1se", newx = X.test)
    pred.objct = ROCR::prediction(best.prdct, y.test)
    auc = ROCR::performance(pred.objct, "auc")
    performance = ROCR::performance(pred.objct, "tpr", "fpr")
    ROCR::plot(performance)
    graphics::abline(a = 0, b = 1, lty = 2)
    graphics::title(model)
    return(auc@y.values[[1]])
  }
  else {
    stop("Please input a valid value for model")
  }
}
