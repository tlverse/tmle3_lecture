library(sl3)
library(tmle3)

# setup data for test
data(cpp_imputed)
data <- as.data.table(cpp_imputed)

# generate binary treatment and outcome
data$haz01 <- as.numeric(data$parity > 0)
data$parity01 <- as.numeric(data$haz > 0)
data[is.na(data)] <- 0

# define variable roles
node_list <- list(
  W = c(
    "apgar1", "apgar5", "gagebrth", "mage",
    "meducyrs", "sexn"
  ),
  A = "parity01",
  Y = "haz01"
)


# define regression tasks
Q_task <- make_sl3_Task(data, covariates=c(node_list$W, node_list$A), outcome=node_list$Y)
g_task <- make_sl3_Task(data, covariates=node_list$W, outcome=node_list$A)

# simple fits for Q and g
lrnr_glm <- make_learner(Lrnr_glm)
Q_fit <- lrnr_glm$train(Q_task)
g_fit <- lrnr_glm$train(g_task)

#create counterfactual data and make task
cf_data <- copy(data)
set(cf_data, , node_list$A, 1)
cf_Q_task <- make_sl3_Task(cf_data, covariates=c(node_list$W, node_list$A), outcome=node_list$Y)

# get relevant likelihood factor values

#P(A=1|W)
g1W <- g_fit$predict(g_task)

#E(Y|A=a,W)
QAW <- Q_fit$predict(Q_task)

#E(Y|A=1,W)
Q1W <- Q_fit$predict(cf_Q_task)

