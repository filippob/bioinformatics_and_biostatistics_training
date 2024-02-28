
# Load libraries
library("themis") ## for upsampling with ROSE
library("glmnet")
library("ggplot2")
library("tidyverse")
library("doParallel")
library("tidymodels")

##############
## PARAMETERS
##############
fname = "example/data.RData" ## input data
ncpus = 4
upsample_ratio = 1.25 ## default = 1
###################
###################

## Import datasets
writeLines(" - loading data")
load(fname)

## data cleaning
writeLines(" - data cleaning")
## using support variable "category" for upsampling
temp <- test %>%
  select(-batch) |>
  mutate(across(c(study, sex, country, treatment, diagnosis, therapy), as.factor),
         category=cut(score_intensity, breaks=c(-Inf, 2, Inf), labels=c("low","high")))

# Setup parallel backend to use n. processors
cl <- makeCluster(ncpus)
registerDoParallel(cl)

############################
## tidymodels con upsampling
############################
print("Using tidymodels")

## training / test split
writeLines(" - split data")
mtbsl1_split <- initial_split(temp, prop = 0.8, strata = category)
mtbsl1_train <- training(mtbsl1_split)
mtbsl1_test <- testing(mtbsl1_split)

### upsampling
writeLines(" - upsamling of data")
upsample_rec <- recipe(category ~ ., data = mtbsl1_train) %>%
  step_rose(category, skip = TRUE, over_ratio = upsample_ratio)

upsample_prep <- prep(upsample_rec)
print(upsample_prep)

up_train <- juice(upsample_prep)

##############################
## CV - FINE TUNING OF LAMBDA
##############################
mod_rec <-  recipe(score_intensity ~ ., data = up_train) %>%
  update_role(category, new_role = "dataset split variable") |>
  step_zv(all_numeric(), -all_outcomes()) %>%
  step_normalize(all_numeric(), -all_outcomes()) |>
  step_dummy(-all_outcomes(), -all_numeric(), -category) |>
  step_rm(category)

print(mod_rec)
pain_cv <- vfold_cv(mtbsl1_train, v=5, repeats = 5, strata = category)

tune_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

lambda_grid <- grid_regular(penalty(), mixture(), levels = 50, filter = penalty >= .000001)
print(lambda_grid)

wf1 <- workflow() %>%
  add_recipe(mod_rec) %>%
  add_model(tune_spec) 

reg_metrics <- metric_set(ccc,rmse,rsq)

lasso_grid <- tune_grid(
  wf1,
  resamples = pain_cv,
  metrics = reg_metrics,
  grid = lambda_grid
)

lasso_grid %>%
  collect_metrics()

lasso_grid %>%
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.5
  ) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme(legend.position = "none")

lasso_grid %>%
  collect_metrics() %>%
  ggplot(aes(mixture, mean, color = .metric)) +
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.5
  ) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme(legend.position = "none")

### FINAL MODEL
best_ccc <- lasso_grid %>%
  select_best("ccc")

print(best_ccc)

final_lasso <- finalize_workflow(
  wf1,
  best_ccc
)

print(final_lasso)

## MODEL EVALUATION
lr_res <- last_fit(
  final_lasso,
  metrics = reg_metrics,
  mtbsl1_split
) 

lr_res %>%
  collect_metrics()

lr_res %>% collect_predictions() |>
  summarise(r_xv = cor(.pred, score_intensity))

preds1 = lr_res %>% collect_predictions()

##########################
## step-by-step evaluation
##########################
mod_prep <- mod_rec %>%
  prep()

mtbsl1_testing <- mod_prep %>% bake(mtbsl1_test) ## preprocess test data
mtbsl1_training = mod_prep |> bake(up_train)

lambda = best_ccc$penalty
alpha = best_ccc$mixture

last_lasso_mod <- 
  linear_reg(penalty = lambda, mixture = alpha) |>
  set_engine("glmnet") %>% 
  set_mode("regression")

last_lasso_wf <- workflow() |>
  add_model(last_lasso_mod) |>
  add_formula(score_intensity ~ .)

final_lasso_fit <- fit(last_lasso_wf, data = mtbsl1_training) ## fit final model on the training set
print(final_lasso_fit)
final_lasso_fit$pre

## 3 make predictions
preds = final_lasso_fit %>%
  predict(new_data = mtbsl1_testing, type = "numeric") %>%
  bind_cols(mtbsl1_testing$score_intensity) |>
  rename(av_pain_intensity = `...2`)

cor(preds$.pred, preds$score_intensity)
sqrt(sum((preds$score_intensity-preds$.pred)^2)/nrow(preds))

ggplot(data = preds, aes(.pred, score_intensity)) + geom_point()

print("DONE!")
