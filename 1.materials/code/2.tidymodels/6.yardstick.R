
# {yardstick}モデル評価 --------------------------------------------------------

# ０．回帰：モデル構築と予測値算出 --------------------------------------------------------

# 学習ルール
reg_rule =
  decision_tree() %>%
  set_engine('rpart') %>%
  set_mode('regression')

# レシピ
reg_rec =
  recipe(Price ~ ., data = train) %>% 
  step_impute_bag(all_predictors())

# ワークフロー
reg_wf = 
  workflow() %>% 
  add_recipe(reg_rec) %>% 
  add_model(reg_rule)

# モデル構築
reg_model = 
  reg_wf %>% 
  fit(data = train) %>% 
  with_seed(1234, .)

# 結果
reg_result =
  reg_model %>% 
  predict(new_data = test) %>% 
  bind_cols(test) %>% 
  select(.pred, Price)

# １．代表的な評価指標 --------------------------------------------------------------

# numeric rmse
reg_result %>% 
  rmse(truth = Price, estimate = .pred)

# numeric mae
reg_result %>% 
  mae(truth = Price, estimate = .pred)

# 分類の結果（ensembleは，「5.tune_dials.R」で作成したものを使用してください．）
class_result = 
  ensemble %>% 
  bind_cols(test) %>% 
  select(starts_with(".pred"), Status)

# class accuracy
class_result %>% 
  accuracy(truth = Status, estimate = .pred_class_all)

# class kap
class_result %>% 
  kap(truth = Status, estimate = .pred_class_all)

# class prob mn_logloss
class_result %>% 
  mn_log_loss(truth = Status, estimate = .pred_bad_all)

# class prob roc_auc
class_result %>% 
  roc_auc(truth = Status, estimate = .pred_bad_all)

# ２．便利関数 ------------------------------------------------------------------

# ① metric_set
reg_metrics =
  metric_set(rmse, mae)

reg_result %>% 
  reg_metrics(truth = Price, estimate = .pred)

class_metrics = 
  metric_set(accuracy, kap)

class_result %>% 
  class_metrics(truth = Status, estimate = .pred_class_all)

# ② conf_mat
class_result %>% 
  conf_mat(truth = Status, estimate = .pred_class_all)

# ② conf_mat（多クラス）  
hpc_cv %>% 
  tibble() %>% 
  conf_mat(truth = obs, estimate = pred)

