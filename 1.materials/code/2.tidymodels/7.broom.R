# {broom}モデル整理 ------------------------------------------------------------

# 学習ルール
rule = 
  logistic_reg() %>% 
  set_engine("glm")

# レシピ
rec =
  recipe(Status ~ ., data = train) %>% 
  step_impute_bag(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

# ワークフロー
wf =
  workflow() %>% 
  add_recipe(rec) %>% 
  add_model(rule)

# モデル
model =
  wf %>% 
  fit(data = train) %>% 
  with_seed(1234, .)

# モデルの係数情報取得（{broom}を利用しない）
model$fit$fit$fit$coefficients

# {broom}利用

# １．モデルの成績情報取得 ------------------------------------------------------------
model %>% 
  glance()


# ２．モデルの係数情報取得 ------------------------------------------------------------
model %>% 
  tidy()


# ３．予測値算出 ------------------------------------------------------------------


# データが付属する
model %>% 
  augment(new_data = test) %>% 
  select(starts_with(".pred"), Status)

# データが付属しない
model %>% 
  predict(new_data = test) %>% 
  bind_cols(test)

# 決定木によるモデル構築

# 学習ルール
rule_rev =
  decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

# レシピ
rec_rev =
  recipe(Status ~ ., data = train) %>% 
  step_impute_bag(all_predictors())

# ワークフロー
wf_rev =
  workflow() %>% 
  add_recipe(rec_rev) %>% 
  add_model(rule_rev)

# モデル
model_rev = 
  wf_rev %>% 
  fit(data = train) %>% 
  with_seed(1234, .)

# {broom}利用

# １．モデルの成績情報
model_rev %>% 
  glance()

# ２．モデルの係数情報
model_rev %>% 
  tidy()

# ３．予測値算出（データが付属する）
model_rev %>% 
  augment(new_data = test) %>% 
  select(starts_with(".pred"), Status)
