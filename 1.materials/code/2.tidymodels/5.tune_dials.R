# {tune}, {dials}ハイパラチューニング -----------------------------------------------

# レシピ
rec =
  recipe(Status ~ ., data = train) %>% 
  step_impute_bag(all_predictors())

# １．ハイパラ指定 ----------------------------------------------------------------

# addin使用するとなぜかtreesが表示されない
rule = 
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>%
  set_engine('ranger') %>%
  set_mode('classification')

# ２．探索範囲指定 ----------------------------------------------------------------

# ワークフロー
wf =
  workflow() %>% 
  add_model(rule) %>% 
  add_recipe(rec)

# 探索範囲確認
wf %>% 
  parameters() %>% 
  pull_dials_object("mtry")

wf %>% 
  parameters() %>% 
  pull_dials_object("trees")

wf %>% 
  parameters() %>% 
  pull_dials_object("min_n")

# 探索範囲更新
range =
  wf %>% 
  parameters() %>% 
  update(trees = trees(c(5, 1000))) %>%
  finalize(train) 

# finalizeにより，？が指定したデータフレームの列数に更新される
range %>% 
  pull_dials_object("mtry")

# updateにより5～1000に更新される
range %>% 
  pull_dials_object("trees")

# 何もしていないので初期値の範囲のまま
range %>% 
  pull_dials_object("min_n")

# mtryの？を説明変数の数にする．
# finalizeは，引数のtibbleの列数を?に割り当てる．
range = 
  wf %>% 
  parameters() %>% 
  update(trees = trees(c(5, 1000))) %>%
  finalize(rec %>% prep() %>% bake(train) %>% select(-Status)) 

# finalizeにより，？が全説明変数の数に更新される
range %>% 
  pull_dials_object("mtry")

# updateにより5～1000に更新される
range %>% 
  pull_dials_object("trees")

# 何もしていないので初期値の範囲のまま
range %>% 
  pull_dials_object("min_n")

# ３．グリッド作成 ----------------------------------------------------------------

# グリッドサーチ
range %>% grid_regular(levels = 4) 

# ランダムサーチ
range %>% grid_random(size = 50) %>% 
  with_seed(1234, .)

# latin_hypercube
range %>% grid_latin_hypercube(size = 50) %>% 
  with_seed(1234, .)

# max_entropy
range %>% grid_max_entropy(size = 50) %>% 
  with_seed(1234, .)

# ４．学習と評価 -------------------------------------------------------------------

# グリッド作成
grid =
  range %>% 
  grid_latin_hypercube(size = 3) %>% 
  with_seed(1234, .)

# 全ハイパラに対し，学習と評価
tune = 
  wf %>% 
  tune_grid(
    resamples = stratified_splits,
    grid = grid,
    control = control_grid(
      save_pred = TRUE,
      extract = extract_model
    ),
    metrics = metric_set(accuracy)
  )

# tune確認
tune

# 各Foldに対するモデル
tune %>% 
  pluck(".extracts", 2)

# 各Foldに対するassessmentの予測値
tune %>% 
  pluck(".predictions", 1)

# 各Foldに対する評価指標
tune %>% 
  pluck(".metrics", 1)

# 各Foldの評価指標のまとめ
tune %>% 
  collect_metrics()

# ５．モデル選定 -----------------------------------------------------------------

# 結果の可視化
tune %>% autoplot()

# 評価指標が１番良い結果を抽出
tune %>% 
  select_best(
    metric = "accuracy"
  )

# 評価指標が良い結果を抽出
tune %>% 
  show_best(
    metric = "accuracy",
    n = 2
  )

# ６．予測値算出 -----------------------------------------------------------------

# Ⅰ：再学習型
# ①良さげなハイパラをワークフローにセット
good_hypara_1 =
  tune %>% 
  show_best(
    metric = "accuracy",
    n = 2
  ) %>% 
  slice(1)

good_hypara_2 =
  tune %>% 
  show_best(
    metric = "accuracy",
    n = 2
  ) %>% 
  slice(2)

final_wf_1 =
  wf %>% 
  finalize_workflow(good_hypara_1)

final_wf_2 =
  wf %>% 
  finalize_workflow(good_hypara_2)

# ②良さげなワークフローを用いて，モデル作成
final_model_1 =
  final_wf_1 %>% 
  fit(train) %>% 
  with_seed(1234, .)

final_model_2 =
  final_wf_2 %>% 
  fit(train) %>% 
  with_seed(1234, .)

# ③評価データにモデルを適用し，予測値算出


# モデル１に対する予測値
pred_1 = 
  bind_cols(
    predict(final_model_1, new_data = test, type = "class"),
    predict(final_model_1, new_data = test, type = "prob")
  ) %>% 
  set_names(str_c(names(.), "_1"))

# モデル２に対する予測値
pred_2 = 
  bind_cols(
    predict(final_model_2, new_data = test, type = "class"),
    predict(final_model_2, new_data = test, type = "prob")
  ) %>% 
  set_names(str_c(names(.), "_2"))

# ７．アンサンブル ------------------------------------------------------------------
ensemble = 
  bind_cols(pred_1,pred_2) %>% 
  mutate(
    .pred_good_all = (.pred_good_1 + .pred_good_2) / 2 ,
    .pred_bad_all = 1 -.pred_good_all,
    .pred_class_all = if_else(
      .pred_good_all >= 0.5,
      "good",
      "bad"
    ) %>% 
      as.factor()
  ) %>% 
  select(.pred_class_all, .pred_good_all, .pred_bad_all)





