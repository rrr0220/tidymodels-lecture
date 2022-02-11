# {rsample}データ分割 ----------------------------------------------------------

# データの定義 
# 既知データ
known

# ０．初期分割 ------------------------------------------------------------------
initial_split = 
initial_split(known, prop = 0.8, strata = "Status") %>% 
  with_seed(1234, .)  # 第一引数にないとき.を使う

known %>% 
  select(Status)

train = training(initial_split)
test = testing(initial_split)


# １．K分割 -------------------------------------------------------------------
kfold_splits =
vfold_cv(train, v = 4) %>% 
  with_seed(1234, .)

# splits列1行目のanalysis
kfold_splits %>% 
  pluck("splits", 1) %>% 
  analysis()

# splits列1行目のassessment
kfold_splits %>% 
  pluck("splits", 1) %>% 
  assessment()

# 各行のsplitに対するanalysisとassessment
kfold_splits =
kfold_splits %>% 
  rowwise() %>% 
  mutate(
    .analysis = list(splits %>% analysis()),
    .assessment = list(splits %>% assessment())
  )


# ２．hold-out分割 ------------------------------------------------------------
hold_out_split =
validation_split(train, prop = 0.7) %>% 
  with_seed(1234, .)

# K分割のK=1の場合


# ３．leave-one-out分割 -------------------------------------------------------
leave_one_out_splits =
  loo_cv(train) 

# データが少ない時に使う

# ４．stratified分割 ----------------------------------------------------------
stratified_split =
vfold_cv(train, v = 4, strata = "Status") %>% 
  with_seed(1234, .)

train %>% 
  count(Status) %>% 
  mutate(prop=n/sum(n))

stratified_split %>% 
  rowwise() %>% 
  mutate(.analysis = list(splits %>% analysis()),
         .assessment = list(splits %>% assessment()),
         .analysis_prop_good = 
           .analysis %>%
           count(Status) %>% 
           mutate(prop=n/sum(n)) %>% 
           filter(Status == "good") %>% 
           pull(prop),
         .assessment_prop_good = 
           .assessment %>%
           count(Status) %>% 
           mutate(prop=n/sum(n)) %>% 
           filter(Status == "good") %>% 
           pull(prop)
         ) 

# １．K分割
kfold_splits =
  vfold_cv(train, v = 4) %>% 
  with_seed(1234, .)

# ０．初期分割
initial_split = 
  initial_split(known, prop = 0.8, strata = "Status") %>% 
  with_seed(1234, .)

# trainのStatusの割合確認
train %>% 
  count(Status) %>% 
  mutate(
    prop = n / sum(n)
  )

# stratified分割による各foldのStatusのgoodの割合
stratified_splits %>% 
  rowwise() %>% 
  mutate(
    .analysis = list(splits %>% analysis()),
    .assessment = list(splits %>% assessment()),
    .analysis_prop_good =
      .analysis %>% 
      count(Status) %>% 
      mutate(
        prop = n / sum(n)
      ) %>% 
      filter(Status == "good") %>% 
      pull(prop),
    .assessment_prop_good =
      .assessment %>% 
      count(Status) %>% 
      mutate(
        prop = n / sum(n)
      ) %>% 
      filter(Status == "good") %>% 
      pull(prop)
  ) %>% 
  select(splits, .analysis_prop_good, .assessment_prop_good)

# K分割による各foldのstatusのgoodの割合
kfold_splits %>% 
  rowwise() %>% 
  mutate(
    .analysis = list(splits %>% analysis()),
    .assessment = list(splits %>% assessment()),
    .analysis_prop_good =
      .analysis %>% 
      count(Status) %>% 
      mutate(
        prop = n / sum(n)
      ) %>% 
      filter(Status == "good") %>% 
      pull(prop),
    .assessment_prop_good =
      .assessment %>% 
      count(Status) %>% 
      mutate(
        prop = n / sum(n)
      ) %>% 
      filter(Status == "good") %>% 
      pull(prop)
  ) %>% 
  select(splits, .analysis_prop_good, .assessment_prop_good)


# ５．時系列分割 -----------------------------------------------------------------

# 左
timeseries_splits =
rolling_origin(
  train %>% rowid_to_column(),
  initial = 100,
  assess = 20,
  skip = 100,
  lag = 0,
  cumulative = FALSE
)

timeseries_splits %>% 
  pluck("splits", 1) %>% 
  analysis()

timeseries_splits %>% 
  pluck("splits", 1) %>% 
  assessment()

timeseries_splits %>% 
  pluck("splits", 2) %>% 
  analysis()

timeseries_splits %>% 
  pluck("splits", 2) %>% 
  assessment()

# 右
rolling_origin(
  train %>% rowid_to_column(),
  initial = 100,
  assess = 20,
  skip = 100,
  lag = 0,
  cumulative = TRUE
)





