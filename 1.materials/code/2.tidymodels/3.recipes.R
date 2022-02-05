# {recipes}特徴量エンジニアリング ----------------------------------------------------

# データ定義 
initial_split = 
  initial_split(known, prop = 0.8, strata = "Status") %>% 
  with_seed(1234, .)

train = training(initial_split)
test = testing(initial_split)


# １．数値変数の変換 ---------------------------------------------------------------
recipe(Status ~ ., data = train) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  prep() %>% 
  bake(new_data = train)

recipe(Status ~ ., data = train) %>% 
  step_log(Age)%>% 
  prep() %>% 
  bake(new_data = train)

recipe(Status ~ ., data = train) %>% 
  step_BoxCox(Age, Time)%>% 
  prep() %>% 
  bake(new_data = train)

recipe(Status ~ ., data = train) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  prep() %>% 
  bake(new_data = train)

recipe(Status ~ ., data = train) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  prep() %>% 
  bake(new_data = train)


# ２．カテゴリ変数の変換 ------------------------------------------------------------
recipe(Status ~ ., data = train) %>% 
  step_dummy(Records) %>% 
  prep() %>% 
  bake(new_data = train) %>% 
  select(starts_with("Records"))

recipe(Status ~ ., data = train) %>% 
  step_dummy(Records, one_hot = TRUE) %>% 
  prep() %>% 
  bake(new_data = train) %>% 
  select(starts_with("Records"))

recipe(Status ~ ., data = train) %>% 
  step_mutate_at(Records, fn = ~as.integer(.)) %>% 
  prep() %>% 
  bake(new_data = train)

recipe(Status ~ ., data = train) %>% 
  step_mutate_at(all_nominal_predictors(), fn = ~as.integer(.)) %>% 
  prep() %>% 
  bake(new_data = train)


# ３．欠損値の補間　①代表値補間 ---------------------------------------------------------
recipe(Status ~ ., data = train) %>% 
  step_impute_mean(Income) %>% 
  prep() %>% 
  bake(new_data = train)

recipe(Status ~ ., data = train) %>% 
  step_impute_mean(all_numeric_predictors()) %>% 
  prep() %>% 
  bake(new_data = train)

recipe(Status ~ ., data = train) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  prep() %>% 
  bake(new_data = train)

recipe(Status ~ ., data = train) %>% 
  step_impute_mode(all_nominal_predictors()) %>% 
  prep() %>% 
  bake(new_data = train)

train_db =
  train %>% 
  mutate(
    across(where(is.numeric),
           ~ as.double(.x)
    )
  )

recipe(Status ~ ., data = train_db) %>% 
  step_impute_roll(
    all_numeric_predictors(), statistic = mean, window = 5
  ) %>% 
  prep() %>% 
  bake(new_data = train_db)

recipe(Status ~ ., data = train_db) %>% 
  step_impute_roll(
    all_numeric_predictors(), statistic = median, window = 5
  ) %>% 
  prep() %>% 
  bake(new_data = train_db)


# ３．欠損値の補間　②予測補間 ----------------------------------------------------------
recipe(Status ~ . , data = train) %>% 
  step_impute_linear(
    all_numeric_predictors(),
    impute_with = imp_vars(Seniority, Time:Records, Expenses)
  ) %>% 
  prep() %>% 
  bake(new_data = train)

recipe(Status ~ . , data = train) %>% 
  step_impute_knn(
    all_numeric_predictors(),
    #impute_with = imp_vars(Seniority, Time:Records, Expenses)
  ) %>% 
  prep() %>% 
  bake(new_data = train)

recipe(Status ~ . , data = train) %>% 
  step_impute_knn(
    all_nominal_predictors(),
    #impute_with = imp_vars(Seniority, Time:Records, Expenses)
  ) %>% 
  prep() %>% 
  bake(new_data = train)

recipe(Status ~ . , data = train) %>% 
  step_impute_knn(
    all_predictors(),
    #impute_with = imp_vars(Seniority, Time:Records, Expenses)
  ) %>% 
  prep() %>% 
  bake(new_data = train)

recipe(Status ~ . , data = train) %>% 
  step_impute_bag(
    all_predictors(),
    #impute_with = imp_vars(Seniority, Time:Records, Expenses)
  ) %>% 
  prep() %>% 
  bake(new_data = train)


# ４．その他の便利関数 --------------------------------------------------------------
train_4 = 
train %>% 
  select(Status, Seniority:Age)

recipe(Status ~ ., data = train_4) %>% 
  step_mutate(new = Time + Age) %>% 
  prep() %>% 
  bake(new_data = train_4)

recipe(Status ~ ., data = train_4 %>% mutate(new = 100)) %>% 
  step_zv(all_predictors()) %>% 
  prep() %>% 
  bake(new_data = train_4 %>% mutate(new = 100))

recipe(
  Status ~ ., 
  data = train_4 %>% mutate(new = c(1, rep(0, nrow(train_4)- 1)))) %>% 
  step_nzv(all_predictors()) %>% 
  prep() %>% 
  bake(
    new_data = 
      train_4 %>% mutate(new = c(1, rep(0, nrow(train_4)- 1))))

recipe(Status ~ ., data = train_4) %>% 
  step_bs(Time, degree = 3) %>% 
  prep() %>% 
  bake(new_data = train_4)

recipe(Status ~ ., data = train_4) %>% 
  step_naomit(Home) %>% 
  prep() %>% 
  bake(new_data = train_4)

recipe(Status ~ ., data = train_4) %>% 
  step_naomit(all_predictors(), all_outcomes()) %>% 
  prep() %>% 
  bake(new_data = train_4)


