# {parsnip}学習ルール設定 --------------------------------------------------------

boost_tree(
  tree_depth = 5,
  trees = 100,
  min_n = 30
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

boost_tree_xgboost_spec <-
  boost_tree(tree_depth = tune(), 
             trees = tune(), 
             learn_rate = tune(), 
             min_n = tune(), 
             loss_reduction = tune(), 
             sample_size = tune(), 
             stop_iter = tune()
             ) %>%
  set_engine('xgboost') %>%
  set_mode('classification')

rand_forest_ranger_spec <-
  rand_forest(
    mtry = tune(), 
    min_n = tune(),
    trees = tune()
    ) %>%
  set_engine('ranger') %>%
  set_mode('classification')



# -------------------------------------------------------------------------

boost_tree(
  tree_depth = 5,
  trees = 100,
  min_n =30
  ) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")


boost_tree_xgboost_spec <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('classification')








