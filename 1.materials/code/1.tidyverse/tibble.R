# data.frameであるirisをtibbleに変換
iris %>% as_tibble()

# tibble関数でtibbleを作成
tibble(
  No. = 1:3,
  language = c("R", "Python", "Julia")
)

# tribbleでtibbleを作成
tribble(
  ~No., ~language,
  #-----|---|----
  1, "R", 
  2, "Python",
  3, "Julia"
)

# tibbleの便利機能
df = 
  tibble(
    int = c(5, 10, 76),
    dbl = c(3.124, 8.425, 5.32),
    chr = c("a", "c", "E")
  )

iris %>% 
  as_tibble() %>% 
  distinct(Species)

data_1 = 
  iris %>% 
  as_tibble() %>% 
  filter(Species == "setosa")

data_2 = 
  iris %>% 
  as_tibble() %>% 
  filter(Species == "versicolor")

data_3 = 
  iris %>% 
  as_tibble() %>% 
  filter(Species == "virginica")

df_rev_1 = 
  df %>% 
  mutate(
    data = 
      list(
        data_1, data_2, data_3
      )
  )

df_rev_1 %>% 
  pluck("data", 1)

make_scatter = function(df, type){
  df_Species = 
    df %>% 
    tibble() %>% 
    filter(Species == type)
  scatter =
    df_Species %>% 
    ggplot(
      mapping = aes(x = Sepal.Length, y = Petal.Length)
    ) + 
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    theme(
      axis.text = element_text(size = 25),
      axis.title = element_text(size = 50)
    )
  return(scatter)
}

fig_1 = make_scatter(iris, "setosa")
fig_2 = make_scatter(iris, "versicolor")
fig_3 = make_scatter(iris, "virginica")

df_rev_2 = 
  df_rev_1 %>% 
  mutate(
    fig = 
      list(
        fig_1, fig_2, fig_3
      )
  )

df_rev_2 %>% 
  pluck("fig", 2)
