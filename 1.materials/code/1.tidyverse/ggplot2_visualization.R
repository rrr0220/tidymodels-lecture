
# 1.キャンパス
ggplot(
  data = iris, 
  mapping = aes(x = Sepal.Length, y = Petal.Length)
)


# 1.キャンパス
ggplot(
  data = iris, 
  mapping = aes(x = Sepal.Length, y = Petal.Length)
) + 
  # 2.グラフ_1
  geom_point()


# 1.キャンパス
ggplot(
  data = iris, 
  mapping = aes(x = Sepal.Length, y = Petal.Length)
) + 
  # 2.グラフ_1
  geom_point()　+
  # 2.グラフ_2 
  geom_smooth(method = "lm", se = FALSE)


# 1.キャンパス
ggplot(
  data = iris, 
  mapping = aes(x = Sepal.Length, y = Petal.Length)
) + 
  # 2.グラフ_1
  geom_point()　+
  # 2.グラフ_2 
  geom_smooth(method = "lm", se = FALSE) + 
  # 3.体裁
  theme(
    panel.background = element_rect(fill = 'pink', colour = 'orange', size = 5),
    
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15)
    )
