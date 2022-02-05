# 多重リストの例
# これは悪い例ですので，反面教師にしましょう．
for i in list_i:
  for j in list_j: 
    for k in list_k: 
      function(i, j, k)

# x,y,zの平均を行ごとに算出して，Mean列に格納
diamonds %>% 
  rowwise() %>% 
  mutate(
    Mean = mean(c(x, y, z))
  ) %>% 
  select(x, y, z, Mean)

data("diamonds")
