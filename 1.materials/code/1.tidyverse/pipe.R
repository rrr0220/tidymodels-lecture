# パイプなし
filter(
  select(txhousing, city, year, month, sales), 
  sales >= 100
  )

# パイプあり
txhousing %>% 
  select(city, year, month, sales) %>% 
  filter(sales >= 100)
