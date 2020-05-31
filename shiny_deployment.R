library(rsconnect)


# for connecting to shiny

rsconnect::setAccountInfo(
  name='elliotmeador84',
  token='1B1CDD3A7799AD4233FF8BD9C3B92275',
  secret='Jetj1c83qkfkh/wdTLDSDY/ju1Pz4Zv5DNgxZ9dO')


rsconnect::deployApp('wales_food_network_shiny/')