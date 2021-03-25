library(kb.modelling)
data <- f_load_one(ticker = "MATAS.CO")
saveRDS(object = data, file = "./data/stocks/MATAS_CO.RDS")
