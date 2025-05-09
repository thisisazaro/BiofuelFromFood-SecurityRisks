

library(plm)



independent_vars <- c("GDP_per_capita_log", "Cereal_Yield_log", "Food_Imports_GDP_log",
                      "Energy_per_Capita_log", "Arable_Land_Pct_log")

pdata <- pdata.frame(full_data_clean,
                     index = c("iso3c", "year"))


fe_model <- plm(Food_Production_Index ~ GDP_per_capita_log + Cereal_Yield_log +
                        Food_Imports_GDP_log + Energy_per_Capita_log + Arable_Land_Pct_log,
                data = pdata, model = "within")

summary(fe_model)


re_model <- plm(Food_Production_Index ~ GDP_per_capita_log + Cereal_Yield_log +
                        Food_Imports_GDP_log + Energy_per_Capita_log + Arable_Land_Pct_log,
                data = pdata, model = "random")

summary(re_model)

phtest(fe_model, re_model)

coeftest(fe_model, vcovHC(fe_model, type = "HC1"))
## Добавить временные эффекты
fe_time_model <- plm(Food_Production_Index ~ GDP_per_capita_log + Cereal_Yield_log +
                             Food_Imports_GDP_log + Energy_per_Capita_log + Arable_Land_Pct_log,
                     data = pdata, model = "within", effect = "twoways")

summary(fe_time_model)

