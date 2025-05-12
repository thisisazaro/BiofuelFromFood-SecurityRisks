

library(plm)
library(tidyr)
library(ggplot2)
library(lmtest)
library(car)
library(mgcv)
library(panelvar)

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


bptest(fe_model)

pwartest(fe_model)


coeftest(fe_model, vcov = vcovHC(fe_model, method = "arellano", type = "HC1"))
plot(resid(fe_model), main = "Residuals of FE model")


vif(lm(Food_Production_Index ~ GDP_per_capita_log + Cereal_Yield_log + 
               Food_Imports_GDP_log + Energy_per_Capita_log + Arable_Land_Pct_log,
       data = full_data_clean))



## влияет ли производство биотоплива на использование пахотных земель

land_model <- plm(
        Arable_Land_Pct ~ log(Ethanol_Production + 1) + log(Biodiesel_Production + 1) +
                GDP_per_capita_log + log(Population_Total) + Food_Production_Index,
        data = pdata,
        model = "within"
)

summary(land_model)




undernour_model <- plm(
        Undernourishment ~ Food_Imports_GDP_log + Cereal_Yield_log + GDP_per_capita_log +
                log(Population_Total),
        data = pdata,
        model = "within"
)
summary(undernour_model)



# Нелинейная зависимость между доходами, временем и продовольственным производством

# обобщённая аддитивная смешанная модель (GAMM), которая позволяет уловить 
# нелинейные зависимости между:
# GDP_per_capita_log и Food_Production_Index
# year и Food_Production_Index
# при этом учтены случайные эффекты по странам (iso3c).



gamm_model <- gamm(
        Food_Production_Index ~ s(GDP_per_capita_log) + s(year),
        random = list(iso3c = ~1),
        data = full_data_clean
)
summary(gamm_model$gam)

plot(gamm_model$gam, pages = 1, se = TRUE, shade = TRUE)



# Увеличение продовольственного производства (Food_Production_Index) 
# снижает уровень продовольственной инфляции (Food_Inflation), контролируя 
# другие экономические и структурные факторы.


# Panel Vector Autoregression (PVAR)
pvar_model <- pvarfeols(
        dependent_vars = c("Food_Inflation", "Food_Production_Index"),
        lags = 1,
        transformation = "demean",
        data = pdata
)
summary(pvar_model)
str(pvar_model, max.level = 2)

pvar_model$OLS$coef



library(ggplot2)

# 1. Извлечение матрицы коэффициентов
A <- pvar_model$OLS$coef
rownames(A) <- colnames(A) <- c("Food_Inflation", "Food_Production_Index")

# 2. Инициализация шока (например, шок в Food_Production_Index)
shock <- c(0, 1)  # Шок только в производстве
irf_horizon <- 10
responses <- matrix(0, nrow = irf_horizon + 1, ncol = 2)
responses[1, ] <- shock

# 3. Расчёт откликов
for (h in 2:(irf_horizon + 1)) {
        responses[h, ] <- A %*% responses[h - 1, ]
}

# 4. Датафрейм для визуализации
irf_df <- data.frame(
        Horizon = 0:irf_horizon,
        Food_Inflation_Response = responses[, 1],
        Food_Production_Response = responses[, 2]
)

# 5. Построение отклика инфляции

ggplot(irf_df, aes(x = Horizon, y = Food_Inflation_Response)) +
        geom_line(color = "blue", linewidth = 1) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        labs(
                title = "Impulse Response of Food Inflation to Shock in Food Production",
                y = "Response", x = "Years Ahead"
        ) +
        theme_minimal()



ggplot(irf_df, aes(x = Horizon, y = Food_Inflation_Response)) +
        geom_line(color = "black", linewidth = 0.7) +
        geom_point(size = 1.8, shape = 21, fill = "white", color = "black", stroke = 0.3) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "gray30") +
        labs(
                title = "Импульсный отклик продовольственной инфляции\nна шок в производстве продовольствия",
                x = "Горизонт (лет вперёд)",
                y = "Отклик продовольственной инфляции"
        ) +
        theme_classic(base_size = 13) +
        theme(
                plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
                axis.title = element_text(size = 13),
                axis.text = element_text(size = 11),
                panel.grid = element_blank()
        )



## СЦЕНАРИИ

fe_model <- plm(Food_Production_Index ~ GDP_per_capita_log + Cereal_Yield_log +
                        Food_Imports_GDP_log + Energy_per_Capita_log + Arable_Land_Pct_log,
                data = pdata, model = "within")


# Базовый сценарий: средние значения за последние 5 лет
library(dplyr)

# Базовый сценарий: средние значения за последние 5 лет
base_data <- full_data_clean %>%
        filter(year %in% 2018:2023) %>%
        group_by(country) %>%
        summarize(
                GDP_per_capita_log = mean(GDP_per_capita_log, na.rm = TRUE),
                Cereal_Yield_log = mean(Cereal_Yield_log, na.rm = TRUE),
                Food_Imports_GDP_log = mean(Food_Imports_GDP_log, na.rm = TRUE),
                Energy_per_Capita_log = mean(Energy_per_Capita_log, na.rm = TRUE),
                Arable_Land_Pct_log = mean(Arable_Land_Pct_log, na.rm = TRUE)
        )

# Сценарий: рост ВВП на 10%, урожайность +5%
scenario_data <- base_data %>%
        mutate(
                GDP_per_capita_log = GDP_per_capita_log + log(1.10),
                Cereal_Yield_log = Cereal_Yield_log + log(1.05)
        )

# Предположим, у тебя есть столбец iso3c в base_data
scenario_data$iso3c <- full_data_clean %>%
        distinct(country, iso3c) %>%
        right_join(scenario_data, by = "country") %>%
        pull(iso3c)

# Добавим фиктивный год (например, 2025) — нужен для pdata.frame
scenario_data$year <- 2025

# Преобразуем в панель
pdata_scenario <- pdata.frame(scenario_data, index = c("iso3c", "year"))

# Предсказание
scenario_data$FPI_pred <- predict(fe_model, newdata = pdata_scenario)

coefs <- coef(fe_model)

scenario_data$FPI_pred <- with(scenario_data, 
                               coefs["GDP_per_capita_log"] * GDP_per_capita_log +
                                       coefs["Cereal_Yield_log"] * Cereal_Yield_log +
                                       coefs["Food_Imports_GDP_log"] * Food_Imports_GDP_log +
                                       coefs["Energy_per_Capita_log"] * Energy_per_Capita_log +
                                       coefs["Arable_Land_Pct_log"] * Arable_Land_Pct_log
)

top_countries <- scenario_data %>%
        arrange(desc(FPI_pred)) %>%
        slice_head(n = 25)

ggplot(top_countries, aes(x = reorder(country, FPI_pred), y = FPI_pred)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(
                title = "Топ-25 стран по прогнозу индекса производства продовольствия",
                x = "Страна", y = "Индекс производства продовольствия"
        ) +
        theme_minimal(base_size = 13)




# Рост производства биотоплива при высоком населении оказывает негативное 
# давление на продовольственное производство, поскольку сельхозземли и 
# ресурсы перераспределяются с продовольствия на топливо.

model <- plm(
        Food_Production_Index ~ log(Ethanol_Production + 1) * log(Population_Total) +
                log(Biodiesel_Production + 1) * log(Population_Total) +
                GDP_per_capita_log + Cereal_Yield_log + Arable_Land_Pct_log,
        data = pdata,
        model = "within"
)
summary(model)



library(ggeffects)
library(ggplot2)

full_data_clean <- full_data_clean %>%
        mutate(
                log_ethanol = log(Ethanol_Production + 1),
                log_biodiesel = log(Biodiesel_Production + 1),
                log_population = log(Population_Total)
        )

pdata <- pdata.frame(full_data_clean, index = c("iso3c", "year"))

model2 <- plm(
        Food_Production_Index ~ log_ethanol * log_population +
                log_biodiesel * log_population +
                GDP_per_capita_log + Cereal_Yield_log + Arable_Land_Pct_log,
        data = pdata,
        model = "within"
)

library(ggeffects)
interaction_df <- ggpredict(model2, terms = c("log_ethanol", "log_population"))
interaction_df

ggplot(interaction_df, aes(x = x, y = predicted, color = group)) +
        geom_line(size = 1) +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
        labs(
                title = "Взаимодействие производства этанола и численности населения на FPI",
                x = "Лог(производства этанола + 1)",
                y = "Предсказанный индекс производства продовольствия (FPI)",
                color = "Лог(населения)",
                fill = "Лог(населения)"
        ) +
        theme_classic(base_size = 13) +
        theme(
                plot.title = element_text(face = "bold", hjust = 0.5),
                legend.position = "bottom"
        )






