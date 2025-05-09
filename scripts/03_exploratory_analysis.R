

## корреляция

library(dplyr)
library(purrr)

vars_to_correlate <- c("GDP_per_capita", "Cereal_Yield", "Food_Imports_GDP",
                       "Arable_Land_Pct", "Food_Production_Index", "Energy_per_Capita",
                       "Agri_Employment", "Undernourishment", "Renewable_Energy_Share", 
                       "Food_Supply_Adequacy", "Ethanol_Production", "Biodiesel_Production",
                       "Food_Inflation", "Population_Total", "Industry_GDP_Share",
                       "rail_km")

# Разделим на группы по странам
data_split <- full_data %>%
        select(iso3c, year, all_of(vars_to_correlate)) %>%
        group_by(iso3c) %>%
        filter(n() >= 3) %>% 
        group_split()
cor_matrix_list <- map(data_split, ~ cor(select(.x, all_of(vars_to_correlate)), 
                                         use = "pairwise.complete.obs"))

iso_list <- map_chr(data_split, ~ unique(.x$iso3c))

cor_results <- tibble(iso3c = iso_list, cor_matrix = cor_matrix_list)
cor_results %>% filter(iso3c == "KAZ") %>% pull(cor_matrix) %>% .[[1]]

library(reshape2)

usa_cor <- cor_results %>% filter(iso3c == "USA") %>% pull(cor_matrix) %>% .[[1]]
usa_df <- melt(usa_cor)
ggplot(usa_df, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile(color = "white") +
        geom_text(aes(label = round(value, 2)), size = 4, color = "black") +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                             limits = c(-1, 1), name = "Корреляция") +
        labs(title = "Корреляционная матрица: USA", x = "", y = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))



# Динамика по странам во времени

full_data %>%
        filter(!is.na(Food_Production_Index)) %>%
        ggplot(aes(x = year, y = Food_Production_Index, color = Country.x)) +
        geom_line(alpha = 0.5) +
        labs(title = "Food Production Index по странам",
             y = "FPI (базовый уровень = 100)", x = "Год") +
        theme_minimal() +
        theme(legend.position = "none")

# FPI по странам в конкретном году

full_data %>%
        filter(year == 2020, !is.na(Food_Production_Index)) %>%
        arrange(desc(Food_Production_Index)) %>%
        slice_max(Food_Production_Index, n = 15) %>%
        ggplot(aes(x = reorder(Country.x, Food_Production_Index), 
                   y = Food_Production_Index)) +
        geom_col(fill = "darkgreen") +
        coord_flip() +
        labs(title = "Топ-15 стран по Food Production Index (2020)",
             x = "", y = "FPI") +
        theme_minimal()

# Динамика FPI для отдельных стран

countries_focus <- c("India", "Brazil", "Nigeria", "Russian Federation", "China")

full_data %>%
        filter(Country.x %in% countries_focus) %>%
        ggplot(aes(x = year, y = Food_Production_Index, color = Country.x)) +
        geom_line(linewidth = 1.2) +
        labs(title = "Food Production Index: Страны-фокус",
             y = "FPI", x = "Год", color = "Страна") +
        theme_minimal()


# построим кластеризацию стран по траекториям Food Production Index (FPI) за годы. 
# Мы определим, какие страны имеют похожую динамику продовольственного производства.

fpi_wide <- full_data %>%
        select(iso3c, Country.x, year, Food_Production_Index) %>%
        filter(!is.na(Food_Production_Index)) %>%
        pivot_wider(names_from = year, values_from = Food_Production_Index) %>%
        group_by(iso3c, Country.x) %>%
        filter(rowSums(is.na(across(where(is.numeric)))) <= 3) %>%  # допустим максимум 3 NA
        ungroup()
fpi_matrix <- fpi_wide %>%
        select(-iso3c, -Country.x) %>%
        as.data.frame()

fpi_scaled <- scale(fpi_matrix)  # нормируем траектории
row.names(fpi_scaled) <- fpi_wide$Country.x
set.seed(123)
k <- 4  # число кластеров (можно протестировать разные)

fpi_kmeans <- kmeans(fpi_scaled, centers = k, nstart = 25)
fpi_wide$cluster <- as.factor(fpi_kmeans$cluster)

library(reshape2)
library(ggplot2)
# траектории FPI по кластерам
fpi_long <- melt(cbind(fpi_wide[, c("Country.x", "cluster")], fpi_matrix),
                 id.vars = c("Country.x", "cluster"),
                 variable.name = "year", value.name = "FPI")

fpi_long$year <- as.numeric(as.character(fpi_long$year))

ggplot(fpi_long, aes(x = year, y = FPI, group = Country.x, color = cluster)) +
        geom_line(alpha = 0.6) +
        facet_wrap(~ cluster, scales = "free_y") +
        labs(title = "Кластеризация стран по траектории Food Production Index",
             x = "Год", y = "FPI", color = "Кластер") +
        theme_minimal()