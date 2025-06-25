# 6.2.1
rm(list = ls())
print("Hello, R World!")
5 + 3 * 2 - 10 / 2
(5 + 3) * (2 - 10) / 2
sqrt(841)
?sqrt
2^3
98^7
sqrt(-4)
x <- 42
x
hoge <- 1:10

# 6.2.2
hoge
hoge2 <- hoge * 2
hoge2
str(hoge2)
hoge2[3]
hoge2[2:5]
hoge2[c(2, 4, 6, 8, 10)]

# 6.2.3
matrix(hoge2, ncol = 2)
hoge3 <- matrix(hoge2, ncol = 2, byrow = TRUE)
hoge3
dim(hoge3)
hoge3[1, ]
hoge3[, 2]
hoge3[2, 2]
hoge3 <- as.data.frame(hoge3)
str(hoge3)
colnames(hoge3) <- c("A", "B")

# 6.2.4
install.packages("tidyverse")
library(tidyverse)
hoge3 <- as_tibble(hoge3)
hoge3
getwd()
install.packages("styler")

# 6.2.5
dat <- read.csv("BaseballDecade.csv")
head(dat)
tail(dat)
dim(dat)
names(dat)
summary(dat)
str(dat)
class(dat)
dat.tb <- dat
dat.tb

# 6.2.6
dat.tb$Name
table(dat.tb$Name)
# %>%
dat.tb$Name %>%
  table() %>%
  sort(decreasing = TRUE)
dat.tb$team %>% unique()
dat.tb$team %>%
  unique() %>%
  length()
dat.tb$team <- dat.tb$team %>% as.factor()
dat.tb$bloodType <- dat.tb$bloodType %>% as.factor()
dat.tb$position <- dat.tb$position %>% as.factor()
dat.tb %>%
  select(team, bloodType, position) %>%
  summary()

# 6.2.7
dat.tb$height %>% mean()
dat.tb$height %>% var()
dat.tb$height %>% sd()
dat.tb$height %>% range()
dat.tb$salary %>% quantile()
dat.tb$salary %>% quantile(probs = c(0, 0.25, 0.33, 0.95, 1))
dat.tb <- dat.tb %>% mutate(bmi = weight / ((height / 100)^2))
dat.tb$bmi %>% summary()
dat.tb <- dat.tb %>% mutate(bmi_category = ifelse(bmi >= 25, "HighBMI", "Standard"))
dat.tb$bmi_category %>% table()

# 6.2.8
dat.tb <- dat.tb %>%
  mutate(position2 = case_when(position == "投手" ~ "投手", TRUE ~ "野手"))
dat.tb$position2 <- dat.tb$position2 %>% as.factor()
dat.tb$position2 %>% table()
dat.tb <- dat.tb %>%
  mutate(League = case_when(
    team %in% c("Giants", "Carp", "Tigers", "Swallows", "Dragons", "DeNA") ~ "Central",
    TRUE ~ "Pacific"
  ))
dat.tb$League <- dat.tb$League %>% as.factor()
table(dat.tb$team, dat.tb$League)
dat.tb <- dat.tb %>%
  mutate(Year_num = Year %>% str_remove("年度") %>% as.numeric())
dat.tb %>%
  select(Year, Year_num) %>%
  head()

# 6.2.9
dat.tb %>%
  filter(position2 == "野手") %>%
  head()
dat.tb %>%
  filter(position2 == "野手") %>%
  summary()
dat.tb %>%
  filter(Year_num <= 2015) %>%
  head()
dat.tb %>%
  filter(Year_num == 2020 & League == "Central") %>%
  head()
dat.tb %>%
  filter(Year_num == 2020 & League == "Central") %>%
  nrow()
dat.tb %>%
  select(Name, team, height, weight) %>%
  head()
dat.tb %>%
  select(Name, team, salary, Year_num) %>%
  filter(Year_num == 2020) %>%
  head()
dat.tb %>%
  arrange(desc(salary)) %>%
  head(1)
dat.tb %>%
  filter(Year_num == 2020 & League == "Central") %>%
  arrange(desc(salary)) %>%
  head(1)
dat.tb %>%
  filter(team == "Giants") %>%
  summarise(avg_height = mean(height), avg_weight = mean(weight))

# 6.2.10
dat.tb %>%
  group_by(team) %>%
  summarise(mean_salary = mean(salary)) %>%
  arrange(desc(mean_salary))
dat.tb %>%
  group_by(Year_num, team) %>%
  summarise(mean_salary = mean(salary)) %>%
  head(10)

dat.tb %>%
  group_by(Year_num, team) %>%
  summarise(
    mean_salary = mean(salary),
    max_salary = max(salary),
    min_salary = min(salary)
  ) %>%
  head(10)
dat.tb %>%
  group_by(bloodType) %>%
  summarise(mean_bmi = mean(bmi)) %>%
  arrange(desc(mean_bmi))
dat.tb %>%
  group_by(League) %>%
  summarise(
    mean_salary = mean(salary),
    median_salary = median(salary)
  )
dat.tb %>%
  group_by(position) %>%
  summarise(
    avg_height = mean(height),
    avg_weight = mean(weight)
  ) %>%
  arrange(desc(avg_height))
dat.tb %>%
  group_by(Year_num) %>%
  summarise(total_HR = sum(HR, na.rm = TRUE))
dat.tb %>% select(Year_num, Name, height, weight) %>% head()
dat.tb %>% select(Year_num, Name, height, weight) %>% 
  filter(Year_num == 2020) %>% head()

#6.2.11
dat.tb2 <- dat.tb %>% select(Year_num, Name, height, weight) %>% 
  filter(Year_num == 2020) %>% select(-Year_num)
head(dat.tb2)
model <- lm(height ~ weight, data = dat.tb2)
dat.tb2 %>% pivot_longer(-Name, names_to = "variable", values_to = "value") %>% head()
dat.tb2_long <- dat.tb2 %>% 
  pivot_longer(-Name, names_to = "variable", values_to = "value")
str(dat.tb2_long)
dat.tb2_long %>% group_by(variable) %>% summarise(mean_value = mean(value))
dat.tb2_long %>% pivot_wider(names_from = variable, values_from = value) %>% head()
bat_stats <- dat.tb %>% filter(position2 == "野手") %>% 
  select(Year_num, Name, AtBats, Hit, HR)

#6.2.12
bat_stats <- bat_stats %>% mutate(avg = Hit / AtBats)
head(bat_stats)
bat_stats %>% arrange(desc(avg)) %>% head(10)
bat_stats %>% group_by(Year_num) %>% summarise(avg_batting = mean(avg, na.rm = TRUE))
bat_stats_long <- bat_stats %>% 
  pivot_longer(c(AtBats, Hit, HR, avg), names_to = "stat", values_to = "value")
head(bat_stats_long)
bat_stats_long %>% group_by(Name, stat) %>% 
  summarise(mean_value = mean(value, na.rm = TRUE)) %>% head()
player_avgs <- bat_stats_long %>% group_by(Name, stat) %>% 
  summarise(mean_value = mean(value, na.rm = TRUE)) %>% 
  pivot_wider(names_from = stat, values_from = mean_value)
player_avgs %>% arrange(desc(avg)) %>% head(1)

#6.2.13
ggplot()
ggplot(dat.tb, aes(x = height)) + geom_histogram()
ggplot(dat.tb, aes(x = height)) + geom_histogram(binwidth = 2)
ggplot(dat.tb, aes(x = height)) + geom_histogram(fill = "blue", color = "black")
ggplot(dat.tb, aes(x = height, y = weight)) + geom_point()
ggplot(dat.tb, aes(x = height, y = weight, color = bloodType)) + geom_point()
ggplot(dat.tb, aes(x = height, y = weight, shape = bloodType)) + geom_point()
ggplot(dat.tb, aes(x = height, y = weight, color = bloodType)) + geom_point(size = 3)
ggplot(dat.tb, aes(x = height, y = weight, color = bloodType)) + geom_point(size = 3) +
  labs(title = "身長と体重の関係")
ggplot(dat.tb, aes(x = height, y = weight, color = bloodType)) + geom_point(size = 3) +
  labs(title = "身長と体重の関係", x = "身長(cm)", y = "体重(kg)")

#6.2.14
ggplot(dat.tb, aes(x = height, y = weight)) + geom_point() + 
  facet_wrap(~ team)
ggplot(dat.tb, aes(x = height, y = weight)) + geom_point() + 
  geom_smooth(method = "lm")
ggplot(dat.tb, aes(x = height, y = weight)) + geom_point() + 
  geom_smooth()
ggplot(dat.tb, aes(x = height, y = weight, color = League)) + geom_point() + 
  geom_smooth(method = "lm")
ggplot(dat.tb, aes(x = position, y = height)) + geom_boxplot()
ggplot(dat.tb, aes(x = position2, y = height)) + geom_boxplot()
ggplot(dat.tb, aes(x = position2, y = height)) + geom_boxplot() + 
  geom_jitter(width = 0.2, alpha = 0.5)
ggplot(dat.tb, aes(x = bloodType, y = weight)) + geom_violin()
ggplot(dat.tb, aes(x = bloodType, y = weight)) + geom_violin() + 
  geom_boxplot(width = 0.1)
ggplot(dat.tb, aes(x = bloodType, y = weight, fill = League)) + geom_violin()

#6.2.15
ggplot(dat.tb, aes(x = bloodType, y = weight)) + geom_violin() + 
  facet_wrap(~ League)
ggplot(dat.tb, aes(x = bloodType, y = weight)) + geom_violin() + 
  facet_grid(League ~ .)
ggplot(dat.tb, aes(x = bloodType, y = weight)) + geom_violin() + 
  facet_grid(League ~ Year_num)
dat.tb %>% group_by(Year_num) %>% 
  summarise(avg_weight = mean(weight)) %>% 
  ggplot(aes(x = Year_num, y = avg_weight)) + geom_line()
dat.tb %>% group_by(Year_num) %>% 
  summarise(avg_weight = mean(weight)) %>% 
  ggplot(aes(x = Year_num, y = avg_weight)) + geom_line() + geom_point()
dat.tb %>% group_by(Year_num) %>% 
  summarise(avg_weight = mean(weight)) %>% 
  ggplot(aes(x = Year_num, y = avg_weight)) + geom_line() + geom_point() + 
  theme_minimal()
dat.tb %>% group_by(Year_num) %>% 
  summarise(avg_weight = mean(weight)) %>% 
  ggplot(aes(x = Year_num, y = avg_weight)) + geom_line() + geom_point() + 
  theme_light()
dat.tb %>% group_by(Year_num) %>% 
  summarise(total_HR = sum(HR, na.rm = TRUE)) %>% 
  ggplot(aes(x = Year_num, y = total_HR)) + geom_col()
dat.tb %>% group_by(Year_num) %>% 
  summarise(total_HR = sum(HR, na.rm = TRUE)) %>% 
  ggplot(aes(x = factor(Year_num), y = total_HR, fill = factor(Year_num))) + 
  geom_col()

#6.2.16
dat.tb %>% group_by(Year_num) %>% 
  summarise(total_HR = sum(HR, na.rm = TRUE)) %>% 
  ggplot(aes(x = factor(Year_num), y = total_HR, fill = factor(Year_num))) + 
  geom_col() + theme(legend.position = "none")
dat.tb %>% group_by(Year_num) %>% 
  summarise(total_HR = sum(HR, na.rm = TRUE)) %>% 
  ggplot(aes(x = factor(Year_num), y = total_HR, fill = factor(Year_num))) + 
  geom_col() + scale_fill_brewer(palette = "Set1") + 
  theme(legend.position = "none")
RColorBrewer::display.brewer.all()
dat.tb %>% group_by(Year_num) %>% 
  summarise(total_HR = sum(HR, na.rm = TRUE)) %>% 
  ggplot(aes(x = factor(Year_num), y = total_HR, fill = factor(Year_num))) + 
  geom_col() + scale_fill_grey() + 
  theme(legend.position = "none")
dat.tb %>% group_by(Year_num) %>% 
  summarise(total_HR = sum(HR, na.rm = TRUE)) %>% 
  ggplot(aes(x = factor(Year_num), y = total_HR, fill = factor(Year_num))) + 
  geom_col() + scale_fill_brewer(palette = "Blues") + 
  theme_bw() + theme(legend.position = "none")
g <- dat.tb %>% group_by(Year_num) %>% 
  summarise(total_HR = sum(HR, na.rm = TRUE)) %>% 
  ggplot(aes(x = factor(Year_num), y = total_HR, fill = factor(Year_num))) + 
  geom_col() + scale_fill_brewer(palette = "Blues") + 
  theme_bw() + theme(legend.position = "none") + 
  labs(title = "年度別HR総数", x = "年度", y = "総HR数")
print(g)
ggsave("yearly_hr.png", plot = g, width = 8, height = 6, dpi = 300)
getwd()

#6.2.17
dat.tb %>% group_by(Year_num) %>% 
  summarise(total_HR = sum(HR, na.rm = TRUE)) %>% 
  ggplot(aes(x = Year_num, y = total_HR)) + 
  geom_line() + geom_point() + 
  labs(title = "年間H総数の推移", x = "年度", y = "総HR数")
dat.tb %>% group_by(Year_num, team) %>% 
  summarise(total_HR = sum(HR, na.rm = TRUE), .groups = "drop") %>% 
  ggplot(aes(x = Year_num, y = total_HR, color = team)) + 
  geom_line() + geom_point()
dat.tb %>% group_by(Year_num, team) %>% 
  summarise(total_HR = sum(HR, na.rm = TRUE), .groups = "drop") %>% 
  ggplot(aes(x = Year_num, y = total_HR, color = team)) + 
  geom_line(linewidth = 1) + geom_point(size = 3)
dat.tb %>% filter(team %in% c("Giants", "Tigers", "Carp")) %>% 
  group_by(Year_num, team) %>% 
  summarise(total_HR = sum(HR, na.rm = TRUE), .groups = "drop") %>% 
  ggplot(aes(x = Year_num, y = total_HR, color = team)) + 
  geom_line() + geom_point()
dat.tb %>% filter(Name == "鈴木 誠也") %>% 
  ggplot(aes(x = Year_num, y = HR)) + 
  geom_line() + geom_point() + 
  labs(title = "鈴木誠也選手のHR数推移")
GGally::ggpairs(dat.tb %>% select(League, height, weight, HR, salary))
dat.tb %>% select(League, height, weight, HR, salary) %>% 
  GGally::ggpairs(mapping = aes(color = League))
summarytools::dfSummary(dat.tb) %>% summarytools::view()
if(!require(plotly)) install.packages("plotly")
plotly::plot_ly(dat.tb, x = ~height, y = ~weight, z = ~salary, color = ~League,
                type = "scatter3d", mode = "markers")

#6.2.18
nested_data <- dat.tb %>% group_by(Year_num) %>% nest()
nested_data
nested_data$data[[1]]
if(!require(purrr)) install.packages("purrr")
nested_data %>% mutate(data_size = map_int(data, nrow))
nested_data %>% mutate(avg_salary = map_dbl(data, ~ mean(.$salary)))
nested_data <- nested_data %>% 
  mutate(correlation = map_dbl(data, ~ cor(.$height, .$weight)))
nested_data <- nested_data %>%
  mutate(model = map(data, ~ lm(height ~ weight, data = .)))
nested_data %>% 
  mutate(coef = map(model, ~ coef(.))) %>% 
  mutate(intercept = map_dbl(coef, ~ .[1]),
         slope = map_dbl(coef, ~ .[2]))
nested_data %>% 
  mutate(model_summary = map(model, ~ summary(.))) %>% 
  mutate(r_squared = map_dbl(model_summary, ~ .$r.squared))

#6.2.19
top_players <- dat.tb %>% 
  group_by(team) %>% 
  filter(salary == max(salary)) %>% 
  select(Year_num, team, Name, salary)
team_data <- dat.tb %>% 
  group_by(team) %>% 
  summarise(avg_salary = mean(salary),
            avg_height = mean(height),
            avg_weight = mean(weight))
team_data %>% inner_join(top_players, by = "team")
team_data %>% left_join(top_players, by = "team")
team_data %>% full_join(top_players, by = "team")
team_data %>% anti_join(top_players, by = "team")
yearly_team_avg <- dat.tb %>% 
  group_by(Year_num, team) %>% 
  summarise(avg_salary = mean(salary), .groups = "drop")
yearly_team_avg %>% inner_join(top_players, by = c("Year_num", "team"))
df1 <- dat.tb %>% filter(Year_num == 2015) %>% select(Name, team, salary)
df2 <- dat.tb %>% filter(Year_num == 2020) %>% select(Name, team, salary)
bind_rows("2015" = df1, "2020" = df2, .id = "year")

#6.2.20
if(!require(psych)) install.packages("psych")
psych::describe(dat.tb %>% select(height, weight, salary, HR))
cor_matrix <- cor(dat.tb %>% select(height, weight, salary, HR), use = "complete.obs")
corrplot::corrplot(cor_matrix, method = "circle")
pca_result <- prcomp(dat.tb %>% select(height, weight, salary, HR) %>% na.omit,
                     scale. = TRUE, center = TRUE)
summary(pca_result)
factoextra::fviz_pca_biplot(pca_result)
scaled_data <- scale(dat.tb %>% select(height, weight))
kmeans_result <- kmeans(scaled_data, centers = 3)
dat.tb$cluster <- as.factor(kmeans_result$cluster)
ggplot(dat.tb, aes(x = height, y = weight, color = cluster)) + 
  geom_point() + 
  labs(title = "KMeans クラスタリング結果")
if(!require(DT)) install.packages("DT")
DT::datatable(dat.tb %>% select(Name, team, position, height, weight,salary))
if(!require(gganimate)) install.packages("gganimate")
hr_by_year <- dat.tb %>% 
  group_by(Year_num, team) %>% 
  summarise(total_HR = sum(HR, na.rm = TRUE), .groups = "drop")
anim <- ggplot(hr_by_year, aes(x = team, y = total_HR, fill = team)) + 
  geom_col() + 
  labs(title = "Year: {frame_time}") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  transition_time(Year_num)
animate(anim, nframes = 100)