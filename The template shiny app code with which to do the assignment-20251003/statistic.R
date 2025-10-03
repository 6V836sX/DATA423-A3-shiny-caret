library(here)
library(dplyr)
library(tidyr)
library(ggplot2)


df <- read.csv(file = "Ass3Data.csv", row.names = "Patient", stringsAsFactors = TRUE)  # "Patient" is no longer a variable

# 先计算均值和方差
stats <- df %>%
  summarise(across(where(is.numeric),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        var  = ~var(.x, na.rm = TRUE))))

# 转成长表，方便绘图
stats_long <- stats %>%
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_sep = "_"
  )

# 查看整理后的数据
print(stats_long)

# 画均值（条形图）
ggplot(stats_long, aes(x = variable, y = mean)) +
  geom_col(fill = "steelblue") +
  coord_flip() + 
  labs(title = "Mean of numeric variables",
       x = "Variable", y = "Mean value")

install.packages("GGally") 
library(GGally)
ggpairs(df %>% dplyr::select(-ObservationDate))
