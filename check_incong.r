df <- read.csv("movie_blockbusters.csv")

dim(df)
head(df)
str(df)
summary(df)

colSums(is.na(df))
sum(is.na(df))
colSums(df == "")

sum(duplicated(df))
sum(duplicated(df$movie_id))
sum(duplicated(df$title))

sum(df$year < 1980 | df$year > 2024)
sum(df$rating < 0 | df$rating > 10)
sum(df$budget_usd <= 0)
sum(df$revenue_usd <= 0)
sum(df$runtime_min <= 0)
sum(df$vote_count < 0)
sum(df$popularity < 0)

sum(paste0(floor(df$year / 10) * 10, "s") != df$decade)
sum(abs(df$revenue_usd - df$budget_usd - df$profit_usd) > 1)
sum(abs(round(df$profit_usd / df$budget_usd, 3) - df$roi) > 0.002)
sum(as.integer(df$revenue_usd > 3 * df$budget_usd & df$revenue_usd > 1e8) != df$blockbuster)

table(df$genre)
table(df$decade)

outliers <- function(x) {
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  sum(x < q[1] - 1.5 * iqr | x > q[2] + 1.5 * iqr)
}
sapply(df[c("runtime_min", "budget_usd", "revenue_usd", "rating", "vote_count", "popularity")], outliers)
sum(df$budget_usd > 4e8)
sum(df$revenue_usd > 3e9)

table(df$blockbuster)
round(prop.table(table(df$blockbuster)) * 100, 2)

sort(round(tapply(df$blockbuster, df$genre, mean) * 100, 1), decreasing = TRUE)
