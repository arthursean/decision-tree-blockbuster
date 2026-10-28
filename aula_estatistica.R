filmes <- read.csv("~/aula/movie_blockbusters.csv")
filmes_filtrado <- c("blockbuster", "decade", "genre", "runtime_min", "budget_usd")
modelo <- rpart(blockbuster ~ ., data = meus_dados[, filmes_filtrado], method = "class")
