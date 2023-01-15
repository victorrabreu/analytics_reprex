# 1. Importação dos novos dados, entretanto gerados



# Desta vez, só precisamos de um data.frame com 3 colunas - a identificação da unidade estatística, a espécie prevista e a espécie observada
# Porém, para efeitos de generalização deste projecto:

iris.1.real <- read.csv2("./pipeline/5_monitorizacao/iris_1_real.csv", sep = ",")
iris.2.real <- read.csv2("./pipeline/5_monitorizacao/iris_2_real.csv", sep = ",")
iris.pred <- read.csv2("./pipeline/4_producao/iris_pred.csv", sep = ",")


# 2. Pré-processamento dos dados



iris.real <- merge(iris.1.real, iris.2.real, by = c("ID", "Species"))

names(iris.real) <- tolower(names(iris.real))
# anyNA(iris.real)
# anyDuplicated(iris.real)
iris.real$petal.length <- as.numeric(iris.real$petal.length)


iris.treino <- read.csv2("./pipeline/3_modelos/iris_treino.csv", sep = ",")

iris.treino <- subset(iris.treino, select = c("sepal.length", "sepal.width", "petal.length", "petal.width", "class"))
iris.treino$petal.length <- as.numeric(iris.treino$petal.length)



# 3. Avaliação do drift na variável resposta



proporcoes.treino <- table(iris.treino$class) / sum(table(iris.treino$class))
proporcoes.reais <- table(iris.real$species) / sum(table(iris.real$species))

proporcoes.treino <- as.data.frame(proporcoes.treino)
proporcoes.reais <- as.data.frame(proporcoes.reais)

colnames(proporcoes.treino) <- c("categoria", "treino")
colnames(proporcoes.reais) <- c("categoria", "real")

proporcoes <- merge(proporcoes.treino, proporcoes.reais, by = "categoria")
proporcoes$psi <- (proporcoes$treino - proporcoes$real) * (log(proporcoes$treino) - log(proporcoes$real))

pop.stab.ind <- sum(proporcoes$psi)


if (pop.stab.ind > 0 & pop.stab.ind < 0.1) {
  
  print(paste0("Resultado preliminar: o valor do PSI, igual a ", pop.stab.ind, ", indica-nos que o concept drift é reduzido"))
  
} else if (pop.stab.ind > 0.1 & pop.stab.ind < 0.2) {
  
  print(paste0("Resultado preliminar: o valor do PSI, igual a ", pop.stab.ind, ", indica-nos que o concept drift é moderado"))
  
} else if (pop.stab.ind > 0.2) {
  
  print(paste0("Resultado preliminar: o valor do PSI, igual a ", pop.stab.ind, ", indica-nos que o concept drift é elevado e, dependendo dos desvios nas métricas associadas ao modelo, poderá ser necessário um novo ajustamento!"))
  
} else {
  
  warning("Erro - valor não admissível!")
  
  }

n <- sum(table(iris.treino$class))
m <- sum(table(iris.real$species))
k <- length(table(iris.treino$class))

nivel.sig <- 0.1

valor.critico <- (1/n + 1/m) * qchisq(nivel.sig, k-1, lower.tail = FALSE)


if (pop.stab.ind > valor.critico) {
  
  print(paste0("Resultado final: O PSI, de ", pop.stab.ind, ", excede o valor crítico de ", valor.critico, ", pelo que há um concept drift materialmente relevante e, dependendo dos desvios nas métricas associadas ao modelo, poderá ser necessário um novo ajustamento!"))
  
} else {
  
  print(paste0("Resultado final: O PSI, de ", pop.stab.ind, ", não excede o valor crítico de ", valor.critico, ", pelo que não há um concept drift materialmente relevante."))
  
}



# 4. Avaliação do drift nas features utilizadas (com instalação e carregamento do pacote necessário)



packages <- c("Matching")

installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  
  install.packages(packages[!installed_packages])
  
}

invisible(lapply(packages, library, character.only = TRUE))


teste.1 <- ks.boot(Tr = iris.real$petal.length, Co = iris.treino$petal.length, nboots = 1000)

nivel.sig <- 0.1

if (teste.1$ks.boot.pvalue > nivel.sig) {
  
  print(paste0("O valor-p do teste de Kolmogorov-Smirnov, na feature utilizada pelo modelo, é de ", teste.1$ks.boot.pvalue, ", superior ao nível de significância ", nivel.sig, ", pelo que não há um feature drift materialmente relevante."))
  
} else {
  
  print(paste0("O valor-p do teste de Kolmogorov-Smirnov, na feature utilizada pelo modelo, é de ", teste.1$ks.boot.pvalue, ", inferior ao nível de significância ", nivel.sig, ", pelo que há um feature drift materialmente relevante e, dependendo dos desvios nas métricas associadas ao modelo, poderá ser necessário um novo ajustamento!"))
  
}



# 5. Cálculo de métricas de previsão



iris.real <- subset(iris.real, select = c(id, species))
iris.pred <- subset(iris.pred, select = c(id, predicted.species))

iris <- merge(iris.real, iris.pred, by = "id")



conf.mat.final <- table(iris$species, iris$predicted.species)
accuracy.final <- sum(diag(conf.mat.final))/sum(conf.mat.final)

load("./resultados/modelos/accuracy_teste.RData")

print(paste0("A variação percentual na accuracy, entre a previsão de teste e a previsão real, foi de ", (accuracy.final - accuracy.teste)/accuracy.teste * 100, "%."))



# source("~/Data & Analytics/Portfolio/analytics_reprex/scripts/monitorizacao.R")