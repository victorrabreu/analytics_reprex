### 1. Carregamento dos pacotes necessários



# Nomes dos packages

packages <- c("class", "nnet", "rpart", "rpart.plot", "e1071")

# Instalação de packages (apenas se necessário!)

installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  
  install.packages(packages[!installed_packages])
  
}

# Carregamento de packages

invisible(lapply(packages, library, character.only = TRUE))



# 2. Importação do modelo construído



load("./resultados/modelos/arvore_decisao.RData")



# 3. Importação dos novos dados, entretanto gerados



# Como o nosso modelo só faz uso de uma feature - petal.length - só precisamos de importar um ficheiro, e nem necessitamos de efetuar grandes pré-processamentos.
# Porém, para efeitos de generalização deste projecto:

iris.1.new <- read.csv2("./pipeline/4_producao/iris_1_new.csv", sep = ",")
iris.2.new <- read.csv2("./pipeline/4_producao/iris_2_new.csv", sep = ",")



# 4. Pré-processamento dos dados



iris.new <- merge(iris.1.new, iris.2.new, by = "ID")

names(iris.new) <- tolower(names(iris.new))
# anyNA(iris.new)
# anyDuplicated(iris.new)

iris.new$sepal.length <- as.numeric(iris.new$sepal.length)
iris.new$sepal.width <- as.numeric(iris.new$sepal.width)
iris.new$petal.length <- as.numeric(iris.new$petal.length)
iris.new$petal.width <- as.numeric(iris.new$petal.width)



# 5. Geração das previsões



iris.new$predicted.species <- predict(arv.dec, iris.new, type = "class")



# 6. Exportação das previsões



write.csv(iris.new, file = "pipeline/4_producao/iris_pred.csv", row.names = FALSE)
