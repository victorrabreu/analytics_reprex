### VERSÃO DE TESTES

# A FAZER:
# TRADUZIR, IMPLEMENTAR MAIS FUNÇÕES, ADICIONAR CABEÇALHOS
# ORDENAR ALFABETICAMENTE
# SEPARAR dpp.recode EM dpp.recode.exact e dpp.recode.interval, implementando
# intrervalos à direita


dpp.prop.obs <- function(data, cond, freqs = "rel", digits = 3){
  
  if(freqs == "rel"){
    
    tmp.1 <- length(data[!is.na(data) == TRUE])
    tmp.2 <- length(data[!is.na(data) == TRUE & cond])
    result <- round(tmp.2/tmp.1, digits)
    
  } else if(freqs == "abs") {
    
    result <- length(data[!is.na(data) == TRUE & cond])
    
  }
  
  return(result)
  
}

# dim(dataset)
# names(dataset)
# class(dataset)

dpp.data.anomalies <- function(data, freqs = "rel", digits = 3){
  
  if(freqs == "rel"){
    
    tmp.1 <- length(data)
    tmp.2 <- length(data[is.na(data) | is.infinite(data) | is.nan(data) | is.null(data)])
    result <- round(tmp.2/tmp.1, digits)
    
  } else if(freqs == "abs") {
    
    result <- length(data[is.na(data) | is.infinite(data) | is.nan(data) | is.null(data)])
    
  }
  
  return(result)
  
}

dpp.train.test.split <- function(df, p_train_1, p_train_2 = 1){
  
  n <- nrow(df)
  
  s_train <- floor(p_train_1 * n)
  
  train_ind <- sample(1:n, s_train, replace = FALSE)
  
  train_train_ind <- sample(train_ind,
                            floor(p_train_2 * length(train_ind)),
                            replace = FALSE)
  train_val_ind <- setdiff(train_ind, train_train_ind)
  test_ind <- setdiff(1:n, train_ind)
  
  result <- list(c(train_train_ind), c(train_val_ind), c(test_ind))
  
  names(result) <- c("train.ind", "val.ind", "test.ind")
  
  return(result)
  
}

dpp.recode.exact <- function(data, orig, new){
  
  result <- data
  
  for (i in 1:length(orig)){
    
    result <- replace(result, data == orig[i], new[i])
    
  }
  
  return(result)
  
}

dpp.recode.interval <- function(data, orig, new){
  
  result <- data
  
  for(i in 1:(length(orig) - 1)){
    
    result[data >= orig[i] & data < orig[i+1]] <- new[i]
    
  }
  
  return(result)
  
}

dpp.rmd.table.display <- function(x, mode = "HTML"){
  
  if(mode%in%c("word", "Word", "powerpoint", "PowerPoint", "office", "Office")){
    
    result <- kable(x)
    
  } else if(mode%in%c("html", "HTML", "web", "Web")) {
    
    result <- x
    
  }
  
  return(result)
  
}
