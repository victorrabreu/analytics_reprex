mod.medidas.erro <- function(modelo, conjunto.teste, valores.obs, transf.rev = NULL){
  
  # DOCSTRING
  
  previsoes <- predict(modelo, newdata = conjunto.teste, type = "response")
  
  if(!is.null(transf.rev)){
    
    previsoes <- do.call(transf.rev, args = list(previsoes))
    
  }
  
  erros <- valores.obs - previsoes
  
  mse <- mean(erros ^ 2)
  
  rmse <- sqrt(mean(erros ^ 2))
  
  mae <- mean(abs(erros))
  
  result <- list(mse = mse, rmse = rmse, mae = mae)
  
  return(result)
  
}