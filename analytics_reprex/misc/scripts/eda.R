### Autor: Victor Abreu
# Com contribuição de:
# http://www.sthda.com/english/wiki/scatter-plot-matrices-r-base-graphs

### A FAZER:

# TRADUZIR DOCUMENTAÇÃO
# ORDENAR FUNÇÕES
# eda.recode.interval - implementar intrervalos à direita
# Inserir warnings!
# Comentar código!
# Verificar variáveis!
# legendas em eda.bars.lines
# implementar intervalos de confiança assintóticos!

color.palette <- c("#F00E0E", "#F54B42", "#F58014", "#FFDD00",
                   "#86FF24", "#19FF4F", "#24E3BA", "#38DEFF",
                   "#2166FC", "#7433FF", "#AF19D1", "#F52C9A")

red.palette <- c("#ff0000", "#ff1f1f", "#ff3f3f", "#ff5f5f",
                 "#ff7f7f", "#ff9f9f", "#ffbfbf", "#ffdfdf", "#ffffff")

orange.palette <- c("#ff8000", "#ff8f1f", "#ff9f3f", "#ffaf5e", "#ffc080",
                    "#ffd0a0", "#ffdfc0", "#ffefe0", "#ffffff")

yellow.palette <- c("#ffff00", "#ffff1f", "#ffff3f", "#ffff5f",
                    "#ffff7f", "#ffff9f", "#ffffbf", "#ffffdf", "#ffffff")

green.palette <- c("#40ff00", "#57ff1f", "#70ff40", "#8aff5f",
                   "#a2ff7f", "#b9ffa0", "#d0ffbf", "#e9ffe0", "#ffffff")

blue.palette.light <- c("#00ffff", "#1fffff", "#3fffff", "#5fffff",
                        "#7fffff", "#9fffff", "#bfffff", "#dfffff", "#ffffff")

blue.palette.dark <- c("#0000ff", "#1f1fff", "#3f3fff", "#5f5fff",
                       "#7f7fff", "#9f9fff", "#bfbfff", "#dedeff", "#ffffff")

purple.palette <- c("#bf00ff", "#c71fff", "#cf40ff", "#d75eff",
                    "#df80ff", "#e79eff", "#efbfff", "#f7deff", "#ffffff")

pink.palette <- c("#ff00a0", "#ff1fab", "#ff40b7", "#ff61c3", "#ff80cf",
                  "#ff9eda", "#ffbfe7", "#ffdef3", "#ffffff")

black.palette <- c("#000000", "#1f1f1f", "#404040", "#5e5e5e", "#808080",
                   "#a1a1a1", "#bfbfbf", "#e0e0e0", "#ffffff")

eda.prop.obs <- function(data, cond, freqs = "rel", digits = 3){
  
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

eda.data.anomalies <- function(data, freqs = "rel", digits = 3){
  
  if(freqs == "rel"){
    
    tmp.1 <- length(data)
    tmp.2 <- length(data[is.na(data) | is.infinite(data) | is.nan(data) | is.null(data)])
    result <- round(tmp.2/tmp.1, digits)
    
  } else if(freqs == "abs") {
    
    result <- length(data[is.na(data) | is.infinite(data) | is.nan(data) | is.null(data)])
    
  }
  
  return(result)
  
}

eda.skewness <- function(data, na.rm = TRUE, digits = 3){
  
  centered.data <- data - mean(data, na.rm = na.rm)
  
  result <- (mean(centered.data ^ 3, na.rm = na.rm)) / ((mean(centered.data ^ 2, na.rm = na.rm)) ^ 1.5)
  
  result <- round(result, digits)
  
  return(result)
  
}

eda.kurtosis <- function(data, na.rm = TRUE, digits = 3){
  
  centered.data <- data - mean(data, na.rm = na.rm)
  
  result <- (mean(centered.data ^ 4, na.rm = na.rm)) / ((mean(centered.data ^ 2, na.rm = na.rm)) ^ 2)
  
  result <- round(result, digits)
  
  return(result)
  
}

eda.univariate.numeric.stats <- function(data, desc = NULL, na.rm = TRUE, digits = 3){
  
  result <- c(round(n <- length(data[!is.na(data) == na.rm])),
              round(mean(data, na.rm = na.rm), digits),
              round(quantile(data, c(0.50), na.rm = na.rm), digits),
              round(as.numeric(names(sort(table(data), decreasing = TRUE))[1]), digits),
              round(var(data, na.rm = na.rm), digits),
              round(sd(data, na.rm = na.rm), digits),
              round(sd(data, na.rm = na.rm)/sqrt(n), digits),
              round(sd(data, na.rm = na.rm)/mean(data, na.rm = na.rm), digits),
              round(quantile(data, c(0.25), na.rm = na.rm), digits),
              round(quantile(data, c(0.75), na.rm = na.rm), digits),
              round(IQR(data, na.rm = na.rm), digits),
              round(min(data, na.rm = na.rm), digits),
              round(max(data, na.rm = na.rm), digits),
              round(max(data, na.rm = na.rm) - min(data, na.rm = na.rm), digits),
              round(eda.skewness(data), digits),
              round(eda.kurtosis(data), digits))
  
  names(result) <- c("no.obs", "mean", "median", "mode", "variance", "std.deviation", "std.error",
                     "coef.variation", "first.quartile", "third.quartile", "inter.quartile.range", "minimum",
                     "maximum", "abs.range", "skewness", "kurtosis")
  
  result <- as.data.frame(result)
  
  colnames(result) <- desc
  
  return(result)
  
}

# eda.univariate.numeric.stats.batch <- apply(dataset, 2, eda.univariate.numeric.stats, desc = NULL, na.rm = na.rm)

eda.joint.numeric.stats <- function(data, na.rm = TRUE, collin = FALSE, threshold = 0.75, digits = 3){
  
  result <- list(no.obs = nrow(data),
                 no.variables = ncol(data),
                 mean.avg = round(colMeans(data, na.rm = na.rm), digits),
                 covariance = round(cov(data), digits),
                 correlation = round(cor(data), digits))
  
  if(collin == TRUE){
    
    result$removal <- (cor(data) >= threshold)
    
  }
  
  return(result)
  
}

eda.conditional.numeric.stats <- function(data, cond, na.rm = TRUE, digits = 3, comparison = FALSE,
                                          col.names = c("Conditional", "Not conditional", "Comparison")){
  
  result <- do.call("eda.univariate.numeric.stats", list(data[cond], digits = digits, na.rm = na.rm))
  
  if(comparison == TRUE){
    
    result <- cbind(result, eda.univariate.numeric.stats(data, digits = digits, na.rm = na.rm))
    
    result <- data.frame(result, ifelse(result[, 1] > result[, 2], "Bigger",
                                        ifelse(result[, 1] == result[, 2], "Equal", "Smaller")))
    
    colnames(result) <- col.names
    
  }
  
  return(result)
  
}

eda.conditional.numeric.groupby.stats <- function(data, cond.factor,
                                                  main = "", sub = "", xlab = "",
                                                  ylab = "", na.rm = TRUE){
  
  result <- cbind(aggregate(data, by = list(group.by = cond.factor), length),
                  aggregate(data, by = list(group.by = cond.factor), function(x) mean(x, na.rm = na.rm))[, 2],
                  aggregate(data, by = list(group.by = cond.factor), function(x) var(x, na.rm = na.rm))[, 2],
                  aggregate(data, by = list(group.by = cond.factor), function(x) sd(x, na.rm = na.rm))[, 2],
                  aggregate(data, by = list(group.by = cond.factor), function(x) quantile(x, na.rm = na.rm))[, 2],
                  aggregate(data, by = list(group.by = cond.factor), function(x) IQR(x, na.rm = na.rm))[, 2],
                  aggregate(data, by = list(group.by = cond.factor), function(x) range(x, na.rm = na.rm))[, 2],
                  aggregate(data, by = list(group.by = cond.factor), function(x) eda.skewness(x, na.rm = na.rm))[, 2],
                  aggregate(data, by = list(group.by = cond.factor), function(x) eda.kurtosis(x, na.rm = na.rm))[, 2])
  
  result <- cbind(result, as.numeric(result[, 5]) / sqrt(as.numeric(result[, 2])), as.numeric(result[, 5]) / as.numeric(result[, 3]))
  
  result <- result[, -13]
  
  names(result) <- c("factor/group", "no.obs", "mean", "variance", "std.deviation",
                     "minimum", "first.quartile", "median", "third.quartile",
                     "maximum", "inter.quartile.range", "abs.range", "skewness", "kurtosis",
                     "std.error", "coef.variation")
  
  return(result)
  
}

eda.conditional.numeric.groupby.plots <- function(data, cond.factor, k,
                                                  main = "", sub = "", xlab = "",
                                                  ylab = "", ylim = NULL, box.col = "#71BD00"){
  
  boxplot(data ~ cond.factor, main = main, sub = sub, xlab = xlab, ylab = ylab,
          ylim = ylim, col = box.col, range = k)
  
}

# eda.data.anomalies.batch <- apply(dataset, 2, eda.data.anomalies, freqs = freqs)

eda.joint.numeric.plots <- function(dataset, dot.col = "#00AFBB",
                                    line.col = "#CF1717", lwd = 2){
    
  # Correlation panel
    
  panel.cor <- function(x, y){
      
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- round(cor(x, y), digits = 2)
    txt <- paste0("R = ", r)
    cex.cor <- 0.8
    text(0.5, 0.5, txt, cex = 3 * cex.cor * sqrt(abs(r)))
      
  }
    
  # Customize upper panel
    
  upper.panel <- function(x, y, pch = 19){
      
    points(x, y, pch = pch, col = dot.col)
    abline(lm(y ~ x), col = line.col, lwd = lwd)
      
  }
  
  # Create the plots
    
  pairs(dataset, lower.panel = panel.cor, upper.panel = upper.panel)
    
}

eda.univariate.numeric.plots <- function(data, main = "", sub = "", xlab = "",
                                         ylab = "", xlim = NULL, ylim = NULL,
                                         bars.col = color.palette[9],
                                         line.col = color.palette[1], lwd = 2, breaks = NULL){
  
  if(is.integer(data[!is.na(data)]) == TRUE | is.numeric(data[!is.na(data)]) == TRUE){
    
    hist(data[!is.na(data)], breaks = breaks, freq = FALSE, col = bars.col, main = main, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim)
    lines(density(data[!is.na(data)]), col = line.col, lwd = lwd)
    
  } else {
    
    warning("The variable entered is not quantitative!")
    
  }
  
}

eda.univariate.categorical.plots <- function(data, main = "", sub = "", xlab = "",
                                             ylab = "", xlim = NULL, ylim = NULL, 
                                             bars.freqs = "rel", bars.col = color.palette,
                                             sort = TRUE, no.elem = NULL, 
                                             names = NULL, agg.name = "Other"){
  
  if(is.integer(data) == TRUE | is.factor(data) == TRUE | is.character(data) == TRUE | is.logical(data) == TRUE){
    
    result <- table(data[!is.na(data)])
    
    if(bars.freqs == "rel"){
      
      result <- prop.table(result)
      
    }
    
    if(sort == TRUE){
      
      result <- sort(result, decreasing = TRUE)
      
    }
    
    if(!is.null(no.elem)){
      
      tmp <- rep(NA, no.elem + 1)
      tmp[1:no.elem] <- result[1:no.elem]
      names(tmp)[1:no.elem] <- names(result)[1:no.elem]
      
      tmp[no.elem + 1] <- sum(result[no.elem + 1:length(result)][1:(length(result) - no.elem)])
      names(tmp)[no.elem + 1] <- agg.name
      
      result <- tmp
      
    }
    
    if(is.null(names)){
      
      desc <- names(result)
      
    }
    
    barplot(result, col = bars.col, main = main, sub = sub, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, names.arg = desc)
    
  } else {
    
    warning("The variable entered is not qualitative!")
    
  }
  
}

eda.conditional.numeric.boxplots <- function(data, cond,
                                     names = c("Conditional", "Non-conditional"),
                                     box.col = "#DBAF0F", main = "", sub = "",
                                     xlab = "", ylab = "", xlim = NULL, ylim = NULL,
                                     k = 0){
  
  if(is.integer(data) == TRUE | is.numeric(data) == TRUE){
    
    boxplot(data[cond], data, names = names, col = box.col,
            main = main, sub = sub, xlab = xlab, ylab = ylab,
            xlim = xlim, ylim = ylim, range = k)
    
  }
  
}

eda.conditional.numeric.densities <- function(data, cond, legend = TRUE,
                                      legend.names = c("Conditional", "Non-conditional"),
                                      legend.pos = "topright", lines.col = color.palette[c(9, 1)], main = "",
                                      sub = "", xlab = "", ylab = "",
                                      xlim = NULL, ylim = NULL){
  
  if(is.integer(data) == TRUE | is.numeric(data) == TRUE){
    
    plot(density(data), main = main, sub = sub, xlab = xlab, ylab = ylab,
         xlim = xlim, ylim = ylim, col = lines.col[1])
    lines(density(data[cond]), col = lines.col[2])
    
  }
  
  if(legend == TRUE){
    
    legend(legend.pos, legend = legend.names, fill = lines.col)
    
  }
  
}

eda.conditional.categorical.barplots <- function(data, cond, main = "", sub = "",
                                     xlab = "", ylab = "", ylim = c(0, 0.7),
                                     bars.col = color.palette[c(9, 1)], legend = TRUE, 
                                     legend.names = c("Conditional", "Non-conditional"),
                                     legend.pos = "topright", sort.bars = "cond",
                                     no.elem = NULL, agg.name = "Other"){
  
  if(is.integer(data) == TRUE | is.factor(data) == TRUE | is.character(data) == TRUE | is.logical(data)){
    
    global.table <- prop.table(table(data))
    cond.table <- prop.table(table(data[cond]))
    
    result <- merge(as.data.frame(global.table), as.data.frame(cond.table),
                    by.x = "data", by.y = "Var1", all = TRUE)
    
    result <- subset(result, select = -c(data))
    
    result[is.na(result)] <- 0
    
    result <- as.matrix(t(result))
    
  }
  
  if(sort.bars == "cond"){
    
    cond.table <- prop.table(table(data[cond]))
    cond.table <- sort(cond.table, decreasing = TRUE)
    
    global.table <- prop.table(table(data))
    global.table <- global.table[names(cond.table)]
    
    result <- rbind(cond.table, global.table)
    
  } else if(sort.bars == "uncond"){
    
    global.table <- prop.table(table(data))
    global.table <- sort(global.table, decreasing = TRUE)
    
    cond.table <- prop.table(table(data[cond]))
    cond.table <- cond.table[names(global.table)]
    
    result <- rbind(cond.table, global.table)
    
  }
  
  if(!is.null(no.elem)){
    
    result[1, no.elem + 1] <- sum(result[1, (no.elem + 1):ncol(result)], na.rm = TRUE)
    result[2, no.elem + 1] <- sum(result[2, (no.elem + 1):ncol(result)], na.rm = TRUE)
    
    result <- result[, 1:(no.elem + 1)]
    
    colnames(result)[no.elem + 1] <- agg.name
    
  }
  
  result[is.na(result)] <- 0
  
  barplot(result, beside = TRUE, col = bars.col, main = main, sub = sub, xlab = xlab,
          ylab = ylab, ylim = ylim)
  
  if(legend == TRUE){
    
    legend(legend.pos, legend = legend.names, fill = bars.col)
    
  }
  
  return(result)
  
}

eda.tukey.outliers <- function(dataset, column, k = 0) {
  
  inf <- quantile(dataset[[column]], c(0.25), na.rm = T) - k * IQR(dataset[[column]], na.rm = T)
  sup <- quantile(dataset[[column]], c(0.75), na.rm = T) + k * IQR(dataset[[column]], na.rm = T)
  
  index <- which(dataset[[column]] < inf | dataset[[column]] > sup)
  
  return(index)
  
}

eda.outlier.display <- function(dataset, column, k, cent.ref = "mean",
                                sort.mode = "decreasing"){
  
  result <- dataset[eda.tukey.outliers(dataset, column, k = k), ]
  
  if (cent.ref == "mean" & sort.mode == "decreasing"){
    
    result <- result[order(abs(result[[column]] - mean(result[[column]])), decreasing = TRUE), ]
    
  } else if (cent.ref == "mean" & sort.mode == "increasing") {
    
    result <- result[order(abs(result[[column]] - mean(result[[column]])), decreasing = FALSE), ]
    
  } else if (cent.ref == "mean" & sort.mode == "decreasing") {
    
    result <- result[order(abs(result[[column]] - mean(result[[column]])), decreasing = TRUE), ]
    
  } else if (cent.ref == "mean" & sort.mode == "increasing") {
    
    result <- result[order(abs(result[[column]] - mean(result[[column]])), decreasing = FALSE), ]
    
  }
  
  return(result)
  
}

eda.recode.exact <- function(data, orig, new){
  
  result <- data
  
  for (i in 1:length(orig)){
    
    result <- replace(result, data == orig[i], new[i])
    
  }
  
  return(result)
  
}

eda.recode.interval <- function(data, orig, new){
  
  result <- data
  
  for(i in 1:(length(orig) - 1)){
    
    result[data >= orig[i] & data < orig[i+1]] <- new[i]
    
  }
  
  return(result)
  
}

eda.rmd.table.display <- function(table, mode = "HTML"){
  
  if(mode%in%c("word", "Word", "powerpoint", "PowerPoint", "office", "Office")){
    
    result <- kable(table)
    
  } else if(mode%in%c("html", "HTML", "web", "Web")) {
    
    result <- table
    
  }
  
  return(result)
  
}

eda.bars.lines <- function(dataset, desc.bars, desc.lines, desc.stat = "mean",
                           main = "", sub = "", xlab = "", ylab.bars = "",
                           ylab.lines = "", ylim.bars = "", ylim.lines = "",
                           lwd = 2, bars.col = color.palette,
                           line.col = "red", margins = c(5, 5, 2, 5),
                           conf.int = FALSE, conf.level = 0.95,
                           conf.lines.col = "green", space = 0,
                           values = FALSE){
  
  means <- aggregate(dataset[[desc.lines]],
                     list(Bars = dataset[[desc.bars]]), desc.stat)
  freqs <- table(dataset[[desc.bars]])
  
  if(desc.stat == "mean" & conf.int == TRUE){
    
    vars <- aggregate(dataset[[desc.lines]],
                      list(Bars = dataset[[desc.bars]]), "var")
    ns <- aggregate(dataset[[desc.lines]],
                    list(Bars = dataset[[desc.bars]]), "length")
    ses <- vars$x/ns$x
    
  }
  
  if(is.character(ylim.bars)) {
    
    ylim.bars <- c(0, max(table(dataset[[desc.bars]])))
    
  }
  
  if(is.character(ylim.lines)) {
    
    ylim.lines <- c(floor(0.8 * min(means$x)), ceiling(1.2 * max(means$x)))
    
  }
  
  par(mar = margins)
  
  # barplot(freqs, space = space, col = bars.col, ylab = ylab.bars, ylim = ylim.bars)
  mp <- barplot(freqs, space = space, col = bars.col, ylab = ylab.bars,
                ylim = ylim.bars)
  
  par(new = TRUE)
  
  with(means, plot(means$x, main = main, sub = sub, xlab = xlab,
                   type = "l", lwd = lwd, xaxt = "n", yaxt = "n",
                   axes = F, xlim = c(min(mp), max(mp) + 1),
                   ylim = ylim.lines, col = line.col, ylab = ylab.bars))
  
  if(desc.stat == "mean" & conf.int == TRUE){
    
    with(means, lines(mp + 0.5, means$x + qnorm(conf.level/2) * ses,
                      col = conf.lines.col, lwd = lwd))
    with(means, lines(mp + 0.5, means$x + qnorm(1 - conf.level/2) * ses,
                      col = conf.lines.col, lwd = lwd))
    
  }
  
  axis(side = 4)
  
  mtext(side = 4, line = 2, ylab.lines)
  
  if(values == TRUE){
    
    return(mp)
    
  }
  
}

eda.frequency.tables <- function(dataset, var.desc = "var1", freqs = "rel",
                                 digits = 3, no.elem = NULL, sort = TRUE,
                                 names = "", agg.name = "Other"){
  
  if(freqs == "rel"){
    
    result <- prop.table(table(dataset[[var.desc]], dnn = names))
    result <- round(result, digits)
    
    if(sort == TRUE){
      
      result <- sort(result, decreasing = TRUE)
      
    }
    
    if(!is.null(no.elem)){
      
      tmp <- rep(NA, no.elem + 1)
      tmp[1:no.elem] <- result[1:no.elem]
      names(tmp)[1:no.elem] <- names(result)[1:no.elem]
      
      tmp[no.elem + 1] <- sum(result[no.elem + 1:length(result)][1:(length(result) - no.elem)])
      names(tmp)[no.elem + 1] <- agg.name
      
      result <- tmp
      
    }
    
  } else if(freqs == "abs") {
    
    result <- table(dataset[[var.desc]], dnn = names)
    
    if(sort == TRUE){
      
      result <- sort(result, decreasing = TRUE)
      
    }
    
    if(!is.null(no.elem)){
      
      tmp <- rep(NA, no.elem + 1)
      tmp[1:no.elem] <- result[1:no.elem]
      names(tmp)[1:no.elem] <- names(result)[1:no.elem]
      
      tmp[no.elem + 1] <- sum(result[no.elem + 1:length(result)][1:(length(result) - no.elem)])
      names(tmp)[no.elem + 1] <- agg.name
      
      result <- tmp
      
    }
    
  }
  
  return(result)
  
}

eda.categorical.crosstabs <- function(dataset, vars.desc = c("var1", "var2"),
                                      freqs = "comp", digits = 3, names = "",
                                      lower.lim = 0.8, upper.lim = 1.2,
                                      sort.mode = "expected", expected.min = 5){
  
  if(freqs == "rel"){
    
    result <- prop.table(table(dataset[[vars.desc[1]]], dataset[[vars.desc[2]]], dnn = names))
    result <- round(result, digits)
    
    
  } else if(freqs == "abs"){
    
    result <- table(dataset[[vars.desc[1]]], dataset[[vars.desc[2]]], dnn = names)
    
  } else if(freqs == "comp"){
    
    observed <- table(dataset[[vars.desc[1]]], dataset[[vars.desc[2]]], dnn = names)
    expected <- table(dataset[[vars.desc[1]]], dataset[[vars.desc[2]]], dnn = names)
    total <- sum(observed)
    
    tmp <- cbind(rowSums(expected)) %*% colSums(expected) / total
    
    for(i in 1:nrow(expected)){
      
      for(j in 1:ncol(expected)){
        
        expected[i, j] <- tmp[i, j]
        
      }
      
    }
    
    observed <- as.data.frame(observed)
    expected <- as.data.frame(as.table(expected))
    
    result <- cbind(observed, expected)
    result <- result[, c(1:3, 6)]
    
    result[, 4] <- round(result[, 4], digits)
    
    result <- result[result[, 4] > expected.min, ]
    
    colnames(result) <- c(vars.desc, "observed", "expected")
    
    result$ratio <- result$observed / result$expected
    
    result <- result[result$ratio < lower.lim | result$ratio > upper.lim, ]
    
    if(sort.mode == "expected"){
      
      result <- result[order(result$expected, decreasing = TRUE), ]
      
    } else if (sort.mode == "observed") {
      
      result <- result[order(result$observed, decreasing = TRUE), ]
      
    }
    
  }
  
  return(result)
  
}
