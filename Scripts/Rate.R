coefficent_value_rate <- function(x, r=1){
  
  l <- NULL
  
  regs <- x[[1]]
  
  for (n in 1:length(regs)){ l <- c(l, regs[[n]]$coefficients[,1]["Rate"]) }
  
  names(l) <- names(regs)
  
  l <- as.data.frame(sort(l, decreasing = T))
  
  colnames(l) <- "Rate"
  
  s <- as.data.frame(values_smartlab_for_test)
  R_names <- intersect(rownames(l), rownames(s))
  
  df <- cbind.data.frame(l[R_names,], s[R_names,])
  
  rownames(df) <- R_names
  colnames(df) <- c("Rate", "Price")
  
  df$`Rate -0.5%` <- df[,"Price"] + df[,1] * r
  df$`Change (%)` = log(df[,3] / df[,2]) * 100
  
  y <- colnames(df)[4]
  
  format(round(df[order(df[[y]], decreasing = T),], 4), scientific = F)
}
coefficent_value_rate(my_stock_reg2, -.5)
