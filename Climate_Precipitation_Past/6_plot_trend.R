
    model <- lm(mean_annual ~ year, data=annual_ppt)
    d <- data.frame(summary(model)$coefficients[, c(1, 4)])
    d <- cbind(row.names(d), d)
    names(d) <- c('coef', 'estimate', 'p_val')
    row.names(d) <- NULL

