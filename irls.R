x <- c(1, 2, 3, 4)
y <- c(3, 4, 7, 33)

mod <- lm(y~x)
summary(mod)

SSR <- sum(resid(mod)^2)



for( i in 1:100) {
  wt <- sqrt( 1/abs(resid(mod)) )
  
  mod1 <- lm(y~x, weight=wt)
  
  
  
  if( abs((sum( resid(mod1)^2)-SSR)) < 1e-06) {
    break
  } 
  
  SSR <- sum(resid(mod1)^2)
  
  cat(wt, "\n")
  
}

summary(mod1)
wt
SSR

