library(car);library(tidyverse);
data("UScrime",package="MASS")
namvars = names(UScrime)

# Regerssion model
reg_model <- as.formula(paste0("y ~ ",paste(namvars[1:5], collapse = " + ")))

# Original
model.lm = UScrime %>% lm(reg_model, data = .) 
newdata = data.frame(M = -50:50, So = 0, #Ed = mean(UScrime$Ed), Po1 = mean(UScrime$Po1), Po2 = mean(UScrime$Po2))
                     Ed = 0, Po1 = 0, Po2 = 0)
conf_int <- predict(model.lm, newdata, interval="confidence") %>% data.frame()
conf_int$x_value <- newdata$M

conf_int %>% 
  ggplot() + 
  geom_point(data = UScrime, aes(x = M, y = y)) +
  geom_line(aes(x = x_value, y = fit),size = 2) +
  geom_line(aes(x = x_value, y = lwr),size = 2,col="red", lty=2) +
  geom_line(aes(x = x_value, y = upr),size = 2,col="red", lty=2) +
  geom_ribbon(aes(x= x_value, ymin=lwr,ymax=upr),alpha=0.3) +
  theme_bw()

# Centering for all independent variables expect 'So'
UScrim_cent <- UScrime[,c(1:5, length(UScrime))]
UScrim_cent[,c(1,3:5)] <- apply(UScrim_cent[,c(1,3:5)], 2, function(x) x - mean(x))
model.lm = UScrim_cent %>% lm(reg_model, data = .) 

newdata = data.frame(M = -50:50, 
                     So = 0, Ed = 0, Po1 = 0, Po2 = 0)
conf_int <- predict(model.lm, newdata, interval="confidence") %>% data.frame()
conf_int$x_value <- newdata[["M"]]

conf_int %>% 
  ggplot() + 
  geom_point(data = UScrim_cent, aes(x = M, y = y)) +
  geom_line(aes(x = x_value, y = fit),size = 2) +
  geom_line(aes(x = x_value, y = lwr),size = 2,col="red", lty=2) +
  geom_line(aes(x = x_value, y = upr),size = 2,col="red", lty=2) +
  geom_ribbon(aes(x= x_value, ymin=lwr,ymax=upr),alpha=0.3) +
  theme_bw()


# function for graphing confidence interval
conf_int_graph <- 
  function(target = "M", M = -50:50, So = 0, Ed = 0, Po1 = 0, Po2 = 0){
    
    newdata = data.frame(M = M, So = So, Ed = Ed, Po1 = Po1, Po2 = Po2)
    conf_int <- predict(model.lm, newdata, interval="confidence") %>% data.frame()
    conf_int$x_value <- newdata[[target]]
    
    conf_int %>% 
      ggplot() + 
      geom_point(data = UScrim_cent, aes_string(x = target, y = "y")) +
      geom_line(aes(x = x_value, y = fit),size = 2) +
      geom_line(aes(x = x_value, y = lwr),size = 2,col="red", lty=2) +
      geom_line(aes(x = x_value, y = upr),size = 2,col="red", lty=2) +
      geom_ribbon(aes(x= x_value, ymin=lwr,ymax=upr),alpha=0.3) +
      theme_bw()
  }

conf_int_graph(target = "Ed", M = 0, So = 0, Ed = -50:50, Po1 = 0, Po2 = 0)