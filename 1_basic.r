a1 <- rnorm(100, mean = 0, sd = 1); 
mean(a1); sd(a1)

a2 <- rbinom(100, 1, .5)
mean(a2)

sample(10 , 2)
rep(2, 100)
c(rep(0, 100), rep(1, 100))

a1 <- c(1,2,3,4,5)
a2 <- matrix(rep(0,50), ncol = 5, nrow = 10)
a3 <- data.frame(a2)


# Data geneartion
lam_y <- matrix(c(1,2.18,1.818,0,0,0, 0,0,0,1, 1.042, 0.979), nrow = 6)
latent_cov <- matrix(c(.5, .616, 
                       .616, 5.8), nrow = 2)
error_y <- diag(0.3, 6)

cov_mat <- lam_y %*% latent_cov %*% t(lam_y) + error_y

use_data <- data.frame(MASS::mvrnorm(n = 500, 
                                     mu = rep(0,6), 
                                     Sigma = cov_mat,
                                     empirical = T) )

cov(use_data); cov_mat

# Generate folders
dir.create("data")
unlink("data", recursive = TRUE)

# delete another file
file.create(file.path("data","new_csv_file.csv"))
file.remove(file.path("data","new_csv_file.csv"))

write_csv(use_data, path = "data/use_data.csv")
write.csv(use_data, "data/use_data.csv", row.names = F)

write_delim(use_data, "data/use_data.csv", delim = "\t", col_names = F)

write.table(use_data, file = "data/use_data.txt",
            row.names=FALSE,
            col.names=FALSE,
            sep = "\t", quote = FALSE)

tbl_df(iris) %>% 
  ggplot(., aes(x = Species, y = Sepal.Length)) +
  geom_col()



prac1 <-
  prac.tibble %>% 
  select(., Y, X1, gender, group) %>% 
  filter(., group == 1 | group == 2)

prac1 %>% 
  mutate(., 
         new = X1*gender,
         old = X1*Y) %>% 
  group_by(gender) %>% 
  summarise(., 
            mean_y = mean(Y),
            mean_x1 = mean(X1),
            var_y = var(Y))

#############################
model1 <-
  prac1 %>% 
  lm(Y ~ X1 + gender, data = .)

summary(model1)

res <- list()
for(i in 2:6){
  data1 <- prac.data[, c(1, i)]
  b1 <- lm(data1[,1] ~ data1[,2], data = data1 )
  b2 <- summary(b1)
  k = i - 1
  res[[k]] <- b2
}

plus <-
  function(data = 1:10){
    k <- 0
    for(i in 1:length(data)){
      k <- k + i 
    }
    return(k)
  }

plus <-
  function(DATA = 1:10){
    
    k <- 0
    for(i in 1:length(DATA)){
      k <- k + i 
    }
    return(k)
  }

