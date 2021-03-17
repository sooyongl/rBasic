library(MplusAutomation); library(data.table); library(tidyverse)
# project.path <- getwd()
# dir.create(path = paste0(project.path, "/Run Mplust in R"))

# Data geneartion
lam_y <- matrix(c(1,2.18,1.818,0,0,0, 0,0,0,1, 1.042, 0.979), nrow = 6)

latent_cov <- matrix(c(.5, .616, 
                       .616, 5.8), nrow = 2)


error_y <- diag(0.3, 6)

cov_mat <- lam_y %*% latent_cov %*% t(lam_y) + error_y

use_data <- as.data.frame(MASS::mvrnorm(500, 
                                        mu = rep(0,6), 
                                        Sigma = cov_mat,
                                        empirical = T) 
)

cov(use_data)
# write_delim(use_data, "Run Mplus in R/use_data", delim = "\t", col_names = F)
# fwrite(use_data, "Run Mplus in R/use_data.txt", sep = "\t", col.names = F)
# write.table(use_data, file = "Run Mplus in R/use_data.txt", 
#             row.names=FALSE, 
#             col.names=FALSE,
#             sep = "\t", quote = FALSE)
# write_delim(use_data, "Run Mplus in R/use_data.txt", 
#             delim = " ",
#             col_names = F)



write_tsv(use_data, "Run Mplus in R/use_data.txt", 
          col_names = F)

Mplus.model <- '

data: file is use_data.txt;
variable: names are v1-v6;

model:
  F1 by v1-v3;
  F2 by v4-v6;
  F1 with F2;

'
# cat("Hello",file="outfile.txt",sep="\n")
# cat("World",file="outfile.txt",append=TRUE)
# 
writeLines(Mplus.model, "Run Mplus in R/mplus_inp.inp")
file.show("Run Mplus in R/mplus_inp.inp")

project.path <- getwd()
setwd("C:/Users/pagas/Desktop")
runModels(paste0(project.path,"/Run Mplus in R/mplus_inp.inp"), showOutput = T)
setwd(project.path)

file.show("Run Mplus in R/mplus_inp.out")

model1 <- readModels("Run Mplus in R/mplus_inp.out")

tibble(inf = rownames(t(model1$summaries)), value = t(model1$summaries)[,1]) %>% 
  filter( inf %in% c("Parameters","ChiSqM_Value","ChiSqM_DF","CFI","TLI","RMSEA_Estimate"))

model1$parameters %>% tbl_df() %>% 
  rename_at(vars(contains("$")), ~ paste0(1:6, "_new"))


filter(str_detect(unstandardized$paramHeader, "BY"))


# readLines("Run Mplus in R/mplus_inp.out")