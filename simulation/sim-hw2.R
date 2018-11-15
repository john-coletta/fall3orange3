library(graphics)
library(ks)
library(tidyverse)
library(triangle)
library(MASS)
library(rgl)
library(readxl)

## WAC IS 10% which is interest per year##
set.seed(69)
WAC = 0.1

Drilling <- read_excel('C:\\Users\\johnb\\OneDrive\\Documents\\MSA\\Fall 3\\SimulationRisk\\Drillingcosts.xlsx')
PriceProj <- read_excel('C:\\Users\\johnb\\OneDrive\\Documents\\MSA\\Fall 3\\SimulationRisk\\Priceprojections.xlsx')

year<- 1:20
simulation.size <- 10000

# Set up distributions for initial expenses

Seismic <- 43000*rnorm(simulation.size, mean = 3, sd = .35)
Leased <- 960*rnorm(simulation.size, mean = 600, sd = 50 )
Completion <- rnorm(simulation.size, mean = 390000, sd = 50000)

Overhead <- rtriangle(simulation.size, a = 172000, b = 215000, c = 279500 )

# Dry wells do not have completion or overhead costs (outside of year 0)

year_0_Dry <- Seismic + Leased + Overhead
year_0_Wet <- Seismic + Leased + Completion + Overhead

# Operating expenses for the wells (if wet)
Operating_exp <- rnorm(simulation.size, mean = 2.25, sd = .3)
Severance_tax <- .046
# Net revenue Interest is applied before severance tax
NRI <- rnorm(simulation.size, mean = .75, sd = .02)

###################################

R <- matrix(data=cbind(1,0.64, 0.64, 1), nrow=2)
U <- t(chol(R))
Initial <- 1000

standardize <- function(x){
  x.std = (x - mean(x))/sd(x)
  return(x.std)
}

destandardize <- function(x.std, x){
  x.old = (x.std * sd(x)) + mean(x)
  return(x.old)
}

##################################
# Below are the productions risks
# Initial Production
# Have to set up proper parameters for rlnorm to work properly
logmean <- 420
logsd <- 120

log.loc <- log(logmean^2 / sqrt(logsd^2 + logmean^2))
log.shape <- sqrt(log(1 + (logsd^2 / logmean^2)))
IP.r <- rlnorm(n=simulation.size, log.loc, log.shape)

# Decline Rate
DR.r <- runif(n=simulation.size, min = .15, max = .32)
Both.r <- cbind(standardize(IP.r),standardize(DR.r))
# Now have to add the correlation in
SB.r <- U %*% t(Both.r)
SB.r <- t(SB.r)

# Destandardize
final.SB.r <- cbind(destandardize(SB.r[,1], IP.r), 
                    destandardize(SB.r[,2], DR.r))

Rate_mat <- matrix(0, nrow = simulation.size, ncol = 16)

# Now loop through to get the production rate for the end of each year we
# want to simulate
Rate <- rep(0,16)
Oil <- matrix(0, nrow=simulation.size, ncol=15)
for(j in 1:simulation.size){
  Rate[1] <- final.SB.r[j,1]
  for(i in 1:15){
    Rate[i+1] <- (1 - final.SB.r[j,2])*Rate[i]
    Oil[j,i] <- 365*(Rate[i]+Rate[i+1])/2
  
  }
  Rate_mat[j,] <- Rate
}
Rate_mat <- columnnames(Rate_mat)
################################################
colnames(Rate_mat) <- c('y2019','y2020','y2021','y2022','y2023','y2024','y2025',
                        'y2026','y2027','y2028','y2029','y2030','y2031','y2032',
                        'y2033','y2034')

################################################


Y2019 <- rtriangle(n = 10000, a = PriceProj$Low.Oil.Price[1], b = PriceProj$High.Oil.Price[1], c = PriceProj$AEO2018.Reference[1]) 
Y2020 <- rtriangle(n = 10000, a = PriceProj$Low.Oil.Price[2], b = PriceProj$High.Oil.Price[2], c = PriceProj$AEO2018.Reference[2]) 
Y2021 <- rtriangle(n = 10000, a = PriceProj$Low.Oil.Price[3], b = PriceProj$High.Oil.Price[3], c = PriceProj$AEO2018.Reference[3]) 
Y2022 <- rtriangle(n = 10000, a = PriceProj$Low.Oil.Price[4], b = PriceProj$High.Oil.Price[4], c = PriceProj$AEO2018.Reference[4]) 
Y2023 <- rtriangle(n = 10000, a = PriceProj$Low.Oil.Price[5], b = PriceProj$High.Oil.Price[5], c = PriceProj$AEO2018.Reference[5]) 
Y2024 <- rtriangle(n = 10000, a = PriceProj$Low.Oil.Price[6], b = PriceProj$High.Oil.Price[6], c = PriceProj$AEO2018.Reference[6]) 
Y2025 <- rtriangle(n = 10000, a = PriceProj$Low.Oil.Price[7], b = PriceProj$High.Oil.Price[7], c = PriceProj$AEO2018.Reference[7]) 
Y2026 <- rtriangle(n = 10000, a = PriceProj$Low.Oil.Price[8], b = PriceProj$High.Oil.Price[8], c = PriceProj$AEO2018.Reference[8]) 
Y2027 <- rtriangle(n = 10000, a = PriceProj$Low.Oil.Price[9], b = PriceProj$High.Oil.Price[9], c = PriceProj$AEO2018.Reference[9]) 
Y2028 <- rtriangle(n = 10000, a = PriceProj$Low.Oil.Price[10], b = PriceProj$High.Oil.Price[10], c = PriceProj$AEO2018.Reference[10]) 
Y2029 <- rtriangle(n = 10000, a = PriceProj$Low.Oil.Price[11], b = PriceProj$High.Oil.Price[11], c = PriceProj$AEO2018.Reference[11]) 
Y2030 <- rtriangle(n = 10000, a = PriceProj$Low.Oil.Price[12], b = PriceProj$High.Oil.Price[12], c = PriceProj$AEO2018.Reference[12]) 
Y2031 <- rtriangle(n = 10000, a = PriceProj$Low.Oil.Price[13], b = PriceProj$High.Oil.Price[13], c = PriceProj$AEO2018.Reference[13]) 
Y2032 <- rtriangle(n = 10000, a = PriceProj$Low.Oil.Price[14], b = PriceProj$High.Oil.Price[14], c = PriceProj$AEO2018.Reference[14]) 
Y2033 <- rtriangle(n = 10000, a = PriceProj$Low.Oil.Price[15], b = PriceProj$High.Oil.Price[15], c = PriceProj$AEO2018.Reference[15]) 
Y2033 <- rtriangle(n = 10000, a = PriceProj$Low.Oil.Price[16], b = PriceProj$High.Oil.Price[16], c = PriceProj$AEO2018.Reference[16]) 

################################################
Initial_costs <- year_0_Dry
Rev_2019_dry <- (Severance_tax*(NRI*(Rate_mat$y2019*Y2019)) - Overhead)/(1+.1)
Rev_2020_dry <- (Severance_tax*(NRI*(Rate_mat$y2020*Y2020)))/(1+.1)^2 
Rev_2021_dry <- (Severance_tax*(NRI*(Rate_mat$y2021*Y2021)))/(1+.1)^2 
  
