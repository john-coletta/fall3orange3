# Needed Libraries for Analysis #
library(graphics)
library(ks)
library(lubridate)
library(tidyverse)
library(triangle)
library(MASS)
library(rgl)
library(readxl)
# Use for truncated normal
#install.packages('truncnorm')
library(truncnorm)
library(ggplot2)
library(scales)
library(Hmisc)

set.seed(69)
simulation.size = 100000

Drilling <- read_excel('C:\\Users\\johnb\\OneDrive\\Documents\\MSA\\Fall 3\\SimulationRisk\\Drillingcosts.xlsx')

Drilling$Year <- year(Drilling$Date)
Drilling_new <- subset(Drilling, Drilling$Year > 1990 & Drilling$Year <2007)
Drilling_new$avg <- rowMeans(subset(Drilling_new,
                                    select = c(Cost_CrudeOil,Cost_NaturalGas,Cost_DryWell)), na.rm = TRUE)

#Concatenating the 48 obs for change in costs into one vector
Change <- c(t(Drilling_new[5:7]))
Change <- as.numeric(Change)
m <- mean(Change)
sd <- sd(Change)


#Vectors to store the simulated costs for 2019
Cost2019 <- rep(0,simulation.size)

#Initial value of Drilling Costs
C2006 <- 2279.8

for(i in 1:simulation.size){
  x <- C2006 #initial cost 
  
  #Loop calculates the cost from 2007 to 2012
  for(j in 1:6){
    r <- rnorm(n=1, mean=m, sd=sd) #normal distribution b/w 2006-2012
    x <- x*(1+r)
  }
  y <- x
  
  #Loop calculates the cost from 2013 to 2015
  for(j in 1:3){
    r <- rtriangle(1, -0.22, -0.07, -0.0917)  #triangle distribution b/w 2012-2015
    y <- y*(1+r)
  }
  z <- y
  
  #Loop calculates the cost from 2016 to 2019
  for(j in 1:4){
    r <- rtriangle(1, 0.02, 0.06, 0.05) #triangle distribution b/w 2015-2019
    z <- z*(1+r)
  }
  
  #Stores the simulated cost for 2019 in the vector
  Cost2019[i] <- z
}

PriceProj <- read_excel('C:\\Users\\johnb\\OneDrive\\Documents\\MSA\\Fall 3\\SimulationRisk\\Priceprojections.xlsx')

Drilling_cost <- Cost2019 * 1000.0

# Below calculates the year 0 costs
Seismic <- 43000*rnorm(simulation.size, mean = 3, sd = .35)
Leased <- 960*rnorm(simulation.size, mean = 600, sd = 50 )
Completion <- rnorm(simulation.size, mean = 390000, sd = 50000)


Overhead2018 <- rtriangle(simulation.size, a = 172000, b = 279500, c = 215000 )
Overhead2019 <- rtriangle(simulation.size, a = 172000, b = 279500, c = 215000 )
Overhead2020 <- rtriangle(simulation.size, a = 172000, b = 279500, c = 215000 )
Overhead2021 <- rtriangle(simulation.size, a = 172000, b = 279500, c = 215000 )
Overhead2022 <- rtriangle(simulation.size, a = 172000, b = 279500, c = 215000 )
Overhead2023 <- rtriangle(simulation.size, a = 172000, b = 279500, c = 215000 )
Overhead2024 <- rtriangle(simulation.size, a = 172000, b = 279500, c = 215000 )
Overhead2025 <- rtriangle(simulation.size, a = 172000, b = 279500, c = 215000 )
Overhead2026 <- rtriangle(simulation.size, a = 172000, b = 279500, c = 215000 )
Overhead2027 <- rtriangle(simulation.size, a = 172000, b = 279500, c = 215000 )
Overhead2028 <- rtriangle(simulation.size, a = 172000, b = 279500, c = 215000 )
Overhead2029 <- rtriangle(simulation.size, a = 172000, b = 279500, c = 215000 )
Overhead2030 <- rtriangle(simulation.size, a = 172000, b = 279500, c = 215000 )
Overhead2031 <- rtriangle(simulation.size, a = 172000, b = 279500, c = 215000 )
Overhead2032 <- rtriangle(simulation.size, a = 172000, b = 279500, c = 215000 )
Overhead2033 <- rtriangle(simulation.size, a = 172000, b = 279500, c = 215000 )

# Operating expenses only apply to wet wells
Operating_exp2019 <- rnorm(simulation.size, mean = 2.25, sd = .3)
Operating_exp2020 <- rnorm(simulation.size, mean = 2.25, sd = .3)
Operating_exp2021 <- rnorm(simulation.size, mean = 2.25, sd = .3)
Operating_exp2022 <- rnorm(simulation.size, mean = 2.25, sd = .3)
Operating_exp2023 <- rnorm(simulation.size, mean = 2.25, sd = .3)
Operating_exp2024 <- rnorm(simulation.size, mean = 2.25, sd = .3)
Operating_exp2025 <- rnorm(simulation.size, mean = 2.25, sd = .3)
Operating_exp2026 <- rnorm(simulation.size, mean = 2.25, sd = .3)
Operating_exp2027 <- rnorm(simulation.size, mean = 2.25, sd = .3)
Operating_exp2028 <- rnorm(simulation.size, mean = 2.25, sd = .3)
Operating_exp2029 <- rnorm(simulation.size, mean = 2.25, sd = .3)
Operating_exp2030 <- rnorm(simulation.size, mean = 2.25, sd = .3)
Operating_exp2031 <- rnorm(simulation.size, mean = 2.25, sd = .3)
Operating_exp2032 <- rnorm(simulation.size, mean = 2.25, sd = .3)
Operating_exp2033 <- rnorm(simulation.size, mean = 2.25, sd = .3)

# Dry wells don't have to be completed but do have overhead for year 0
year_0_Dry <- -(Seismic + Leased + Overhead2018 + Drilling_cost) 
year_0_Wet <- Seismic + Leased + Completion + Overhead2018 + Drilling_cost
Severance_tax <- .046
NRI <- rnorm(simulation.size, mean = .75, sd = .02)

################################### choleski for correlated variables #####

R <- matrix(data=cbind(1,0.64, 0.64, 1), nrow=2)
U <- t(chol(R))

standardize <- function(x){
  x.std = (x - mean(x))/sd(x)
  return(x.std)
}

destandardize <- function(x.std, x){
  x.old = (x.std * sd(x)) + mean(x)
  return(x.old)
}

################################## choleski continued ####
# Getting the correlation into the matrix

IP.r <- rlnorm(n=simulation.size, mean= 6, sd=0.28)
DR.r <- runif(n=simulation.size, min = .15, max = .32)
Both.r <- cbind(standardize(IP.r),standardize(DR.r))
SB.r <- U %*% t(Both.r)
SB.r <- t(SB.r)

final.SB.r <- cbind(destandardize(SB.r[,1], IP.r), 
                    destandardize(SB.r[,2], DR.r))
Prod_mat <- matrix(0, nrow = simulation.size, ncol = 16)
Rate <- rep(0,16)
Oil <- rep(0,16)

# This loop gets the production
for(j in 1:simulation.size){
  Rate[1] = final.SB.r[j,1]
  Prod_mat <- as.data.frame.matrix(Prod_mat)
  ################################################ final part choleski ###
  for(i in 1:15){
    Rate[i+1] <- (1 - final.SB.r[j,2])*Rate[i]
    Oil[i] <- 365*(Rate[i]+Rate[i+1])/2
    
  }
  Prod_mat[j,] <-  Oil
  if(j %% 10000 == 0){
    print(j)
  }
}

colnames(Prod_mat) <- c('y2019','y2020','y2021','y2022','y2023','y2024','y2025',
                        'y2026','y2027','y2028','y2029','y2030','y2031','y2032',
                        'y2033','y2034')

################################################ Price of oil in a year ####


Y2019 <- rtriangle(n = simulation.size, a = PriceProj$Low.Oil.Price[1], b = PriceProj$High.Oil.Price[1], c = PriceProj$AEO2018.Reference[1]) 
Y2020 <- rtriangle(n = simulation.size, a = PriceProj$Low.Oil.Price[2], b = PriceProj$High.Oil.Price[2], c = PriceProj$AEO2018.Reference[2]) 
Y2021 <- rtriangle(n = simulation.size, a = PriceProj$Low.Oil.Price[3], b = PriceProj$High.Oil.Price[3], c = PriceProj$AEO2018.Reference[3]) 
Y2022 <- rtriangle(n = simulation.size, a = PriceProj$Low.Oil.Price[4], b = PriceProj$High.Oil.Price[4], c = PriceProj$AEO2018.Reference[4]) 
Y2023 <- rtriangle(n = simulation.size, a = PriceProj$Low.Oil.Price[5], b = PriceProj$High.Oil.Price[5], c = PriceProj$AEO2018.Reference[5]) 
Y2024 <- rtriangle(n = simulation.size, a = PriceProj$Low.Oil.Price[6], b = PriceProj$High.Oil.Price[6], c = PriceProj$AEO2018.Reference[6]) 
Y2025 <- rtriangle(n = simulation.size, a = PriceProj$Low.Oil.Price[7], b = PriceProj$High.Oil.Price[7], c = PriceProj$AEO2018.Reference[7]) 
Y2026 <- rtriangle(n = simulation.size, a = PriceProj$Low.Oil.Price[8], b = PriceProj$High.Oil.Price[8], c = PriceProj$AEO2018.Reference[8]) 
Y2027 <- rtriangle(n = simulation.size, a = PriceProj$Low.Oil.Price[9], b = PriceProj$High.Oil.Price[9], c = PriceProj$AEO2018.Reference[9]) 
Y2028 <- rtriangle(n = simulation.size, a = PriceProj$Low.Oil.Price[10], b = PriceProj$High.Oil.Price[10], c = PriceProj$AEO2018.Reference[10]) 
Y2029 <- rtriangle(n = simulation.size, a = PriceProj$Low.Oil.Price[11], b = PriceProj$High.Oil.Price[11], c = PriceProj$AEO2018.Reference[11]) 
Y2030 <- rtriangle(n = simulation.size, a = PriceProj$Low.Oil.Price[12], b = PriceProj$High.Oil.Price[12], c = PriceProj$AEO2018.Reference[12]) 
Y2031 <- rtriangle(n = simulation.size, a = PriceProj$Low.Oil.Price[13], b = PriceProj$High.Oil.Price[13], c = PriceProj$AEO2018.Reference[13]) 
Y2032 <- rtriangle(n = simulation.size, a = PriceProj$Low.Oil.Price[14], b = PriceProj$High.Oil.Price[14], c = PriceProj$AEO2018.Reference[14]) 
Y2033 <- rtriangle(n = simulation.size, a = PriceProj$Low.Oil.Price[15], b = PriceProj$High.Oil.Price[15], c = PriceProj$AEO2018.Reference[15]) 
Y2033 <- rtriangle(n = simulation.size, a = PriceProj$Low.Oil.Price[16], b = PriceProj$High.Oil.Price[16], c = PriceProj$AEO2018.Reference[16]) 

################################################ Revenue for Oil ####
Rev_2019_wet <- ((1-Severance_tax)*(NRI*(Prod_mat$y2019*Y2019))- Overhead2019 - Operating_exp2019*Prod_mat$y2019)/(1+.1)
Rev_2020_wet <- ((1-Severance_tax)*(NRI*(Prod_mat$y2020*Y2020))- Overhead2020 - Operating_exp2020*Prod_mat$y2020)/(1+.1)^2 
Rev_2021_wet <- ((1-Severance_tax)*(NRI*(Prod_mat$y2021*Y2021))- Overhead2021 - Operating_exp2021*Prod_mat$y2021)/(1+.1)^3 
Rev_2022_wet <- ((1-Severance_tax)*(NRI*(Prod_mat$y2022*Y2022))- Overhead2022 - Operating_exp2022*Prod_mat$y2022)/(1+.1)^4 
Rev_2023_wet <- ((1-Severance_tax)*(NRI*(Prod_mat$y2023*Y2023))- Overhead2023 - Operating_exp2023*Prod_mat$y2023)/(1+.1)^5 
Rev_2024_wet <- ((1-Severance_tax)*(NRI*(Prod_mat$y2024*Y2024))- Overhead2024 - Operating_exp2024*Prod_mat$y2024)/(1+.1)^6 
Rev_2025_wet <- ((1-Severance_tax)*(NRI*(Prod_mat$y2025*Y2025))- Overhead2025 - Operating_exp2025*Prod_mat$y2025)/(1+.1)^7 
Rev_2026_wet <- ((1-Severance_tax)*(NRI*(Prod_mat$y2026*Y2026))- Overhead2026 - Operating_exp2026*Prod_mat$y2026)/(1+.1)^8 
Rev_2027_wet <- ((1-Severance_tax)*(NRI*(Prod_mat$y2027*Y2027))- Overhead2027 - Operating_exp2027*Prod_mat$y2027)/(1+.1)^9 
Rev_2028_wet <- ((1-Severance_tax)*(NRI*(Prod_mat$y2028*Y2028))- Overhead2028 - Operating_exp2028*Prod_mat$y2028)/(1+.1)^10 
Rev_2029_wet <- ((1-Severance_tax)*(NRI*(Prod_mat$y2029*Y2029))- Overhead2029 - Operating_exp2029*Prod_mat$y2029)/(1+.1)^11 
Rev_2030_wet <- ((1-Severance_tax)*(NRI*(Prod_mat$y2030*Y2030))- Overhead2030 - Operating_exp2030*Prod_mat$y2030)/(1+.1)^12 
Rev_2031_wet <- ((1-Severance_tax)*(NRI*(Prod_mat$y2031*Y2031))- Overhead2031 - Operating_exp2031*Prod_mat$y2031)/(1+.1)^13 
Rev_2032_wet <- ((1-Severance_tax)*(NRI*(Prod_mat$y2032*Y2032))- Overhead2032 - Operating_exp2032*Prod_mat$y2032)/(1+.1)^14 
Rev_2033_wet <- ((1-Severance_tax)*(NRI*(Prod_mat$y2033*Y2033))- Overhead2033 - Operating_exp2033*Prod_mat$y2033)/(1+.1)^15 

# Finally calculate the net present value and cost of a dry well
NPV<- (Rev_2019_wet+Rev_2020_wet+Rev_2021_wet+Rev_2022_wet+ 
         Rev_2023_wet+Rev_2024_wet+Rev_2025_wet+Rev_2026_wet+
         Rev_2027_wet+Rev_2028_wet+Rev_2029_wet+Rev_2030_wet+
         Rev_2031_wet+Rev_2032_wet+Rev_2033_wet)-year_0_Wet

Cost_Dry_Well <- -year_0_Dry

# Below is the hw 3 code modified to do the final project
#########################

# Get the number of dry and wet wells
hydro.dist <- numeric()
reser.dist <- numeric()
prop.dist <- rep(0,simulation.size)
total.npv <- rep(0,simulation.size)

for(j in 1:simulation.size){
  
  if(j %% 10000 == 0){
    print(j)
  }
  
  # Pull the number of wells
  num.wells <- sample(c(10:30),1,replace=T)
  produce <- rep(0, num.wells)
  # Calculate the risk for each well
  hydro.risk <- rtruncnorm(num.wells, a=0, b=1, mean=0.99, sd=0.05)
  reservoir.risk <- rtruncnorm(num.wells, a=0, b=1, mean=0.8, sd=0.1)
  
  # Calculate the probability each well produces
  p.produce <- hydro.risk * reservoir.risk
  # Calculate whether each well produces or not
  v.produce <- rbernoulli(num.wells,p.produce)
  produce <- ifelse(v.produce,1,0)
  interim.npv <- 0
  for(well in produce){
    if(well==1){
      interim.npv = interim.npv + sample(NPV,1)
    }
    if(well==0){
      interim.npv = interim.npv - sample(Cost_Dry_Well,1)
    }
    total.npv[j] = interim.npv
  }
  
  
  # Get proportion of wet wells and store simulated values
  prop.dist[j] <- mean(produce)
  hydro.dist <- c(hydro.dist,hydro.risk)
  reser.dist <- c(reser.dist,reservoir.risk)
}

total.units <- total.npv / 1000000
total.df <- as.data.frame(total.units)

med_npv <- median(total.df$total.units)
med_npv
VaR.npv <- quantile(total.df$total.units, probs=0.05)
VaR.npv
ES.npv <- total.df %>% filter(total.units < VaR.npv) %>% summarise(es=mean(total.units))
ES.npv
num.below0 <- total.df %>% filter(total.units <=0) %>% summarise(n=n())
pct.below0 <- num.below0/simulation.size

ggplot(total.df) +
  geom_histogram(mapping = aes(total.units), bins = 50, colour = "black", fill = "lightblue") +
  xlab("Projected Net Present Value for Total Project (Millions of Dollars)") +
  ylab("Count") +
  ggtitle("Simulated Distribution of Project Net Present Value") +
  geom_vline(xintercept  = VaR.npv, colour = "red", lwd = 1.25) +
  geom_vline(xintercept = med_npv, color='black',linetype=2, lwd=1.25) +
  #scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks = seq(-15,400,25), limits = c(-15,400)) +
  theme(axis.text.x = element_text(angle = 45))

Hmisc::describe(total.df)

ggplot(total.df %>% filter(total.units < VaR.npv)) +
  geom_histogram(mapping = aes(total.units), bins = 50, colour = "black", fill = "lightblue") +
  xlab("Projected Net Present Value for Total Project (Millions of Dollars)") +
  ylab("Count") +
  ggtitle("Simulated Distribution of Bottom 5% of Project Net Present Values") +
  geom_vline(xintercept  = ES.npv$es, colour = "red", lwd = 1.25) +
  geom_vline(xintercept = 0, color='black',linetype=2) +
  #scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks = seq(-25,85,10), limits = c(-15,85)) +
  theme(axis.text.x = element_text(angle = 45))
