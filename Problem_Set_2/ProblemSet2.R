# Problem Set 2 
## Load Data 
CPdata = read.csv("CongestionPricing.csv")


# Question 1(a)
# Which price would maximize the total revenue?

## Find the max price willing to pay in the data 
CPdata$maxWTP = apply(CPdata[,2:3],1,max) 
maxPrice = max(CPdata$maxWTP)
N = nrow(CPdata) #number of rows in the data

## Defining empty array variables we will be introducing
demandSP = rep(NA,maxPrice)
revenue = rep(NA,maxPrice) 
demandLond = rep(NA,maxPrice)

## Find how many people buy at each price 
for(p in 1 :maxPrice) {
  demandSP[p] = sum(CPdata$maxWTP>=p)
  demandLond[p] = demandSP[p]*192/N #convert demand to represent population in thousands 
  revenue[p] = p*demandLond[p]*1000
}

## Identify Best Price
bestRevenue = max(revenue)
bestPrice = which(revenue == bestRevenue)
print(paste("If a single price is to be charged across all time slots, the optimal price is", bestPrice))
print(paste("Total revenue for single-price strategy is", bestRevenue))

# With this price in effect, what is the total level of emissions?

## Defining empty arrays
demandNonPeakS = rep(NA,N)
demandPeakS = rep(NA,N)

## Calculate consumer surplus and classify which hours they will enter
for (r in 1:N){
  surplusNonPeakS = CPdata[r,3] - bestPrice
  surplusPeakS = CPdata[r,2] - bestPrice
  demandNonPeakS[r] = (surplusNonPeakS>surplusPeakS)*(surplusNonPeakS>=0)
  demandPeakS[r] = (surplusPeakS>=surplusNonPeakS)*(surplusPeakS>=0)
}
                                                     
## Demand of population in thousands 
demandNonPeakS_Total = sum(demandNonPeakS)*(192/N)
demandPeakS_Total = sum(demandPeakS) * (192/N)

## Calculate Average Speed and emissions at Price 8

### Non peak
avgSpeedNonPeak = 30 - 0.0625 * demandNonPeakS_Total

emissonsNonPeak = (ifelse(avgSpeedNonPeak<25,
                          617.5-16.7*avgSpeedNonPeak,
                          235.0-1.4*avgSpeedNonPeak))*demandNonPeakS_Total
###Peak
avgSpeedPeak = 30 - 0.0625 * demandPeakS_Total

emissionspeak = (ifelse(avgSpeedPeak<25,
                        617.5-16.7*avgSpeedPeak,
                        235.0-1.4*avgSpeedPeak))*demandPeakS_Total

totalEmissions = emissonsNonPeak + emissionspeak
  
print(paste("With price 8, the total level of emissions is", totalEmissions))



# Question 1(b)
# With price for non-peak at £7, what price would you recommend for the peak period?

nonPeakPrice = 7 ##price for non-peak period

CPdata$surplusNonPeak = CPdata$Nonpeak_WTP-nonPeakPrice

# Matrix of N rows, max price columns, fill with 0s
surplusPeak = matrix(0, N, maxPrice)

# Compute Maximum Peak Surplus
for (p in 1:maxPrice){ 
  for (i in 1:N){
    surplusPeak[i,p]= CPdata[i,2]-p
    } 
 }

## Initialize arrays that we will use later 
demandNonPeak = rep(0,maxPrice)
demandPeak = rep(0,maxPrice)
revenue2 = rep(0,maxPrice)
totalEmissions2 = rep(0,maxPrice)

## Compare consumer surplus and classify customer into peak and no peak

for (p in 1:maxPrice){
  surplusNonPeak = CPdata$surplusNonPeak 
  demandNonPeak[p]=sum((surplusNonPeak>surplusPeak[,p])*(surplusNonPeak>=0)) * (192/N) 
  demandPeak[p]=sum((surplusPeak[,p]>=surplusNonPeak)*(surplusPeak[,p]>=0)) * (192/N) 
  revenue2[p]=nonPeakPrice*demandNonPeak[p]*1000+p*demandPeak[p]*1000


## Level of emissions at each price 
### Non peak
avgSpeedNonPeak = 30 - 0.0625*demandNonPeak[p]

emissonsNonPeak = (ifelse(avgSpeedNonPeak<25,
                          617.5-16.7*avgSpeedNonPeak,
                          235.0-1.4*avgSpeedNonPeak))*demandNonPeak[p]
###Peak
avgSpeedPeak = 30 - 0.0625*demandPeak[p]

emissionsPeak = (ifelse(avgSpeedPeak<25,
                        617.5-16.7*avgSpeedPeak,
                        235.0-1.4*avgSpeedPeak))*demandPeak[p]

totalEmissions2 [p] = emissonsNonPeak + emissionsPeak
}

bestRevenue2 = max(revenue2)
bestPrice2 = which(revenue2 == bestRevenue2)
bestEmissions = totalEmissions2[bestPrice2]

print(paste("The recommended price for peak period is", bestPrice2))
print(paste("Total revenue for two-price strategy is", bestRevenue2))
print(paste("Total emissions for two-price strategy is" , bestEmissions))


# Question 1(c)

## Create a new Data Frame 
peakPrice = 1:maxPrice
Qc_df = data.frame( peakPrice , demandNonPeak, demandPeak,
                    revenue2, totalEmissions2)
Qc_df_filtered = subset(Qc_df, revenue2 > 1100000) # we want to look at a minimum revenue of 1100


## Find the row with the minimum emissions
min_emissions_row = Qc_df_filtered [which.min(Qc_df_filtered$totalEmissions2), ] 

## Extract Peak price for the row 
peak_price_min_emissions = min_emissions_row$peakPrice 
revenue_min_emissions = min_emissions_row$revenue2 
min_emissions = min_emissions_row$totalEmissions2

print(paste("For Question 1C the recommended price for peak period is", peak_price_min_emissions))
print(paste("Total revenue at price", peak_price_min_emissions, "is",revenue_min_emissions ))
print(paste("The minimum emissions would be:",min_emissions ))
