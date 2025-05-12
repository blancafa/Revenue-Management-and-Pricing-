# Q1(a)-- Compute the hotel’s expected daily revenue on a FCFS basis

meanEarly = 32 # Mean Demand for Early Reservation
meanReg = 18 # Mean Demand for Regular Reservation
pReg = 195  # Price for Regular reservation
pEarly = pReg*0.8 # Price for Early Reservation # 20% discount
capacity = 23 # Capacity

ExpRevenue = rep(0,capacity+1)
for(i in 1:1){
  protect = i-1
  availableEarlyFare = capacity-protect
  ExpRevenue[i] = 0; 
  for(eR in 0:100){   # eR means Early Reservation
    soldEarlyFare = min(availableEarlyFare, eR)
    remainforRegularFair = capacity-soldEarlyFare
    for(rR in 0:100){ #rR means Regular reservation
      soldRegularFare = min(remainforRegularFair, rR)
      RevenueThisIter=pEarly*soldEarlyFare+pReg*soldRegularFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(eR,meanEarly)*dpois(rR,meanReg)
      
    }
  }
  
}
RevenueFCFS = ExpRevenue[1]
print(paste("FCFSRevenue:", round(RevenueFCFS, 1)))

## ------------ ##

#Q1(b)-- Optimal Protection Level and Expected Revenue

meanEarly = 32 # Mean Demand for Early Reservation
meanReg = 18 # Mean Demand for Regular Reservation
pReg = 195  # Price for Regular reservation
pEarly = pReg*0.8 # Price for Early Reservation # 20% discount
capacity = 23 # Capacity
ExpRevenue=rep(0,capacity+1)
for (i in 1:(capacity+1)){
  protect=i-1
  availableEarlyFare=capacity-protect;
  ExpRevenue[i]=0
  for(eR in 0:100){
    soldEarlyFare = min(availableEarlyFare, eR)
    remainforRegularFair = capacity-soldEarlyFare
    for(rR in 0:100){
      soldRegularFare = min(remainforRegularFair, rR)
      RevenueThisIter=pEarly*soldEarlyFare+pReg*soldRegularFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(eR,meanEarly)*dpois(rR,meanReg)
      
    }
    
  }
    
}
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest=Protectindexbest-1
OptimalExpRevenue=max(ExpRevenue)
print(paste("The Optimal Protection Level for Regular Fare Demand:", ProtectBest))
print(paste("The Optimal Expected Revenue is:", OptimalExpRevenue))

# ···· #
# Q1 (b)--Percentage Improvement 

FCFSRevenue = 3592
percent_improvement = ((OptimalExpRevenue - FCFSRevenue) / FCFSRevenue) * 100
print(paste("Percentage Improvement:", percent_improvement, "%"))

## ------------ ##

#Q1 (c)

# Change only Prices 
meanEarly = 32  
meanReg = 18
pReg = 220 # New Price for Regular reservation
pEarly = pReg * 0.8  # 20% discount for Early Reservation
capacity = 23   

# Hotel’s expected daily revenue on a FCFS basis (New price)
ExpRevenue = rep(0, capacity + 1)
for(i in 1:1){
  protect = i - 1
  availableEarlyFare = capacity - protect
  ExpRevenue[i] = 0; 
  for(eR in 0:100){   # eR means Early Reservation
    soldEarlyFare = min(availableEarlyFare, eR)
    remainforRegularFare = capacity - soldEarlyFare
    for(rR in 0:100){ # rR means Regular reservation
      soldRegularFare = min(remainforRegularFare, rR)
      RevenueThisIter = pEarly * soldEarlyFare + pReg * soldRegularFare
      ExpRevenue[i] = ExpRevenue[i] +
        RevenueThisIter * dpois(eR, meanEarly) * dpois(rR, meanReg)
    }
  }
}

RevenueFCFS = ExpRevenue[1]
print(paste("FCFS Revenue with new parameters:", round(RevenueFCFS, 1)))

# Optimal protection level for regular arrivals
ExpRevenue = rep(0, capacity + 1)
for (i in 1:(capacity + 1)){
  protect = i - 1
  availableEarlyFare = capacity - protect
  ExpRevenue[i] = 0
  for(eR in 0:100){
    soldEarlyFare = min(availableEarlyFare, eR)
    remainforRegularFare = capacity - soldEarlyFare
    for(rR in 0:100){
      soldRegularFare = min(remainforRegularFare, rR)
      RevenueThisIter = pEarly * soldEarlyFare + pReg * soldRegularFare
      ExpRevenue[i] = ExpRevenue[i] +
        RevenueThisIter * dpois(eR, meanEarly) * dpois(rR, meanReg)
    }
  }
}

Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest = Protectindexbest - 1
OptimalExpRevenue = max(ExpRevenue)
print(paste("The Optimal Protection Level for Regular Fare Demand:", ProtectBest))
print(paste("The Optimal Expected Revenue is:", OptimalExpRevenue))

# Calculate percentage improvement
percent_improvement = ((OptimalExpRevenue - RevenueFCFS) / RevenueFCFS) * 100
print(paste("Percentage Improvement:", percent_improvement, "%"))

### 

#Change only Demand

meanEarly = 35 # New mean demand for Early reservation
meanReg = 20 # New mean demand for Regular reservation
pReg = 195      
pEarly = pReg * 0.8  
capacity = 23   

# Hotel’s expected daily revenue on a FCFS basis
ExpRevenue = rep(0, capacity + 1)
for(i in 1:1){
  protect = i - 1
  availableEarlyFare = capacity - protect
  ExpRevenue[i] = 0; 
  for(eR in 0:100){   # eR means Early Reservation
    soldEarlyFare = min(availableEarlyFare, eR)
    remainforRegularFare = capacity - soldEarlyFare
    for(rR in 0:100){ # rR means Regular reservation
      soldRegularFare = min(remainforRegularFare, rR)
      RevenueThisIter = pEarly * soldEarlyFare + pReg * soldRegularFare
      ExpRevenue[i] = ExpRevenue[i] +
        RevenueThisIter * dpois(eR, meanEarly) * dpois(rR, meanReg)
    }
  }
}

RevenueFCFS = ExpRevenue[1]
print(paste("FCFS Revenue with new parameters:", round(RevenueFCFS, 1)))

# Optimal protection level for regular arrivals
ExpRevenue = rep(0, capacity + 1)
for (i in 1:(capacity + 1)){
  protect = i - 1
  availableEarlyFare = capacity - protect
  ExpRevenue[i] = 0
  for(eR in 0:100){
    soldEarlyFare = min(availableEarlyFare, eR)
    remainforRegularFare = capacity - soldEarlyFare
    for(rR in 0:100){
      soldRegularFare = min(remainforRegularFare, rR)
      RevenueThisIter = pEarly * soldEarlyFare + pReg * soldRegularFare
      ExpRevenue[i] = ExpRevenue[i] +
        RevenueThisIter * dpois(eR, meanEarly) * dpois(rR, meanReg)
    }
  }
}

Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest = Protectindexbest - 1
OptimalExpRevenue = max(ExpRevenue)
print(paste("The Optimal Protection Level for Regular Fare Demand:", ProtectBest))
print(paste("The Optimal Expected Revenue is:", OptimalExpRevenue))

# Calculate percentage improvement
percent_improvement = ((OptimalExpRevenue - RevenueFCFS) / RevenueFCFS) * 100
print(paste("Percentage Improvement:", percent_improvement, "%"))
