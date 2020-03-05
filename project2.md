## Conjoint Analysis 

### Project description:
Aim at revitalizing their product portfolio based on the opinions of potential end-users by analyzing the survey result and constructing conjoint analysis and customer segmentation method.

### Procedures:
**1.Preprocessed data: change data format and impute missing value**

```
# full-data
full_data = conjointData[,c(1:3,8:15)]
full_data = full_data[-c(5,7,9,11)] # delate NA column

# regression and lm_result & prediction and pre_result
lm_result = data.frame(ID = unique(full_data$ID))
pre_result = full_data

for (i in unique(full_data$ID)){
  # pre_result
  subset = subset(full_data, ID == i)
  lm = lm(ratings ~ .-ID - profile, subset) 
  subset$ratings[is.na(subset$ratings) == TRUE] <- predict(lm,subset[is.na(subset$ratings),])
  pre_result$ratings[pre_result$ID==i] = subset$ratings
  
  # lm_result
  subset = subset(pre_result, ID == i)
  lm = lm(ratings ~ .-ID - profile, subset)
  coe = summary(lm)
  lm_result$intercept[lm_result$ID == i] = coe[["coefficients"]][1]
  lm_result$`139.99`[lm_result$ID == i] = coe[["coefficients"]][2]
  lm_result$`18 inches`[lm_result$ID == i] = coe[["coefficients"]][3]
  lm_result$Bouncing[lm_result$ID == i] = coe[["coefficients"]][4]
  lm_result$Racing[lm_result$ID == i] = coe[["coefficients"]][5]
}

```

**2. Conduct Benefit Segmentation via Cluster Analysis of Conjoint Part-Utilities**
<br><br>
Cluster Result:
<br>
<img src="images/project_2_1.png?raw=true"/>
<br>
Code:

```
runClusts = function(toClust,nClusts,print=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100){
  if(length(nClusts)>4){
    warning("Using only first 4 elements of nClusts.")
  }
  kms=list(); ps=list();
  for(i in 1:length(nClusts)){
    kms[[i]] = kmeans(toClust,nClusts[i],iter.max = iter.max, nstart=nstart)
    ps[[i]] = fviz_cluster(kms[[i]], geom = "point", data = toClust) + ggtitle(paste("k =",nClusts[i]))
    
  }
  library(gridExtra)
  if(print){
    tmp = marrangeGrob(ps, nrow = 2,ncol=2)
    print(tmp)
  }
  list(kms=kms,ps=ps)
}

runClusts(lm_result[2:5],c(2:4),print=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100)
```


**3.priori segmentation**
<br><br>
**Goal:**
<br>
Conduct a priori segmentation using the variables gender and age in order to profile the attribute preferences based on these variables. Then test whether these a priori segmentation variables affect the part-utilities. 
<br>
Identify the ideal product for the a priori segments and profile the segment-level attribute preferences if the differences are meaningful.
<br><br>
Result Visualization:
<br><br>
<img src="images/project_2_2.png?raw=true"/>
<br><br>
**Analysis Results**
<br>
age = 0 & gender = 0 (a0g0): $119.99 + 26 inches + rocking + racing
<br>
age = 0 & gender = 1 (a0g1): $119.99 + 26 inches + rocking + glamourous
<br>
age = 1 & gender = 0 (a1g0): $119.99 + 26 inches + bouncing + racing
<br>
age = 1 & gender = 1 (a1g1): $119.99 + 26 inches + rocking + glamourous
<br><br>
Code:

```
# regressino result extract for four segments

four_lm_result = data.frame(segments = c("a1g1","a1g0","a0g1","a0g0"))
for (i in c(1:0)){
  for (k in c(1:0)){
    lm = lm(ratings ~ .-ID - profile, data = c_data[c_data$age == i & c_data$gender == k,])
    coe = summary(lm) 
    four_lm_result[four_lm_result$segments == name, 2:6] = coe[["coefficients"]]
  }
}

# data frame used for plot 
col_name = c("`139.99`" , "`18 inches`", "Bouncing" ,"Racing") 
plot = data.frame(segments = rep(unique(four_lm_result$segments),each = 4),coe = rep(col_name,4),fill = c(1:16))
a = 0
for (i in unique(plot$segments)){
  for(k in c(1:4)){
    plot$fill[a+k] = as.numeric(four_lm_result[four_lm_result$segments == i,k+2])
  }
  a = a + 4
}

# plot graph
colnames(plot) = c("Segments","Product Attributes","Effection")
gplot(data=plot, aes(x=`Product Attributes`, y=Effection, fill=Segments)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired") +
  geom_text(aes(label=round(as.double(Effection),2)),position=position_dodge(0.9),vjust=-0.5)

```

**4.Simulate market shares for different product-line scenarios**
<br><br>
**Goal:**
<br>
Use disaggregate analysis with a first choice rule to forecast market shares. Calculate profitability for each product in the product line as well as the overall profitability for the firm.
<br>
Suggest the best possible product line strategy given considerations related to competitive response, cannibalization, profitability, and long-run performance. 
<br><br>
Code:
<br>
**Simulate market share**
```
# Prepare Data
d_data = data.frame(ID = c(1:200))
for (i in c(1:200)){
  for (k in c(1:16)){
    d_data[i,k+1] = c_data$ratings[c_data$ID == i & c_data$profile == k]
    names(d_data)[k+1] = paste0("profile_",k)
  }
}
# Create Functions
simFCDecisions = function(scen,data){
  inmkt = data[,scen] #construct the subsetted matrix of options
  bestOpts = apply(inmkt,1,max) #identify which option is best = max
  ret = inmkt
  for (i in c(1:nrow(inmkt))){
    for(k in c(1:length(inmkt)))
      if(inmkt[i,k] == bestOpts[i]){
        ret[i,k] = 1
      }
    else{
      ret[i,k] = 0
    }
  }
  names(ret) = names(inmkt)
  ret
}

calcUnitShares = function(decisions){
  colSums(decisions)/sum(decisions) #assumes that total decisions is market size
}

simFCShares=function(scen,data){
  decs = simFCDecisions(scen,data) #determine decisions
  calcUnitShares(decs) #calculate shares and return
}

simFCScenarios = function(scenarios,data,...){
  res = matrix(nrow=length(scenarios),ncol=length(data)) #sets everything to NA by default
  for(i in 1:length(scenarios)){ ##loop over scenarios
    res[i, scenarios[[i]] ] = simFCShares(scenarios[[i]],data,...)##  calculate market shares and save to right columns in res for the scenario
  }
  res = as.data.frame(res); 
  names(res) = names(data)
  res ##return result table
}

# for short term effection -- assume competitor does not responses
# add possible scenarios
scens = list()
scens[[1]]=c(13,5,7)
scens[[2]]=c(13,4,7)
scens[[3]]=c(13,14,7)
scens[[4]]=c(13,16,7)
scens[[5]]=c(5,4,7)
scens[[6]]=c(5,14,7)
scens[[7]]=c(5,16,7)
scens[[8]]=c(14,4,7)
scens[[9]]=c(14,16,7)
scens[[10]]=c(16,4,7)
market_share = simFCScenarios(scens,d_data[2:17])[,c(4,5,7,13,14,16)]
```
<img src="images/project_2_marketshare1.png?raw=true"/>

```
# for longer term -- assume competitor responses
# add possible scenarios
scens_2 = list()
scens_2[[1]]=c(13,5,8)
scens_2[[2]]=c(13,4,8)
scens_2[[3]]=c(13,14,8)
scens_2[[4]]=c(13,16,8)
scens_2[[5]]=c(5,4,8)
scens_2[[6]]=c(5,14,8)
scens_2[[7]]=c(5,16,8)
scens_2[[8]]=c(14,4,8)
scens_2[[9]]=c(14,16,8)
scens_2[[10]]=c(16,4,8)
market_share_2 = simFCScenarios(scens_2,d_data[2:17])[,c(4,5,8,13,14,16)]

```
<img src="images/project_2_marketshare2.png?raw=true"/>
<br><br>
**Calculate profit and choose the best scenarios combination**
<br>
Result:
<br>
Assumption: The competitor won’t respond immediately.
<br>
Our best combination: 1.5% profile 5 and 88.5% profile 14.
<br>
Maximum Profit: $266,497
<br>
Assumption: The competitor respond by lower their price to $119.99.
<br>
our best combination: profile 4 and profile 14
<br>
Profit: $379948.09 (sum of profit in year 1 and in year 2)


