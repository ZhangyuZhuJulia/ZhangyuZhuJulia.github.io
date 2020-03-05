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
<br><br>
<img src="images/project_2_1.png?raw=true"/>
<br><br>
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
<br>
Goal:
<br>
Conduct a priori segmentation using the variables gender and age in order to
profile the attribute preferences based on these variables


**4.Simulate market shares for different product-line scenarios**
