## Humana-Mays 2019 Healthcare Analytics Case Competition 

### Case Description:
Analyze 7 million medical records of 20,000 patients provided by Humana to create a model aim at predict if members will continue opioid therapy six months after initial prescribing. Provide insights and suggestions to help identify members at risk for continued long term use of opioid therapies allowing for early intervention

### Background Information:
Throughout the early 2000s, LTOT for non cancer pain conditions (NCPC) increased considerably without a corresponding increase in incidence of NCPC.<br>
As many as 1 in 4 patients receiving long term opioid therapy in a primary care setting will struggle with opioid disorder <br>
Evidence suggests an elevated risk for overdose, abuse, misuse, and negative health outcomes related to increased dosing or longer duration therapy, including fractures, Myocardial Infarction, and sexual dysfunction.

### Definition:
1. Opioid Naïve <br>
Defined as not having an opioid ‘on hand’ in the preceding 90 day period, based on service date and pay day supply count
2. Long Term Opioid Therapy (LTOT) <br>
Defined as continuous use of an opioid medication with 90% of days covered over a 6 month period

### Procedures:
1. **Data prepartion**<br>
* **Getting insights from large and un-organized dataset**<br>
Because this case is deeply involved with large amount of medical and healthcare knowledges and backgraounds, what we did first is understand the data deeply and find relationship between variables.<br>
Based on the goal to predict if members will continue opioid therapy six months after initial prescribing, we think backward about possible causation leads to this result.<br>
After understand the logic and variables provided in the dataset, we combined variables to generate new information that will help build the prediction model and clean the data to prepare for building the model <br>
<br> * Create subset for each ID:
```
for(i in unique(full_data$id)){
  thisID = subset(full_data,id==i)
```
* **some of the new variables we created**<br>
a. new day 0 date
<br> reason to create this variable: 
<br> each patient may have multiple day 0, which represent the begining of a new 6 months period. If a patient did not take any pill for past 90 days, and the next day that he/she starts taking pill is the new day 0. We need identify all qualified day 0 for each patient in order to count have many days this patient has been taking pills within a 6 months period.
<br>Code:

```
if(length(thisID$row_num)>=2){
  for (j in 2:max(thisID$row_num)){
  thisID$new_day_0_date[1] <- 0
  ifelse(thisID$df[thisID$row_num == j] >= 90,
         thisID$new_day_0_date[thisID$row_num == j] <- thisID$new_day[thisID$row_num == j],
         thisID$new_day_0_date[thisID$row_num == j] <- NA)
     }
else{
    thisID$new_day_0_date[1] <- 0
    }
full_data$new_day_0_date[full_data$id==i] = thisID$new_day_0_date 

```
b. new time line
<br> reason to create this variable: 
<br> Because each patient can have multiple day 0 which means they have multiple 6 months period need to be evaluated. In order to avoid mis-calculation between each period, we created a new variable called new time line. This number represents which period this specific record belongs to.  
<br>Code:

```
if(length(thisID$row_num)>=2){ 
  thisID$new_time_line[1] <- 0
  for (j in 2:max(thisID$row_num)){
     ifelse(is.na(thisID$new_day_0_date[thisID$row_num == j]),
            thisID$new_time_line[thisID$row_num == j] <- thisID$new_time_line[thisID$row_num == j-1],
            thisID$new_time_line[thisID$row_num == j] <- thisID$new_day_0_date[thisID$row_num == j])
    }}
else{
    thisID$new_time_line[1] <- 0
    }
full_data$new_time_line[full_data$id==i] = thisID$new_time_line 
```

c. available day
<br> reason to create this variable:
<br> This variable represent the cummulative amount of days this patient has been taking the pill within this 6 months period. 
<br>Code:

```
########################### available date #############################
if(length(thisID$row_num)>=2){
  for (j in 2:max(thisID$row_num)){  
    ifelse(thisID$over[thisID$row_num == j] == 0,
       ifelse(thisID$new_time_line[thisID$row_num == j] != thisID$new_time_line[thisID$row_num == j-1],
              thisID$available_day[thisID$row_num == j] <- thisID$PAY_DAY_SUPPLY_CNT[thisID$row_num == j],
              ifelse(thisID$end_day[thisID$row_num == j] < thisID$max_end_date[thisID$row_num == j],
                     thisID$available_day[thisID$row_num == j] <- thisID$available_day[thisID$row_num == j-1],
                     ifelse(thisID$new_day[thisID$row_num == j] > thisID$max_end_date[thisID$row_num == j-1],
                            thisID$available_day[thisID$row_num == j] <- 
                              thisID$available_day[thisID$row_num == j-1] + 
                              thisID$PAY_DAY_SUPPLY_CNT[thisID$row_num == j],
                            thisID$available_day[thisID$row_num == j] <- 
                              thisID$available_day[thisID$row_num == j-1] + 
                              thisID$end_day[thisID$row_num == j]-thisID$max_end_date[thisID$row_num == j-1]))),
       ifelse(thisID$over[thisID$row_num == j] != thisID$over[thisID$row_num == j-1] & thisID$new_day[thisID$row_num == j] < thisID$current_period_end_date[thisID$row_num == j],
              ifelse(thisID$new_day[thisID$row_num == j] > thisID$max_end_date[thisID$row_num == j-1],
                     thisID$available_day[thisID$row_num == j] <- thisID$available_day[thisID$row_num == j-1] + 
thisID$current_period_end_date[thisID$row_num == j]-thisID$new_day[thisID$row_num == j]+1,
                     thisID$available_day[thisID$row_num == j] <- thisID$available_day[thisID$row_num == j-1] + thisID$current_period_end_date[thisID$row_num == j]-thisID$max_end_date[thisID$row_num == j-1]),
              thisID$available_day[thisID$row_num == j] <- NA))
    }}
else{
  thisID$available_day[1] <- thisID$PAY_DAY_SUPPLY_CNT[1]
    }
full_data$available_day[full_data$id==i] = thisID$available_day 
```

* **label patients** <br>
Use the available day we calculated to label patients. If the patient has been taking pills for more the 90% of the time during the 6 months period, then this patient is defined as Long Term Opioid Therapy (LTOT). 

2. **Building the model**<br>
a. We seperate the whole data as 80% of it become trainng dataset and 20% of it become validation dataset.<br>
b. We defined function getDetailRMSE to help evaluete the model by compare the RMSE got from each model.

```
################################################# predictive model ########################################################

isTraining = runif(nrow(data4))<.8  
detailTrain = subset(data4,isTraining)
detailValid = subset(data4,!isTraining)

#Function returns the RMSE using the validation set for the detailing data
getDetailRMSE = function(model){
  actualY = detailValid$LTOT
  predictedY = predict(model,detailValid)
  return(mean((actualY-predictedY)^2)^.5)
}
```
c. the best model we picked is a random forest model.

```
###################### best model selected  #########################
library('randomForest')
RandomForest_model6 <- randomForest(LTOT~ days+PAY_DAY_SUPPLY_CNT + PAYABLE_QTY + MME + QTY_PER_DAY + days:PAY_DAY_SUPPLY_CNT + days:MME + days:QTY_PER_DAY + PAY_DAY_SUPPLY_CNT:PAYABLE_QTY  + PAYABLE_QTY:MME + PAYABLE_QTY:QTY_PER_DAY,data = data4, mtry=5.25) 
```
d. use the model we selected, we made the prediction and substract all patient that are eligable for LTOT
```
###################### prediction  #########################
result_table <- data.frame(ID = c(1:5984),predicted_value = c(1:5984))
for (i in unique(HMAHCC_HOLDOUT$ID)) {
  thisID = subset(detailValid,ID==i)
  thisID$predicted_value <- predict(randomForest(LTOT~ days+PAY_DAY_SUPPLY_CNT + PAYABLE_QTY + MME + DRUG_TYPE + EVENT_DESCR + QTY_PER_DAY + days:PAY_DAY_SUPPLY_CNT + days:MME + days:QTY_PER_DAY + PAY_DAY_SUPPLY_CNT:PAYABLE_QTY  + PAYABLE_QTY:MME + PAYABLE_QTY:QTY_PER_DAY,data = data4, mtry=5.25),thisID)
  detailValid$predicted_value[detailValid$ID==i] = thisID$predicted_value
}
```
