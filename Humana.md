## Humana-Mays 2019 Healthcare Analytics Case Competition 

### Case Description:
Analyze 7 million medical records of 20,000 patients provided by Humana to create a model aim at predict if members will continue opioid therapy six months after initial prescribing. Provide insights and suggestions to help identify members at risk for continued long term use of opioid therapies allowing for early intervention

### Background Information:
Throughout the early 2000s, LTOT for non cancer pain conditions (NCPC) increased considerably without a corresponding increase in incidence of NCPC.<br>
As many as 1 in 4 patients receiving long term opioid therapy in a primary care setting will struggle with opioid disorder <br>
Evidence suggests an elevated risk for overdose, abuse, misuse, and negative health outcomes related to increased dosing or longer duration therapy, including fractures, Myocardial Infarction, and sexual dysfunction.

### Definition:
**Definitions**<br><br>
1. Opioid Naïve <br>
Defined as not having an opioid ‘on hand’ in the preceding 90 day period, based on service date and pay day supply count
2. Long Term Opioid Therapy (LTOT) <br>
Defined as continuous use of an opioid medication with 90% of days covered over a 6 month period

### Procedures:

1. Data prepartion<br>

* **Getting insights from large and un-organized dataset**<br>
Because this case is deeply involved with large amount of medical and healthcare knowledges and backgraounds, what we did first is understand the data deeply and find relationship between variables.<br>
Based on the goal to predict if members will continue opioid therapy six months after initial prescribing, we think backward about possible causation leads to this result.<br>
After understand the logic and variables provided in the dataset, we combined variables to generate new information that will help build the prediction model and clean the data to prepare for building the model <br>

<br><br> Create subset for each ID
```
for(i in unique(full_data$id)){
  thisID = subset(full_data,id==i)
```
<br><br> some of the new variables we created
<br> * 


