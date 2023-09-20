Goal:
The goal of collection this dataset is to analyze it using classification and clustering on the input parameters like gender, age, various diseases, and smoking status in order to predict the likelihood of an individual suffering from a stroke.

Dataset:
https://www.kaggle.com/datasets/fedesoriano/stroke-prediction-dataset

```{r}

data<-read.csv("./Documents/GitHub/Stroke-Prediction/Dataset/healthcare-dataset-stroke-data.csv")
```

Genral info about the dataset:
Among the 5110 objects in our dataset sample, 12 attributes are used to describe them. Our characteristics' values are utilized to identify their types, such as the nominal for id, binary for gender, and numeric for age.
Additionally, we had two attributes for hypertension and heart disease that took two values 1 and 0 to indicate whether they are sufferd from it or not, respectively. The last attribute, "stroke", was described by two values 0 and 1 for the possibility of having a stroke or not as a result of analysis of the previous data, which is what we aim to train our model to predict.

Column name and description:

1- Attribute Name: id
Description: Unique id of the patient
Data Type: Nominal
Possible values: Range between 67-72940

2- Attribute Name: gender
Description: Gender of the patient
Data Type: Binary
Possible values: Female, Male

3- Attribute Name: age
Description: Age of the patient
Data Type: Numeric
Possible values: Range between 0.08-82

4- Attribute Name: hypertension
Description: Hypertension binary feature, 1 means the patient has hypertension, 0 means they do not.
Data Type: Binary
Possible values: 0,1

5- Attribute Name: heart_disease
Description: Heart disease binary feature, 1 means the patient has heart disease, 0 means they do not.
Data Type: Binary
Possible values: 0,1

6- Attribute Name: ever_married
Description: Has the patient ever been married?
Data Type: Binary
Possible values: Yes, No

7- Attribute Name: work_type
Description: Work type of the patient
Data Type: Nominal
Possible values: "Private", "Self-employed", "children", "Govt_job", "Never_worked"

8- Attribute Name: residence_type
Description: Residence type of the patient
Data Type: Binary
Possible values: "Urban", "Rural"

9- Attribute Name: avg_glucose_level
Description: Average glucose level in blood
Data Type: Numeric
Possible values: Range between 55.1-272

10- Attribute Name: bmi
Description: Body Mass Index
Data Type: Numeric
Possible values: Range in [28.7, 28.4, 27.7, 26.7]

11- Attribute Name: smoking_status
Description: Smoking status of the patient
Data Type: Nominal
Possible values: "never smoked", "Unknown", "formerly smoked", "smokes"

12- Attribute Name: stroke
Description: Stroke event, 1 means the patient had a stroke, 0 means not
Data Type: Nominal
Possible values: "never smoked", "Unknown", "formerly smoked", "smokes"

```{r}
str(data)
```

