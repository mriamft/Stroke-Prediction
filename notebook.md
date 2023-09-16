Goal:
The goal of collection this dataset is to analyze it using classification and clustering on the input parameters like gender, age, various diseases, and smoking status in order to predict the likelihood of an individual suffering from a stroke.

Dataset:
https://www.kaggle.com/datasets/fedesoriano/stroke-prediction-dataset

```{r}
data<-read.csv("healthcare-dataset-stroke-data.csv")

```
Genral info about the dataset:
Among the 5110 objects in our dataset sample, 12 attributes are used to describe them. Our characteristics' values are utilized to identify their types, such as the nominal for id, binary for gender, and numeric for age.
Additionally, we had two attributes for hypertension and heart disease that took two values 1 and 0 to indicate whether they are sufferd from it or not, respectively. The last attribute, "stroke", was described by two values 0 and 1 for the possibility of having a stroke or not as a result of analysis of the previous data.

Data
```{r}
str(data)
```
