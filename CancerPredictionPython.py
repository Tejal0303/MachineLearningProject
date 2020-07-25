# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

#loading libraries
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
import seaborn as sns
from sklearn.metrics import classification_report as cr, confusion_matrix as cm
import statsmodels.api as sm
import matplotlib.pyplot as plt
import pylab
import statistics as st
from scipy import stats
import scipy.stats
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import classification_report as cr, confusion_matrix as cm
from sklearn.feature_selection import f_classif as fs
from sklearn.feature_selection import RFE
from sklearn import preprocessing # for label encoding
from io import StringIO
from sklearn.linear_model import  LogisticRegression
logmodel=LogisticRegression()
import sklearn.metrics as metrics
from sklearn.ensemble import RandomForestClassifier
regressor=RandomForestClassifier()


#Reading File
cr=pd.read_csv("C:/Users/tc186035/Desktop/DSP21_2/1_PersonalProject/JulyProject/data.csv")

#Data Analysis
cr.head()
cr.shape
cr.isnull().sum()
cr.info()

#Removing Unquie/Identity/Unwanted Columns
cols=["id","Unnamed: 32"]
cr=cr.drop(cols,axis=1)

# Target Variable tred analysis
cr.diagnosis.value_counts()

# Segregation of Independent and Dependent variable's
cr_y=cr.iloc[:,0]
cr_x =cr.iloc[:,1:32]
   
# Coreleation of Dependent Variables 
corrmat=cr.corr()
fig,ax=plt.subplots()
sns.heatmap(corrmat)

def checkcorrelation(dataset,threshold):
                            col_corr=set()
                            cor_matrix=dataset.corr()
                            for i in range(len(cor_matrix.columns)):
                                for j in range(i):
                                    if abs(cor_matrix.iloc[i,j])>threshold:
                                        colname=cor_matrix.columns[i]
                                        col_corr.add(colname)
                            return col_corr

 

checkcorrelation(cr,0.75)

# Copy Data in another variables
cr_rmcorr =cr_x.copy()

cr_rmcorr
#95% correlation coloumn list
colcorr=['area_mean','area_se','area_worst','perimeter_mean','perimeter_se','perimeter_worst',"radius_worst"]

# Removing higly coreleated Columns 
cr_rmcorr=cr_rmcorr.drop(colcorr,axis=1)
cr_rmcorr.shape

# Split Data in train and test
x_train,x_test,y_train,y_test = train_test_split(cr_rmcorr, cr_y, random_state = 20,test_size = 0.3)

# Logistic Model Building (Model:1 with limited Column)
logmodel.fit(x_train.iloc[:,1:12],y_train) 
logmodel.fit(x_train, y_train)

#Predection of Data
y_pred = logmodel.predict(x_test)

# comparing actual response values (y_test) with predicted response values (y_pred)
print("Logistic Regression model accuracy(in %):",metrics.accuracy_score(y_test, y_pred) * 100)
print(metrics.confusion_matrix(y_test, y_pred))

# Random Forest Model Building (Model:1 with limited Column)
m2=RandomForestClassifier(n_estimators = 500, max_depth=8, max_features=3).fit(x_train,y_train)
regressor.fit(x_train,y_train)
# Predection of Data 
y_pred_rf = regressor.predict(x_test)
print("Logistic Regression model accuracy(in %):",metrics.accuracy_score(y_test, y_pred_rf) * 100)
print(metrics.confusion_matrix(y_test, y_pred_rf))

 




