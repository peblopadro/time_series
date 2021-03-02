# -*- coding: utf-8 -*-
"""
Created on Mon Feb 22 14:54:40 2021

@author: Pedro Martinez
"""
''' Importing Packages '''
import pandas as pd
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
import sklearn
import statsmodels as sm
import seaborn as sns
from datetime import datetime
from sklearn.metrics import (confusion_matrix,accuracy_score, roc_curve, auc, classification_report) 


pip install mlxtend
#import pip; pip.main(['install', 'mlxtend'])

#pip install xgboost
import pip; pip.main(['install', 'xgboost'])
import xgboost as xgb
#conda install -c conda-forge xgboost

import pip; pip.main(['install', 'Shapely'])


''' importing and cleaning data '''
# Loading Data
df = pd.read_csv('C:\\Users\\Pedro Martinez\\Downloads\\lc_loan.csv',low_memory=False)
df.shape
df.info()
# is any borrower data repeated? -i.e. no time series of any borrower
df.shape[0] == df['id'].value_counts().sum()

# Type of Loan Status
df['loan_status'].value_counts(sort=True)

    #choff_df = df[df['loan_status']=='Charged Off']
    #choff_df.head(3)
    #choff_df.info()
    
    #choff_df.isnull().sum()[choff_df.isnull().sum()!=0]

    # extracting columns with No Data
    #null_cols = choff_df.isnull().sum()[choff_df.isnull().sum()!=0]
    #null_cols

# extracting columns with less than half of available data
n_rows , n_cols = df.shape
no_data_cols = df.isnull().sum()[df.isnull().sum() > n_rows*0.50]
no_data_cols

# removing columns with less than half the data
df_clean = df.drop(no_data_cols.index , axis=1)
df_clean.info()
df_clean.head(1)

del df

## creating a binary column for Charged-Off loans ##
a = pd.Series(np.where( (df_clean['loan_status'] == 'Charged Off') , 1 , 0 ) )
b = pd.Series(np.where( (df_clean['loan_status'] == 'Does not meet the credit policy. Status:Charged Off') , 1 , 0 ) )

df_clean['ChOff'] = a+b

    #df_test['ChOff'] = pd.Series(np.where( (df_clean['loan_status'] != 'Charged Off') & (df_clean['loan_status'] == 'Does not meet the credit policy. Status:Charged Off'), 0 , 1 ))
    #( (df_clean['loan_status'] == 'Charged Off') | (df_clean['loan_status'] == 'Does not meet the credit policy. Status:Charged Off') ).sum()
    #( (df_clean['loan_status'] != 'Charged Off') | (df_clean['loan_status'] != 'Does not meet the credit policy. Status:Charged Off') ).sum()
    
df = df_clean; del df_clean

n_rows, n_cols  = df.shape

df_num = df.select_dtypes(exclude='object')
df_cat = df.select_dtypes(exclude=['float64','int64'])

# Total Number of Charge-Offs #
total = (df['ChOff']==1).sum()
df_choff = df[df['ChOff']==1]



''' Exploratory Data Analysis '''
plt.style.use('ggplot')
#mpl.rcParams['agg.path.chunksize'] = 10000

pd.crosstab(df['home_ownership'],df['ChOff'],values=df['int_rate'],aggfunc='mean').round(1)
pd.crosstab(df['home_ownership'],df['ChOff'],values=df['int_rate'],aggfunc='median').round(1)

(df.groupby('addr_state')['ChOff'].sum()).sort_values(ascending=False) # total count
(df.groupby('addr_state')['ChOff'].sum()/total*100).sort_values(ascending=False) # in pct of total count
choff_state = (df.groupby('addr_state')['ChOff'].sum()).sort_values(ascending=False)
choff_state[:10].plot(kind='bar',title='Top 10 states with Charged-Off Loans'); plt.ylabel('count'); plt.xlabel('state') ; plt.show()
(choff_state/total*100)[:10].plot(kind='bar',title='Top 10 states with Charged-Off Loans (in % of Totals)'); plt.ylabel('% of Total Charged-Off Loans'); plt.xlabel('state') ; plt.show()

sns.countplot(x='ChOff', data=df, title='test')
sns.countplot(x='ChOff', hue='grade', data=df.sort_values('grade')).set_title('Charged-Off Loans by Grade')
sns.countplot(x='ChOff', hue='grade', data=df_choff.sort_values('grade'))
sns.countplot(x='ChOff', hue='home_ownership', data=df); plt.title('Charged-Off Loans by Home Ownership'); plt.legend(loc='upper right')
sns.countplot(x='ChOff', hue='purpose', data=df.sort_values('purpose')); plt.legend(bbox_to_anchor=(1.05, 1.0), loc='upper left'); plt.title('Charged-Off Loans by Purpose')
sns.countplot(x='ChOff', hue='delinq_2yrs', data=df.sort_values('delinq_2yrs')); plt.legend(bbox_to_anchor=(1.05, 1.0), loc='upper left'); plt.title('Charged-Off Loans by Delinquencies in the last 2 years')

sns.pairplot(data=df, y_vars=['ChOff'], x_vars=['annual_inc'],hue='grade'); plt.title('Charged-Off Loans by Annual Income'); plt.xlabel('Annual Income \n (in $100Ks)')
sns.pairplot(data=df[df['dti']<50], y_vars=['ChOff'], x_vars=['dti'],hue='home_ownership'); plt.title('Charged-Off Loans by Debt-to-Income'); plt.xlabel('Debt-to-Income')
sns.pairplot(data=df[df['dti']<50], y_vars=['ChOff'], x_vars=['dti'],hue='grade'); plt.title('Charged-Off Loans by Debt-to-Income'); plt.xlabel('Debt-to-Income')
sns.pairplot(data=df[df['dti']<50], y_vars=['ChOff'], x_vars=['dti'],hue='delinq_2yrs'); plt.title('Charged-Off Loans by Debt-to-Income'); plt.xlabel('Debt-to-Income')
sns.pairplot(data=df[df['dti']<50], y_vars=['ChOff'], x_vars=['emp_length'],hue='grade')


sns.boxplot('ChOff','int_rate',data=df); plt.title('boxplot on Interest Rates by Charged-Off Loans'); plt.ylabel('Interest Rate(%)')
sns.boxplot('ChOff','dti',data=df[df['dti']<50]); plt.title('boxplot on DTI by Charged-Off Loans'); plt.ylabel('Debt-to-Income')
sns.boxplot('ChOff','annual_inc',data=df[df['annual_inc']<2*1e6])


plt.hist('dti',data=df_choff,density=False,rwidth=0.9); plt.xlabel('DTI'); plt.ylabel('count') ; plt.title('histogram of DTI within Charged-Off group') ;plt.show()
plt.hist('delinq_2yrs',data=df_choff,density=False); plt.xlabel('# Deliquencies (last 2 years)'); plt.ylabel('count') ; plt.show()

#plt.plot('ChOff','',data=); plt.show()

#plt.scatter('ChOff','',data=df[df['']<2*1e6]); plt.show()
#plt.scatter('ChOff','dti',data=df_choff); plt.show()
#plt.scatter('total_rev_hi_lim','ChOff',data=df_choff); plt.ylim([0,1]); plt.xlabel(); plt.ylabel('ChOff'); plt.show()


### change to numeric values the Employment History ##
#emp_hist = pd.Series(0,index=df.index)
   for i in df['emp_length'][df['emp_length'].notnull()]:
        splitted = i.split()
        
        if len(splitted[0]) == 3:
            df['emp_hist'] = 15
        elif splitted[0] == '<':
            df['emp_hist'] = 0.5
        elif splitted[0].isdigit():
            df['emp_hist']= int(splitted[0])

### change Last_Credit_Pulled ###
train_time = datetime(2016,2,1,0,0)

for i in df['last_credit_pull_d'][df['last_credit_pull_d'].notnull()]:
     date = datetime.strptime( i , '%b-%Y' )
     delta = train_time - date
     df['time_to_last_credit_pull'] = delta.days


''' Modeling '''
## choosing specific columns ##
#Note: this is to avoid MemoryError while doing One Hot Encoding if otherwise not done
    pre_selected_variables = np.array(['loan_amnt','int_rate','grade','emp_length','home_ownership','annual_inc','purpose','dti','delinq_2yrs','inq_last_6mths','open_acc','pub_rec','total_acc'])
X_train = df[pre_selected_variables] #,'last_credit_pull_d']]

    # One Hot Encoding
    X_train_num = X_train.select_dtypes(exclude='object')
    X_train_cat = X_train.select_dtypes(include='object')
    
    X_train_cat_ohe = pd.get_dummies(X_train_cat)

X_train = pd.concat( [X_train_num , X_train_cat_ohe] , axis =1 )

y_train = df['ChOff']

Train = pd.concat([X_train,y_train] , axis = 1)
Train = Train.dropna()

X_train = Train.drop('ChOff', axis=1)
y_train = Train[['ChOff']]

del [X_train_num, X_train_cat, X_train_cat_ohe]

# remove '<' from colnames for GradiantBoosting
X_train = X_train.rename(columns={'emp_length_< 1 year':'emp_length_less_1 year'})


'''Logistic Regression via Statsmodels ''' 
import statsmodels.api as smlr

## Regularized Logistic ##
logit = smlr.Logit(y_train.astype('float64'),X_train.astype('float64'), method='powell',check_rank=True)
logit_fit = logit.fit_regularized(method='l1',maxiter=500)
    print(logit_fit.summary())
    logit_fit.bic
    logit_fit.params

    # selecting only statistically significant variables
    variables = logit_fit.params[logit_fit.pvalues < 0.05 ]
    variables.index

# re-fitting with significant variables
logit = smlr.Logit(y_train.astype('float64'),X_train[variables.index.values].astype('float64'), check_rank=True)
logit_fit = logit.fit_regularized(method='l1')
    print(logit_fit.summary())
    logit_fit.bic
    statsmodels_logit_fit = pd.DataFrame(logit_fit.params.values,index=logit_fit.params.index.values,columns=['coefficient'])

prob_train_predict = logit_fit.predict(X_train[variables.index.values])
    #prob_train_predict = prob_train_predict.rename('prob_predict',axis=1)
y_train_predict = prob_train_predict.apply(lambda x: 1 if x > 0.5 else 0)
    train_fit = pd.concat([y_train,pd.Series(prob_train_predict.values)], axis=1 )

y_train['ChOff'].value_counts()
y_train_predict.value_counts()

# Confusion Matrix  
print("Confusion Matrix : \n", confusion_matrix(y_train, y_train_predict) )  
 # Accuracy Score
print('Test accuracy = ', accuracy_score(y_train, y_train_predict))
# Classification Report
print(classification_report(y_train, y_train_predict,target_names=['Non-ChOff', 'ChOff']))


''' Logistic Regression via ScikitLearn '''
from sklearn.linear_model import LogisticRegression

lr = LogisticRegression(penalty='l1',fit_intercept=False,solver='liblinear',C = 1e9)
lr_fit = lr.fit(X_train[variables.index.values] , np.ravel(y_train)) #np.ravel(X_train.columns))
    lr_fit.get_params()
    lr.coef_
    sklear_log_fit = pd.DataFrame(lr.coef_[0],index=variables.index.values,columns=['coefficient'])
    sklear_log_fit
    
    prob_train_predict_sklearn = lr_fit.predict_proba(X_train[variables.index.values])
    y_train_predict_sklearn = pd.Series(prob_train_predict_sklearn[:,1]).apply(lambda x: 1 if x > 0.5 else 0)
        
    # Confusion Matrix  
    print("Confusion Matrix : \n", confusion_matrix(y_train, y_train_predict_sklearn) )  
    # Accuracy Score
    print('Test accuracy = ', accuracy_score(y_train, y_train_predict_sklearn))
    # Classification Report
    print(classification_report(y_train, y_train_predict_sklearn,target_names=['Non-ChOff', 'ChOff']))

## Forward Selection ##
from mlxtend.feature_selection import SequentialFeatureSelector as SFS
logit_sfs = SFS(lr,k_features=13,forward=True,floating=False,verbose=1,scoring='accuracy',cv=0)
logit_sfs_fit = logit_sfs.fit(X_train, np.ravel(y_train))
    sfs_features = np.array(logit_sfs_fit.k_feature_names_); print(sfs_features)
    logit_sfs_fit.k_score_
    
    lr_sfs = LogisticRegression(penalty='l1',fit_intercept=False,solver='liblinear',C = 1e9)
    lr_sfs_fit = lr_sfs.fit(X_train[sfs_features] , np.ravel(y_train))
    
    y_train_predict_sklearn_sfs = lr_sfs_fit.predict(X_train[sfs_features])
    #y_train_predict_sklearn_sfs = pd.Series(prob_train_predict_sklearn_sfs[:,1]).apply(lambda x: 1 if x > 0.5 else 0)
        
    # Confusion Matrix  
    print("Confusion Matrix : \n", confusion_matrix(y_train, y_train_predict_sklearn_sfs) )  
    # Accuracy Score
    print('Train accuracy = ', accuracy_score(y_train, y_train_predict_sklearn_sfs))
    # Classification Report
    print(classification_report(y_train, y_train_predict_sklearn_sfs,target_names=['Non-ChOff', 'ChOff']))
    
    
# comparing sklearn vs statsmodels fits
pd.concat([sklear_log_fit,statsmodels_logit_fit],axis=1)

### Cross Validation ###
from sklearn.model_selection import cross_val_score
cv_results = cross_val_score(lr, X_train[variables.index.values], np.ravel(y_train), cv=10)
    print(cv_results)
    cv_results.mean()

    #### cross_val_predict
    lr = linear_model.LinearRegression()
    X, y = datasets.load_diabetes(return_X_y=True)
    
    # cross_val_predict returns an array of the same size as `y` where each entry
    # is a prediction obtained by cross validation:
    predicted = cross_val_predict(lr, X, y, cv=10)
    #####



''' Gradiant Boosting '''
gb = xgb.XGBClassifier()
gb_fit = gb.fit(X_train,np.ravel(y_train))
    gb_fit.get_params()
    gb_feature_importances = pd.DataFrame(gbr_fit.feature_importances_, index=variables.index.values)
    gb_feature_importances; gbr_feature_importances.sum()

prob_train_predict_gb = gb_fit.predict_proba(X_train[variables.index.values]) # vs gb_variables
y_train_predict_gb = pd.Series(prob_train_predict_gb[:,1]).apply(lambda x: 1 if x > 0.5 else 0)
#y_train_predict_gb = gb_fit.predict(X_train[variables.index.values])

    # Confusion Matrix  
    print("Confusion Matrix : \n", confusion_matrix(y_train, y_train_predict_gb) )  
    # Accuracy Score
    print('Train accuracy = ', accuracy_score(y_train, y_train_predict_gb))
    # Classification Report
    print(classification_report(y_train, y_train_predict_gb,target_names=['Non-ChOff', 'ChOff']))
 
    
    ## Gradiant Boosting Feature Selection 
    gb_weight = gb_fit.get_booster().get_score(importance_type = 'weight')
        gb_weight = pd.DataFrame.from_dict(gb_weight,orient='index')
        gb_weight = gb_weight.sort_values(by=[0],ascending=False)
        plt.plot(gb_weight[0],kind='bar')
    
    xgb.plot_importance(gb_fit, importance_type = 'weight', max_num_features=10); plt.show()
        ## feature selection by Gradient Boosting
        gb_variables = gb_weight[:8].index.values


# Plotting Tree
from xgboost import plot_tree
conda install graphviz python-graphviz

plot_tree( gb_fit, num_trees=1, rankdir='LR')
    #fig = plt.gcf()
    #fig.set_size_inches(150, 100)
    #fig.savefig('tree.png')

    def plot_tree(xgb_model, filename, rankdir='UT'):
        """
        Plot the tree in high resolution
        :param xgb_model: xgboost trained model
        :param filename: the pdf file where this is saved
        :param rankdir: direction of the tree: default Top-Down (UT), accepts:'LR' for left-to-right tree
        :return:
        """
        import xgboost as xgb
        import os
        gvz = xgb.to_graphviz(xgb_model, num_trees=1, rankdir=rankdir)
        _, file_extension = os.path.splitext(filename)
        format = file_extension.strip('.').lower()
        data = gvz.pipe(format=format)
        full_filename = filename
        with open(full_filename, 'wb') as f:
            f.write(data)
        
    plot_tree(gb_fit, 'xgboost_train_tree.pdf')

''' Gradient Boosting via Sklearn '''
from sklearn.ensemble import GradientBoostingRegressor
gbr = GradientBoostingRegressor(max_features='auto',learning_rate=0.01,n_estimators=200)
gbr_fit = gbr.fit(X_train[variables.index.values] , np.ravel(y_train))
    gbr_fit.get_params()
    gbr_feature_importances = pd.DataFrame(gbr_fit.feature_importances_, index=variables.index.values)
    gbr_feature_importances; gbr_feature_importances.sum()

prob_train_predict_gbr = gbr_fit.predict(X_train[variables.index.values])
y_train_predict_gbr = pd.Series(prob_train_predict_gbr).apply(lambda x: 1 if x > 0.5 else 0)

# Confusion Matrix  
print("Confusion Matrix : \n", confusion_matrix(y_train, y_train_predict_gbr) )  
# Accuracy Score
print('Train accuracy = ', accuracy_score(y_train, y_train_predict_gbr))
# Classification Report
print(classification_report(y_train, y_train_predict_gbr,target_names=['Non-ChOff', 'ChOff']))



''' KNN '''
from sklearn.neighbors import KNeighborsClassifier

    ## GridSearchCV
    from sklearn.model_selection import GridSearchCV
    
    param_grid = {'n_neighbors': np.arange(1, 7)}
    knn_cv = GridSearchCV(KNeighborsClassifier() , param_grid, cv=3)
    knn_cv.fit(X_train[variables.index.values], np.ravel(y_train)) # note: long computation time
        knn_cv.best_params_
        knn_cv.best_score_
        
        # output: 6
    
knn = KNeighborsClassifier(n_neighbors=5)
knn_fit = knn.fit( X_train[variables.index.values] , np.ravel(y_train) )
    knn_fit.kneighbors()

prob_train_predict_knn = knn_fit.predict_proba(X_train[variables.index.values])
y_train_predict_knn = knn.predict(X_train[variables.index.values])

    # Confusion Matrix  
    print("Confusion Matrix : \n", confusion_matrix(y_train, y_train_predict_knn) )  
    # Accuracy Score
    print('Train accuracy = ', accuracy_score(y_train, y_train_predict_knn))
    # Classification Report
    print(classification_report(y_train, y_train_predict_knn,target_names=['Non-ChOff', 'ChOff']))


        ### PLotting a pair ###
        from matplotlib.colors import ListedColormap
        from sklearn import neighbors
        
        n_neighbors = 5
        
        # we only take the first two features. We could avoid this ugly
        # slicing by using a two-dim dataset
        X = X_train[variables.index.values[[0,4]]]
        y = y_train
        
        h = .25  # step size in the mesh
        
        # Create color maps
        cmap_light = ListedColormap(['orange', 'cyan', 'cornflowerblue'])
        cmap_bold = ['darkorange', 'c', 'darkblue']
        
        for weights in ['uniform', 'distance']:
            # we create an instance of Neighbours Classifier and fit the data.
            clf = neighbors.KNeighborsClassifier(n_neighbors, weights=weights)
            clf.fit(X, y)
        
            # Plot the decision boundary. For that, we will assign a color to each
            # point in the mesh [x_min, x_max]x[y_min, y_max].
            x_min, x_max = X[:, 0].min() - 1, X[:, 0].max() + 1
            y_min, y_max = X[:, 1].min() - 1, X[:, 1].max() + 1
            xx, yy = np.meshgrid(np.arange(x_min, x_max, h),
                                 np.arange(y_min, y_max, h))
            Z = clf.predict(np.c_[xx.ravel(), yy.ravel()])
        
            # Put the result into a color plot
            Z = Z.reshape(xx.shape)
            plt.figure(figsize=(8, 6))
            plt.contourf(xx, yy, Z, cmap=cmap_light)
        
            # Plot also the training points
            sns.scatterplot(x=X[:,0], y=X[:,1], hue=y_train,
                            palette=cmap_bold, alpha=1.0, edgecolor="black")
            plt.xlim(xx.min(), xx.max())
            plt.ylim(yy.min(), yy.max())
            plt.title("5-Class classification (k = %i, weights = '%s')"
                      % (n_neighbors, weights))
            plt.xlabel((X_train[variables.index.values[[0,4]]]).columns[0])
            plt.ylabel((X_train[variables.index.values[[0,4]]]).columns[1])
        
        plt.show()





'''ROC Curve ''' 
fallout_gbr, sensitivity_gbr, thresholds_gbr = roc_curve(y_train, prob_train_predict_gbr) #
roc_auc_gbr = auc(fallout_gbr, sensitivity_gbr)

fallout_knn, sensitivity_knn, thresholds_knn = roc_curve(y_train, prob_train_predict_knn[:,1])
roc_auc_knn = auc(fallout_knn, sensitivity_knn)

fallout_logit, sensitivity_logit, thresholds_logit = roc_curve(y_train, prob_train_predict_sklearn[:,1]) #
roc_auc_logit = auc(fallout_logit, sensitivity_logit)

    plt.figure()
    lw = 2
    
    plt.plot(fallout_knn, sensitivity_knn, color='darkblue',
             lw=lw, label='ROC curve KNN (area = %0.2f)' % roc_auc_knn)
    
    plt.plot(fallout_gbr, sensitivity_gbr, color='darkorange',
             lw=lw, label='ROC curve XGBoost (area = %0.2f)' % roc_auc_gbr)

    plt.plot(fallout_logit, sensitivity_logit, color='darkgreen',
             lw=lw, label='ROC curve Logit (area = %0.2f)' % roc_auc_logit)    
    
    plt.plot([0, 1], [0, 1], color='navy', lw=lw, linestyle='--')
    plt.xlim([0.0, 1.0])
    plt.ylim([0.0, 1.05])
    plt.xlabel('False Positive Rate')
    plt.ylabel('True Positive Rate')
    plt.title('Receiver operating characteristic')
    plt.legend(loc="lower right")
    plt.show()



''' Test Set '''
Test = pd.read_csv('C:\\Users\\Pedro Martinez\\Downloads\\lc_2016_2017.csv',low_memory=False)
Test.head(1)

## Creating the 'Charged-Off columns ##
    a_test = pd.Series(np.where( (Test['loan_status'] == 'Charged Off') , 1 , 0 ) )
    b_test = pd.Series(np.where( (Test['loan_status'] == 'Does not meet the credit policy. Status:Charged Off') , 1 , 0 ) )
Test['ChOff'] = a+b

Test.info()

### Preparing Data for Test ###
y_test = Test[['ChOff']]
X_test = Test.drop('ChOff', axis=1)
    X_test = X_test[pre_selected_variables]

    X_test_num = X_test.select_dtypes(exclude='object')
    X_test_cat = X_test.select_dtypes(include='object')
    X_test_cat_ohe = pd.get_dummies(X_test_cat)

X_test = pd.concat( [X_test_num , X_test_cat_ohe] , axis =1 )
#y_train = df['ChOff']

Test = pd.concat([X_test,y_test] , axis = 1)
    Test.isnull().sum() # only DTI has small number of null values, though it is not in the models
Test = Test.dropna()
y_test = Test[['ChOff']]

del [X_test_num, X_test_cat, X_test_cat_ohe]


### PREDICTING: REGULARIZED LOGISTIC ###
prob_test_predict_sklearn = lr_fit.predict_proba(Test[variables.index.values])
y_test_predict_sklearn = pd.Series(prob_test_predict_sklearn[:,1]).apply(lambda x: 1 if x > 0.5 else 0)
    
    # Confusion Matrix  
    print("Confusion Matrix : \n", confusion_matrix(y_test, y_test_predict_sklearn) )  
    # Accuracy Score
    print('Test accuracy = ', accuracy_score(y_test, y_test_predict_sklearn))
    # Classification Report
    print(classification_report(y_test, y_test_predict_sklearn,target_names=['Non-ChOff', 'ChOff']))


### PREDICTING: GRADIANT BOOSTING ###
prob_test_predict_gb = gb_fit.predict_proba(Test[variables.index.values]) # vs gb_variables
y_test_predict_gb = pd.Series(prob_test_predict_gb[:,1]).apply(lambda x: 1 if x > 0.5 else 0)

    # Confusion Matrix  
    print("Confusion Matrix : \n", confusion_matrix(y_test, y_test_predict_gb) )  
    # Accuracy Score
    print('Test accuracy = ', accuracy_score(y_test, y_test_predict_gb))
    # Classification Report
    print(classification_report(y_test, y_test_predict_gb,target_names=['Non-ChOff', 'ChOff']))


### PREDICTING: KNN ###
prob_test_predict_knn = knn.predict_proba(Test[variables.index.values])
y_test_predict_knn = knn.predict(Test[variables.index.values])

    # Confusion Matrix  
    print("Confusion Matrix : \n", confusion_matrix(y_test, y_test_predict_knn) )  
    # Accuracy Score
    print('Test accuracy = ', accuracy_score(y_test, y_test_predict_knn))
    # Classification Report
    print(classification_report(y_test, y_test_predict_knn, target_names=['Non-ChOff', 'ChOff']))
    

''' ROC Curve Test set ''' 
fallout_gb_test, sensitivity_gb_test, thresholds_gb_test = roc_curve(y_test, prob_test_predict_gb[:,1]) #
roc_auc_gb_test = auc(fallout_gb_test, sensitivity_gb_test)

fallout_knn_test, sensitivity_knn_test, thresholds_knn_test = roc_curve(y_test, prob_test_predict_knn[:,1])
roc_auc_knn_test = auc(fallout_knn_test, sensitivity_knn_test)

fallout_logit_test, sensitivity_logit_test, thresholds_logit_test = roc_curve(y_test, prob_test_predict_sklearn[:,1]) #
roc_auc_logit_test = auc(fallout_logit_test, sensitivity_logit_test)

    plt.figure()
    lw = 2
    
    plt.plot(fallout_knn_test, sensitivity_knn_test, color='darkblue',
             lw=lw, label='ROC curve KNN (area = %0.2f)' % roc_auc_knn_test)
    
    plt.plot(fallout_gb_test, sensitivity_gb_test, color='darkorange',
             lw=lw, label='ROC curve XGBoost (area = %0.2f)' % roc_auc_gb_test)

    plt.plot(fallout_logit_test, sensitivity_logit_test, color='darkgreen',
             lw=lw, label='ROC curve Logit (area = %0.2f)' % roc_auc_logit_test)    
    
    plt.plot([0, 1], [0, 1], color='navy', lw=lw, linestyle='--')
    plt.xlim([0.0, 1.0])
    plt.ylim([0.0, 1.05])
    plt.xlabel('False Positive Rate')
    plt.ylabel('True Positive Rate')
    plt.title('Receiver operating characteristic (Test set)')
    plt.legend(loc="lower right")
    plt.show()