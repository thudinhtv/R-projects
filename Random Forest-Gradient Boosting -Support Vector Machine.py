#Read in library
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score
from sklearn.metrics import classification_report
from sklearn.feature_extraction.image import grid_to_graph
from sklearn import tree
from sklearn.ensemble import RandomForestRegressor
from sklearn.externals.six import StringIO
from IPython.display import Image, display
import pydotplus

from sklearn.linear_model import LogisticRegression


# Importing Dataset
file_name = 'Path\\DATA\\Enrollment_Data.csv' 
enrollment_data = pd.read_csv(file_name, sep= ',')

# Exploratory analysis:
enrollment_data.shape
enrollment_data.head()
enrollment_data.dtypes
enrollment_data.isna().sum()

# Drop CAMPUS_VISIT_2, IRSCHOOL, LEVEL_YEAR
enrollment_data.drop('CAMPUS_VISIT_2', axis=1, inplace=True)
enrollment_data.drop('IRSCHOOL', axis=1, inplace=True)
enrollment_data.drop('LEVEL_YEAR', axis=1, inplace=True)

# Impute avg_income, CONTACT_CODE1, distance, ETHNICITY, satscore, sex, telecq
## List unique values
enrollment_data.avg_income.unique()
enrollment_data.distance.unique()
enrollment_data.ETHNICITY.unique()
enrollment_data.satscore.unique()
enrollment_data.sex.unique()
enrollment_data.telecq.unique()

## fill missing values of categorical variables with with most frequent values
enrollment_data.CONTACT_CODE1.unique()
enrollment_data.CONTACT_CODE1.fillna(enrollment_data.CONTACT_CODE1.value_counts().index[0], inplace=True)
enrollment_data.ETHNICITY.unique()
enrollment_data.ETHNICITY.fillna(enrollment_data.ETHNICITY.value_counts().index[0], inplace=True)
list(enrollment_data.sex.unique())
enrollment_data.sex.fillna(enrollment_data.sex.value_counts().index[0], inplace=True)

## fill missing values of numerical variables with mean
avg_income_mean = round(enrollment_data['avg_income'].mean(), 1)
enrollment_data['avg_income'].fillna(avg_income_mean, inplace=True)
distance_mean = round(enrollment_data['distance'].mean(), 1)
enrollment_data['distance'].fillna(distance_mean, inplace=True)
satscore_mean = round(enrollment_data['satscore'].mean())
enrollment_data['satscore'].fillna(satscore_mean, inplace=True)
telecq_mean = round(enrollment_data['telecq'].mean(), 1)
enrollment_data['telecq'].fillna(telecq_mean, inplace=True)

'''
# Create dummy variables for certain categorical variables
for column in ['CONTACT_CODE1', 'ETHNICITY']:
    dummies = pd.get_dummies(enrollment_data[column], prefix=column)
    enrollment_data[dummies.columns] = dummies
    enrollment_data.drop(column, axis=1, inplace=True)
'''
# Convert string to number
for column in ['CONTACT_CODE1', 'ETHNICITY']:
    var = list(enrollment_data[column].unique())
    enrollment_data[column].replace(var,[i for i in range(len(var))], inplace=True)
enrollment_data['Instate'].replace(['N', 'Y'], [0,1], inplace=True)
enrollment_data['TERRITORY'].replace(['A', 'N'], ['9','10'], inplace=True)
enrollment_data['TERRITORY'] = pd.to_numeric(enrollment_data['TERRITORY'])
enrollment_data['Contact_Month'].replace(['Jan', 'Feb',  'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov','Dec'], [1, 2, 3, 4, 5, 6, 7, 8, 9,10, 11, 12], inplace=True)
enrollment_data.dtypes

# Seperating the target variable
Y = enrollment_data['Target_Enroll']
X = enrollment_data.drop(columns =['Target_Enroll'])

# Spliting the dataset into train and test
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size = 0.3, random_state = 12345)

# Show the number of records as well as the frequency distribution in both datasets
X_train.shape
Y_train.shape
X_test.shape
Y_test.shape
Y_train.value_counts()

# run the 5 below rows together to create frequency bar chart
train_plot=Y_train.value_counts().plot(kind='bar')
plt.xlabel('School Enrollment')
train_plot.set_xticklabels(['Not enrolled','Enrolled'])
plt.ylabel('Frequency')
plt.title('Frequency Distribution of School Enrollment in Training Data')

Y_test.value_counts()

test_plot=Y_test.value_counts().plot(kind='bar')
plt.xlabel('School Enrollment')
test_plot.set_xticklabels(['Not enrolled','Enrolled'])
plt.ylabel('Frequency')
plt.title('Frequency Distribution of School Enrollment in Validation Data')

# 1. Logistic regression model to Predict Enrollment
logreg = LogisticRegression(penalty='l2', C=1.0, solver='lbfgs', max_iter=1000, multi_class='ovr')
logreg.fit(X_train, Y_train)
Y_pred = logreg.predict(X_test)
print('Accuracy of logistic regression classifier on test set: {:.2f}'.format(logreg.score(X_test, Y_test)))

from sklearn.metrics import confusion_matrix
confusion_matrix = confusion_matrix(Y_test, Y_pred)
print("Confusion Matrix:\n", confusion_matrix)

# 2. Build a decision tree with 2-way splits, using Gini index as splitting criterion
# Maximum depth =4

#Creating the classifier object
clf_gini1 = tree.DecisionTreeClassifier(criterion = "gini",splitter="best", random_state = 12345, max_depth=4, min_samples_leaf=5)

# Performing training
clf1=clf_gini1.fit(X_train, Y_train)

# Visualize the decision tree
col_names = list(X_train.columns.values)
dot_data = StringIO()
tree.export_graphviz(clf1, out_file=dot_data, feature_names=col_names, filled=True,rounded=True)
graph = pydotplus.graph_from_dot_data(dot_data.getvalue())
display(Image(graph.create_png()))

# Prediction on test with giniIndex
Y_pred = clf_gini1.predict(X_test)
print("Predicted values:")
print(Y_pred)

# Calculate accuracy
from sklearn.metrics import confusion_matrix
print("Confusion Matrix:\n", confusion_matrix(Y_test, Y_pred))
print ('Accuracy of decision tree classifier on test set: {:.2f}'.format(accuracy_score(Y_test,Y_pred)))
print("Report : ", classification_report(Y_test, Y_pred))

# 3. Logistic regression with backward elimination
# http://stephacking.com/multivariate-linear-regression-python-step-6-backward-elimination/

## Find the P values for ALL variables
import statsmodels.formula.api as sm
def backwardElimination(x,y, sl):
    numVars = len(x[0])
    for i in range(0, numVars):
        regressor_OLS = sm.Logit(y, x).fit()
        maxVar = max(regressor_OLS.pvalues).astype(float)
        if maxVar > sl:
            for j in range(0, numVars - i):
                if (regressor_OLS.pvalues[j].astype(float) == maxVar):
                    x = np.delete(x, j, 1)
    regressor_OLS.summary()
    return x

SL = 0.05
X3 = X.iloc[:,:].values
numVars = len(X3[0])
Y3 =Y.iloc[:].values
X_opt = X3[:, [0, 1, 2, 3, 4, 5]]

X_Modeled = backwardElimination(X_opt, Y3, SL)
X_train3, X_test3, Y_train3, Y_test3 = train_test_split(X_Modeled, Y3, test_size = 0.3, random_state = 12345)

## SVM
# https://scikit-learn.org/stable/modules/generated/sklearn.svm.SVC.html

from sklearn.svm import SVC
#clf = SVC(kernel='linear'/'rbf'/ 'poly'/ 'sigmoid', gamma = 'auto')

#3.1. SVM RBF (default option for kernel)
clf = SVC(kernel='rbf', gamma = 'auto')
clf.fit(X_train3, Y_train3)
Y_pred3 = clf.predict(X_test3)
print ('Accuracy of SVM RBF: {:.2f}'.format(accuracy_score(Y_test3,Y_pred3)))

from sklearn.metrics import confusion_matrix
confusion_matrix3 = confusion_matrix(Y_test3, Y_pred3)
print(confusion_matrix3)

#3.2. SVM Sigmoid 
clf = SVC(kernel='sigmoid', gamma = 'auto')
clf.fit(X_train3, Y_train3)
Y_pred3 = clf.predict(X_test3)
print ('Accuracy of SVM Sigmoid: {:.2f}'.format(accuracy_score(Y_test3,Y_pred3)))

from sklearn.metrics import confusion_matrix
confusion_matrix3 = confusion_matrix(Y_test3, Y_pred3)
print(confusion_matrix3)

#3.3 & 3.4. SVM Linear and SVM Poly
# NOTE: SVM Linear and SVM Poly took a very long time, seems like non-stopping
# to run, thus here we will only take the SVM model with RBF
# which was previously run to compare with other models

# 4. Random Forest model
# https://scikit-learn.org/stable/modules/generated/sklearn.ensemble.RandomForestClassifier.html

from sklearn.ensemble import RandomForestClassifier

## 4.1.Random Forest with maximum trees = 100, maximum depth =2
rf = RandomForestClassifier(n_estimators=100, max_depth=2, random_state=0)

## Train the model on training data
rf.fit(X_train, Y_train);
Y_pred4 = rf.predict(X_test)

from sklearn.metrics import confusion_matrix
print("Confusion Matrix:\n", confusion_matrix(Y_test, Y_pred4))
print ('Accuracy of Random Forest Classifier on test set: {:.2f}'.format(accuracy_score(Y_test,Y_pred4)))
print("Report : ", classification_report(Y_test, Y_pred4))

## 4.2.Random Forest with maximum trees = 50, maximum depth =3
rf = RandomForestClassifier(n_estimators=50, max_depth=3, random_state=0)

## Train the model on training data
rf.fit(X_train, Y_train);
Y_pred4 = rf.predict(X_test)

from sklearn.metrics import confusion_matrix
print("Confusion Matrix:\n", confusion_matrix(Y_test, Y_pred4))
print ('Accuracy of Random Forest Classifier on test set: {:.2f}'.format(accuracy_score(Y_test,Y_pred4)))
print("Report : ", classification_report(Y_test, Y_pred4))


#5.	Use Gradient Boosting
# https://scikit-learn.org/stable/modules/generated/sklearn.ensemble.GradientBoostingClassifier.html
from sklearn import ensemble
clf = ensemble.GradientBoostingClassifier()
clf.fit(X_train, Y_train)
Y_pred5 = clf.predict(X_test)
from sklearn.metrics import confusion_matrix
print("Confusion Matrix:\n", confusion_matrix(Y_test, Y_pred5))
print ('Accuracy of Gradient Boosting Classifier on test set: {:.2f}'.format(accuracy_score(Y_test,Y_pred5)))
print("Report : ", classification_report(Y_test, Y_pred5))

