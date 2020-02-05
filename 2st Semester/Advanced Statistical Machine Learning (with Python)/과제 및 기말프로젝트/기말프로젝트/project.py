import numpy as np
from collections import Counter
from sklearn.datasets import make_classification
from imblearn.over_sampling import SMOTE, ADASYN

X, y = make_classification(n_classes=4, weights=[0.02, 0.05, 0.4, 0.53],n_features=10,
                           n_clusters_per_class=1, n_samples=1000, random_state=10)
print('Original dataset shape %s' % Counter(y))
print('X shape:',X.shape,", y shape:",y.shape)

sm = SMOTE(k_neighbors=5, random_state=1203)
X_res, y_res = sm.fit_resample(X, y)
print('Resampled dataset shape %s' % Counter(y_res))
print('X_res shape:',X_res.shape,", y_res shape:",y_res.shape)

ada=ADASYN(n_neighbors=5, random_state=1203)
X_syn,y_syn=ada.fit_resample(X,y)
print('Resampled dataset shape from ADASYN %s' % Counter(y_syn))
print('X_syn shape:',X_syn.shape,", y_syn shape:",y_syn.shape)

# Data Split --> Train:test = 3:1
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test=train_test_split(X_syn,y_syn,test_size=0.25,random_state=1204)

from sklearn.metrics import f1_score #f1 score
from sklearn.metrics import accuracy_score # accuracy
from sklearn.metrics import auc , roc_curve  # auc
from sklearn.model_selection import GridSearchCV

####################################################################
#1. Logistic Regression
from sklearn.linear_model import LogisticRegression

param_grid_logistic={'C':[1e-4,1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3,1e4]}
logistic_cv=GridSearchCV(LogisticRegression(penalty = 'l2'), param_grid_logistic, cv=5)
logistic_cv.fit(X_train,y_train)
print(logistic_cv.best_params_)

y_fit_logistic_train=logistic_cv.predict(X_train)
y_fit_logistic_test=logistic_cv.predict(X_test)
fpr_logistic_train, tpr_logtistic_train, _ = roc_curve(y_test, y_fit_logistic_test, pos_label=2)
fpr_logistic_test, tpr_logtistic_test, _ = roc_curve(y_test, y_fit_logistic_test, pos_label=2)


print("Train:\n","Accuracy / F1 score / AUC  \n",round(accuracy_score(y_train,y_fit_logistic_train),2),'   /' ,
      round(f1_score(y_train,y_fit_logistic_train,average='macro'),2),'    /' ,round(auc(fpr_logistic_train, tpr_logtistic_train),2))
print("Test:\n","Accuracy / F1 score / AUC  \n",round(accuracy_score(y_test,y_fit_logistic_test),2),'   /' ,
      round(f1_score(y_test,y_fit_logistic_test,average='macro'),2),'    /' ,round(auc(fpr_logistic_test, tpr_logtistic_test),2))

print('Misclassified training samples: %d' %(y_train!=y_fit_logistic_train).sum()) #the number of misclassified train data
print('Misclassified test samples: %d' %(y_test!=y_fit_logistic_test).sum()) #the number of misclassified test data
####################################################################
#2. K-nn
from sklearn.neighbors import KNeighborsClassifier
param_grid_knn={'n_neighbors':[i+1 for i in range(10)] ,
                'p':[i+1 for i in range(10)]}

knn_cv=GridSearchCV(KNeighborsClassifier(), param_grid_knn, cv=5)
knn_cv.fit(X_train,y_train)
print(knn_cv.best_params_)

y_fit_knn_train=knn_cv.predict(X_train)
y_fit_knn_test=knn_cv.predict(X_test)
fpr_knn_train, tpr_logtistic_train, _ = roc_curve(y_test, y_fit_knn_test, pos_label=2)
fpr_knn_test, tpr_logtistic_test, _ = roc_curve(y_test, y_fit_knn_test, pos_label=2)


print("Train:\n","Accuracy / F1 score / AUC  \n",round(accuracy_score(y_train,y_fit_knn_train),2),'   /' ,
      round(f1_score(y_train,y_fit_knn_train,average='macro'),2),'    /' ,round(auc(fpr_knn_train, tpr_logtistic_train),2))
print("Test:\n","Accuracy / F1 score / AUC  \n",round(accuracy_score(y_test,y_fit_knn_test),2),'   /' ,
      round(f1_score(y_test,y_fit_knn_test,average='macro'),2),'    /' ,round(auc(fpr_knn_test, tpr_logtistic_test),2))

print('Misclassified training samples: %d' %(y_train!=y_fit_knn_train).sum())
print('Misclassified test samples: %d' %(y_test!=y_fit_knn_test).sum())
####################################################################
#3. LDA
# Iris data에 대한 LDA 적합
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis

lda= LinearDiscriminantAnalysis(store_covariance=False)
lda.fit(X_train,y_train)

y_fit_lda_train=lda.predict(X_train)
y_fit_lda_test=lda.predict(X_test)
fpr_lda_train, tpr_logtistic_train, _ = roc_curve(y_test, y_fit_lda_test, pos_label=2)
fpr_lda_test, tpr_logtistic_test, _ = roc_curve(y_test, y_fit_lda_test, pos_label=2)


print("Train:\n","Accuracy / F1 score / AUC  \n",round(accuracy_score(y_train,y_fit_lda_train),2),'   /' ,
      round(f1_score(y_train,y_fit_lda_train,average='macro'),2),'    /' ,round(auc(fpr_lda_train, tpr_logtistic_train),2))
print("Test:\n","Accuracy / F1 score / AUC  \n",round(accuracy_score(y_test,y_fit_lda_test),2),'   /' ,
      round(f1_score(y_test,y_fit_lda_test,average='macro'),2),'    /' ,round(auc(fpr_lda_test, tpr_logtistic_test),2))

print('Misclassified training samples: %d' %(y_train!=y_fit_lda_train).sum())
print('Misclassified test samples: %d' %(y_test!=y_fit_lda_test).sum())
####################################################################
#4. Tree
from sklearn.tree import DecisionTreeClassifier
param_grid_tree={'criterion':["gini",'entropy'] ,
                'ccp_alpha':[10**(i+1) for i in range(-7,1)]}

tree_cv=GridSearchCV(DecisionTreeClassifier(random_state=1204), param_grid_tree, cv=5)
tree_cv.fit(X_train,y_train)
print(tree_cv.best_params_)

y_fit_tree_train=tree_cv.predict(X_train)
y_fit_tree_test=tree_cv.predict(X_test)
fpr_tree_train, tpr_logtistic_train, _ = roc_curve(y_test, y_fit_tree_test, pos_label=2)
fpr_tree_test, tpr_logtistic_test, _ = roc_curve(y_test, y_fit_tree_test, pos_label=2)


print("Train:\n","Accuracy / F1 score / AUC  \n",round(accuracy_score(y_train,y_fit_tree_train),2),'   /' ,
      round(f1_score(y_train,y_fit_tree_train,average='macro'),2),'    /' ,round(auc(fpr_tree_train, tpr_logtistic_train),2))
print("Test:\n","Accuracy / F1 score / AUC  \n",round(accuracy_score(y_test,y_fit_tree_test),2),'   /' ,
      round(f1_score(y_test,y_fit_tree_test,average='macro'),2),'    /' ,round(auc(fpr_tree_test, tpr_logtistic_test),2))

print('Misclassified training samples: %d' %(y_train!=y_fit_tree_train).sum())
print('Misclassified test samples: %d' %(y_test!=y_fit_tree_test).sum())
####################################################################
#5. SVM (커널써야할수도 있음.)
from sklearn.svm import SVC
svm=SVC(kernel='linear',C=1.0,random_state=1)
param_grid_svm={'C':[10**(i+1) for i in range(-3,2)] ,
                'gamma':[10**(i+1) for i in range(-3,2)],
                'kernel':['rbf','sigmoid']}

svm_cv=GridSearchCV(SVC(random_state=1206), param_grid_svm, cv=5)
svm_cv.fit(X_train,y_train)
print(svm_cv.best_params_)

y_fit_svm_train=svm_cv.predict(X_train)
y_fit_svm_test=svm_cv.predict(X_test)
fpr_svm_train, tpr_logtistic_train, _ = roc_curve(y_test, y_fit_svm_test, pos_label=2)
fpr_svm_test, tpr_logtistic_test, _ = roc_curve(y_test, y_fit_svm_test, pos_label=2)


print("Train:\n","Accuracy / F1 score / AUC  \n",round(accuracy_score(y_train,y_fit_svm_train),2),'   /' ,
      round(f1_score(y_train,y_fit_svm_train,average='macro'),2),'    /' ,round(auc(fpr_svm_train, tpr_logtistic_train),2))
print("Test:\n","Accuracy / F1 score / AUC  \n",round(accuracy_score(y_test,y_fit_svm_test),2),'   /' ,
      round(f1_score(y_test,y_fit_svm_test,average='macro'),2),'    /' ,round(auc(fpr_svm_test, tpr_logtistic_test),2))

print('Misclassified training samples: %d' %(y_train!=y_fit_svm_train).sum())
print('Misclassified test samples: %d' %(y_test!=y_fit_svm_test).sum())
####################################################################
#6. Random Forest
from sklearn.ensemble import RandomForestClassifier
param_grid_rf={'criterion':["gini","entropy"] ,
               'max_features':['sqrt','log2',X_train.shape[1]]}

rf_cv=GridSearchCV(RandomForestClassifier(random_state=1206), param_grid_rf, cv=5)
rf_cv.fit(X_train,y_train)
print(rf_cv.best_params_)

y_fit_rf_train=rf_cv.predict(X_train)
y_fit_rf_test=rf_cv.predict(X_test)
fpr_rf_train, tpr_logtistic_train, _ = roc_curve(y_test, y_fit_rf_test, pos_label=2)
fpr_rf_test, tpr_logtistic_test, _ = roc_curve(y_test, y_fit_rf_test, pos_label=2)


print("Train:\n","Accuracy / F1 score / AUC  \n",round(accuracy_score(y_train,y_fit_rf_train),2),'   /' ,
      round(f1_score(y_train,y_fit_rf_train,average='macro'),2),'    /' ,round(auc(fpr_rf_train, tpr_logtistic_train),2))
print("Test:\n","Accuracy / F1 score / AUC  \n",round(accuracy_score(y_test,y_fit_rf_test),2),'   /' ,
      round(f1_score(y_test,y_fit_rf_test,average='macro'),2),'    /' ,round(auc(fpr_rf_test, tpr_logtistic_test),2))

print('Misclassified training samples: %d' %(y_train!=y_fit_rf_train).sum())
print('Misclassified test samples: %d' %(y_test!=y_fit_rf_test).sum())
####################################################################
#7. AdaBoost
from sklearn.ensemble import AdaBoostClassifier
adaB = AdaBoostClassifier(DecisionTreeClassifier(max_depth=2), n_estimators=500, random_state=1206)
adaB.fit(X_train,y_train)

y_fit_adaB_train=adaB.predict(X_train)
y_fit_adaB_test=adaB.predict(X_test)
fpr_adaB_train, tpr_logtistic_train, _ = roc_curve(y_test, y_fit_adaB_test, pos_label=2)
fpr_adaB_test, tpr_logtistic_test, _ = roc_curve(y_test, y_fit_adaB_test, pos_label=2)

print("Train:\n","Accuracy / F1 score / AUC  \n",round(accuracy_score(y_train,y_fit_adaB_train),2),'   /' ,
      round(f1_score(y_train,y_fit_adaB_train,average='macro'),2),'    /' ,round(auc(fpr_adaB_train, tpr_logtistic_train),2))
print("Test:\n","Accuracy / F1 score / AUC  \n",round(accuracy_score(y_test,y_fit_adaB_test),2),'   /' ,
      round(f1_score(y_test,y_fit_adaB_test,average='macro'),2),'    /' ,round(auc(fpr_adaB_test, tpr_logtistic_test),2))

print('Misclassified training samples: %d' %(y_train!=y_fit_adaB_train).sum())
print('Misclassified test samples: %d' %(y_test!=y_fit_adaB_test).sum())
####################################################################


