from scipy.io import loadmat
import matplotlib.pyplot as plt
import numpy as np
import random
from sklearn import linear_model, discriminant_analysis
from itertools import combinations_with_replacement
spam = loadmat('C:\\Users\\Daniel\\Desktop\\CU\\Machine Learning\\spam_fixed.mat')
random.seed(25)

def averaged_perceptron(data,labels):
    # Initialize
    w = np.ndarray((data.shape[1],),dtype=np.float)
    avg_wgt = np.ndarray((data.shape[1],),dtype=np.float)
    avg_t = 0.0
    weight_vects = []
    t = []
    b = 0

    # this version appends stores every weight vector then averages end result
    # n sets number of iterations through data

    n = 64
    for j in range(0,n):
        shuff = np.arange(data.shape[0])
        np.random.shuffle(shuff)
        for i in shuff:
            y = labels[i]
            x = data[i]
            res = (y)*(np.dot(w,np.transpose(x)) + b)
            if (res <= 0):
                w = np.add(w,np.multiply(y,x))
                b = b + y
            weight_vects.append(w)
        t.append(b)
        
    # converts list to ndarray
    weight_vects = np.asarray(weight_vects).flatten()
    weight_vects = np.reshape(weight_vects,((n*data.shape[0]),data.shape[1]))
    t = np.asarray(t)
    avg_wgt = np.mean(weight_vects, axis=0)
    avg_t = np.mean(t)
    return {'w':avg_wgt,'t':avg_t}

def perceptron_predictions (data,labels,test):
    # computes dot product of test vector and avg weight vector, equivalent to <w,x> for each test vector.
    # np.sign converts to -1 and +1

    results = averaged_perceptron(data,labels)
    mm = np.dot(test,np.transpose(results['w'])) + results['t']
    preds = np.sign(mm)
    return preds

def dim_extend(data):
    # function that extends dimensions of matrix to required size
    combs = []
    
    # number of possible combinations is N choose 2
    num_combos = int((((data.shape[1])*(data.shape[1]-1))/2) + data.shape[1])
    
    for i in range(0,data.shape[0]):
        temp = combinations_with_replacement(data[i],2)
        for i,j in temp:
            combs.append(i*j)
    # converts list to ndarray
    combs = np.asarray(combs).flatten()
    combs = np.reshape(combs,(data.shape[0],num_combos))
    data_mod = np.append(data,combs,axis=1)
    return data_mod

# logistic regression
def logistic_reg(data,labels,test):
    logreg = linear_model.LogisticRegression(C = 10000000, solver = 'lbfgs')
    logreg.fit(data,labels)
    preds_logreg = logreg.predict(test)
    return preds_logreg

# LDA
def lda(data,labels,test):
    l_discrim = discriminant_analysis.LinearDiscriminantAnalysis(solver = 'eigen')
    l_discrim.fit(data,labels)
    preds_lda = l_discrim.predict(test)
    return preds_lda

# QDA
def qda(data,labels,test):
    q_discrim = discriminant_analysis.QuadraticDiscriminantAnalysis()
    q_discrim.fit(data,labels)
    preds_qda = q_discrim.predict(test)
    return preds_qda

def cross_validation(func,data,labels):
    # Initialize
    
    s1 = set(range(data.shape[0]))
    s2 = set()
    error = []
    
    # Makes 2 sets of data points each iteration: test_ind and train_ind
    for i in range(10):
        n = int((len(s1))*(1/(10-i)))
        test_set = random.sample(s1,n)
        s1 = s1.difference(set(test_set))
        train_set = s1.union(s2)

        train_ind = np.asarray(list(train_set))
        test_ind = np.asarray(list(test_set))
        preds = func(data[train_ind],labels[train_ind],data[test_ind])
        error.append((sum(preds!=labels[test_ind])+0.0)/(len(test_set)))

        s2.update(set(test_set))
    error = np.asarray(error)
    return np.mean(error)

# runs each function through the CV function. Note that this should be run twice for perceptron to give good results.

data = spam['data']
labels = spam['labels'].flatten()
test_data = spam['testdata']
test_labels = spam['testlabels'].flatten()

preds = cross_validation(perceptron_predictions,data,labels)
print ('Averaged Perceptron error:',preds)

preds = cross_validation(logistic_reg,data,labels)
print ('Logistic Regression error:',preds)

preds = cross_validation(lda,data,labels)
print ('Linear Discriminant Analysis error:',preds)

preds = cross_validation(qda,data,labels)
print ('Quadratic Discriminant Analysis error:',preds)

data_2 = dim_extend(data)
test_data_2 = dim_extend(test_data)
preds = cross_validation(perceptron_predictions,data_2,labels)
print ('Perceptron w/ extended features error:',preds)

preds = cross_validation(logistic_reg,data_2,labels)
print ('Logistic Regression w/ extended features error:',preds)


# Lowest rate was logistic regression w/ extended features, train error:
preds = logistic_reg(data_2,labels,data_2)
print ('Logistic Regression w/ extended features training error:',(sum(preds!=labels)+0.0)/(len(labels)))


# test error:
preds = logistic_reg(data_2,labels,test_data_2)
print ('Logistic Regression w/ extended features test set error:',(sum(preds!=test_labels)+0.0)/(len(test_labels)))

