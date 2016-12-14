from scipy.io import loadmat
ocr = loadmat('C:\Users\Daniel\Desktop\CU\Machine Learning\ocr.mat')

import matplotlib.pyplot as plt
import numpy as np
import random
from matplotlib import cm

def predictions (X,Y,test):
    preds = []
    X = np.array(X,dtype=np.float)
    test = np.array(test,dtype=np.float)
    Y=Y.flatten()

    # distance calculation
    mat1 = np.array([np.square(X).sum(axis=1),]*(test.shape[0]),dtype=np.float)
    mat2 = np.array([np.square(test).sum(axis=1),]*(X.shape[0]),dtype=np.float).transpose()
    mat = mat1+mat2-2*(np.dot(test,np.transpose(X)))

    # extract index of min in each row
    lbl_ind = np.argmin(mat,axis=1)
    preds = Y[lbl_ind].flatten()
    return preds

# construct matrices

Y = ocr['labels']
test = ocr['testdata'][0:10000]
correct_preds = np.array(ocr['testlabels'][0:10000],dtype=np.float).flatten()
sizes=[1000,2000,4000,8000]
error=[]
total_errors = []

# iterate through sample sizes and take ten of each size

for n in sizes:
    for i in xrange(0,10):
        sel = random.sample(xrange(60000),n)
        X = ocr['data'][sel,:]
        Y = ocr['labels'][sel,:]
        results = predictions(X,Y,test)
        error.append((sum(results!=correct_preds)+0.0)/(10000))
    total_errors.append(error)
    error=[]

# Plot results

means = map(np.mean, total_errors)
std_devs = map(np.std, total_errors)

plt.errorbar(sizes, means, std_devs, linestyle='-', marker='o', linewidth=1.5)
plt.axis([500,8500,0.05,0.12])
plt.ylabel('Error rate')
plt.xlabel('Number of training points')
plt.title('Error rate vs. Number of training points')
plt.show()

print "Mean error for the different sizes:", means
print "Standard deviation for the different sizes:", std_devs

# Parameter generating function using Laplace smoothing to estimate the parameters of each class

def param_gen(X,Y):
    # X is data, Y is labels
    total= X.sum()
    num_lbls = np.unique(Y)
    params = []
    for i in num_lbls:
        lbls = np.where(Y==i)[0]
        cl_words = X[lbls,:].sum(axis=0)
        cl_total = cl_words.sum()
        cl_params = ((cl_words+1)/(cl_total+2))
        params.append(cl_params)
    params = np.asarray(params)
    params = params.flatten()
    params = params.reshape(len(num_lbls),X.shape[1])
    return params

# Takes parameters from Laplace smoothing, calculates probabilities, takes the log then returns the most probable class

def predictions(params,test):
    labels = news['labels']
    test = test.toarray()
    num_lbls = np.unique(labels)
    cl_prior=[]
    preds = []

    for i in num_lbls:
        cl_num = np.where(labels==i)[0]
        x1 = len(cl_num)+0.0
        x2 = labels.shape[0]
        cl_prior.append(x1/x2)
    cl_prior = np.asarray(cl_prior)

    # Beware!! The label 1 class prior probability is stored in index 0 of the cl_prior list!!
    
    log1 = np.log(params)
    log2 = np.log(1-params)
    
    for i in xrange(0,test.shape[0]):
        msg1 = test[i,:].astype(int)
        msg2 = 1-msg1
        prod_mat = np.multiply(log1,msg1)+np.multiply(log2,msg2)
        prod_mat = prod_mat.sum(axis=1)+cl_prior
        preds.append(np.argmax(prod_mat)+1)
    preds = np.asarray(preds)

    return preds

# Runs both functions and prints error rates

news = loadmat('C:\\Users\\Daniel\\Desktop\\CU\\Machine Learning\\news.mat')

parameters = param_gen(news['data'],news['labels'])
test_preds = predictions(parameters,news['testdata'])

test_err = (sum(news['testlabels'].flatten()!=test_preds)+0.0)/(news['testlabels'].shape[0])

train_preds = predictions(parameters,news['data'])

train_err = (sum(news['labels'].flatten()!=train_preds)+0.0)/(news['labels'].shape[0])

print "Training Error:", train_err
print "Test Error", test_err

# Prints 20 most common words for each class

with open('C:\\Users\\Daniel\\Desktop\\CU\\Machine Learning\\news.vocab') as data:
    lines = data.read().splitlines()

parameters = param_gen(news['data'],news['labels'])
words = []

for i in xrange(0,20):
    cat = parameters[i,:]
    cat = np.argpartition(cat,-20)[-20:]
    for j in cat:
        words.append(lines[j])
words = np.asarray(words, dtype=str).reshape(20,20)
print "20 most common words in each class:", words

