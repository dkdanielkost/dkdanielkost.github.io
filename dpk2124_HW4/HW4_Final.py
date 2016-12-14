import numpy as np
import pandas as pd
from numpy import genfromtxt, linalg
import matplotlib.pyplot as plt

my_data = genfromtxt('ratings_fake.csv', delimiter=',')
dat = np.reshape(my_data[:,2],(200,200))
my_data2 = pd.read_csv('ratings_train.csv', delimiter=',', header=-1)
my_data3 = pd.read_csv('ratings_test.csv', delimiter=',', header=-1)

dims = list(my_data2.max(axis=0))
dat2 = np.zeros((dims[0],dims[1]))
for i in list(my_data2.index):
    row = my_data2.ix[i,0]
    col = my_data2.ix[i,1]
    dat2[row-1,col-1] = my_data2.ix[i,2]

dims = list(my_data3.max(axis=0))
dat3 = np.zeros((dims[0],dims[1]))
for i in list(my_data3.index):
    row = my_data3.ix[i,0]
    col = my_data3.ix[i,1]
    dat3[row-1,col-1] = my_data3.ix[i,2]


def update_b (rdat,usr,ind):
    # exctract nonzero ratings
    nzinds = np.asarray(np.nonzero(rdat[ind])).flatten()
    a_ij = rdat[ind,nzinds]
    #mu = my_data2.ix[:,2].sum()/len(my_data2.index)
    mu = np.mean(rdat)
    c_j = c[nzinds]
    dp = np.dot(usr[ind], v[nzinds].T)
    return ((a_ij-dp-c_j-mu)/(len(nzinds))).sum()


def update_u2 (rdat,b,ind):
    nzinds = np.asarray(np.nonzero(rdat[ind])).flatten()
    a_ij = rdat[ind,nzinds]
    b_i = b[ind]
    #mu = my_data2.ix[:,2].sum()/len(my_data2.index)
    mu = np.mean(rdat)
    V = np.dot(v[nzinds].T, v[nzinds])
    A = np.dot((a_ij-b_i-c[nzinds]-mu),v[nzinds])
    return np.dot(A, np.linalg.inv(V+(2*lam)*np.identity(k)))


def update_c (rdat,mov,ind):
    nzinds = np.asarray(np.nonzero(rdat[:,ind])).flatten()
    a_ij = rdat[nzinds,ind]
    #mu = my_data2.ix[:,2].sum()/len(my_data2.index)
    mu = np.mean(rdat)
    b_i = b[nzinds]
    dp = np.dot(mov[ind], u[nzinds].T)
    return ((a_ij-dp-b_i-mu)/(len(nzinds))).sum()


def update_v2 (rdat,c,ind):
    nzinds = np.asarray(np.nonzero(rdat[:,ind])).flatten()
    a_ij = rdat[nzinds,ind]
    c_j = c[ind]
    #mu = my_data2.ix[:,2].sum()/len(my_data2.index)
    mu = np.mean(rdat)
    U = np.dot(u[nzinds].T, u[nzinds])
    A = np.dot((a_ij-b[nzinds]-c_j-mu),u[nzinds])
    return np.dot(A, np.linalg.inv(U+(2*lam)*np.identity(k)))


def clip(n):
    if n<1:
        return 1
    elif n>5:
        return 5
    else:
        return n    


def rmse():
    mse = 0
    for i in list(my_data3.index):
        row = my_data3.ix[i,0]-1
        col = my_data3.ix[i,1]-1
        a_ij = my_data3.ix[i,2]
        mse += (np.square(clip(np.dot(u[row],v[col].T)+b[row]+c[col]+mu)-a_ij))
    return(np.sqrt(mse/20000))


# Alternating LS 3
# n = 200 and m = 200 here
k=10
lam=10
T=40
np.random.seed(1)
# initialize b and c to zero
b = np.zeros((dat2.shape[0],))
c = np.zeros((dat2.shape[1],))
u = []
v = []
log_like=[]
test_rmse=[]

cov = (1/k)*np.identity(k)
mean = np.zeros((k,))

for i in range(len(b)):
    u.append(np.random.multivariate_normal(mean, cov))
for j in range(len(c)):
    v.append(np.random.multivariate_normal(mean, cov))

u = np.asarray(u)
v = np.asarray(v)
#mu = my_data2.ix[:,2].sum()/len(my_data2.index)
mu = np.mean(dat2)

for t in range(T):
    for i in range(len(b)):
        if np.sum(dat2[i,]) > 0:
            b[i] = update_b(dat2,u,i)
    for i in range(len(b)):
        if np.sum(dat2[i,]) > 0:
            u[i] = update_u2(dat2,b,i)
    for j in range(len(c)):
        if np.sum(dat2[:,j]) > 0:
            c[j] = update_c(dat2,v,j)
    for j in range(len(c)):
        if np.sum(dat2[:,j]) > 0:
            v[j] = update_v2(dat2,c,j)
    tot = 0
    for i in range(len(b)):
        nzinds = np.asarray(np.nonzero(dat2[i])).flatten()
        tot += (np.square(np.dot(u[i],v[nzinds].T)+b[i]+c[nzinds]+mu-dat2[i,nzinds]).sum())
    norm_u = np.square(np.linalg.norm(u,2,axis=1)).sum()
    norm_v = np.square(np.linalg.norm(v,2,axis=1)).sum()
    tot = ((-0.5)*tot)-lam*(norm_u+norm_v)
    test_rmse.append(rmse())
    log_like.append(tot)


x=np.asarray(range(40))
x+=1
y=np.asarray(test_rmse)
plt.plot(x,y)
plt.xlabel('Iteration')
plt.ylabel('Test RMSE')
plt.title('Test RMSE as a function of Iteration number')
plt.show()


x=np.asarray(range(40))
x+=1
y=np.asarray(log_like)
plt.plot(x,y)
plt.xlabel('Iteration')
plt.ylabel('Log Likelihood')
plt.title('Log Likelihood as a function of Iteration number')
plt.gcf().subplots_adjust(left=0.15)
plt.show()

