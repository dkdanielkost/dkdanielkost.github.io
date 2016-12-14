
# coding: utf-8

# In[ ]:

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

