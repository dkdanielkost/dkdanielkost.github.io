
# coding: utf-8

# In[ ]:

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


# In[ ]:

def perceptron_predictions (data,labels,test):
    # computes dot product of test vector and avg weight vector, equivalent to <w,x> for each test vector.
    # np.sign converts to -1 and +1

    results = averaged_perceptron(data,labels)
    mm = np.dot(test,np.transpose(results['w'])) + results['t']
    preds = np.sign(mm)
    return preds

