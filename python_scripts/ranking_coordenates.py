import numpy as np

def ranking(values, tolerance = 10):
    """
    values: 1d np.array
    """
    aRanking = np.zeros_like(values)
    if len(values) > 1:
        indicesThatSortValues = values.argsort()
        rank = 0
        for i, OldIndex in enumerate(indicesThatSortValues):
            if i > 0:
                proximity = abs(values[indicesThatSortValues[i]] - values[indicesThatSortValues[i - 1]])

                closeToPrevious = (proximity <= tolerance)
                if closeToPrevious:
                    aRanking[OldIndex] = rank
                else:
                    rank += 1
                    aRanking[OldIndex] = rank
    return aRanking

def xyRanking(stims):
    """
    stims: 2d array 
    """
    x = [xy[0] for xy in stims]
    y = [xy[1] for xy in stims]

    x = ranking(np.array(x))
    y = ranking(np.array(y))

    xy = [[x[i], y[i]] for i in range(len(stims))]
    return xy

if __name__ == '__main__':

    valueList = [121, 400, 300, 399, 33, 4, 3, 120, 119, 205]

    print "\nOriginal values:"
    print valueList

    print "\nTheir ranks:"
    print ranking(np.array(valueList))

    stimList = [[121, 200], [119, 402], [118, 590]]

    print "\nOriginal stimuli:"
    print stimList

    print "\nTheir ordinal coordinates:"
    print xyRanking(stimList)

    print "\nTheir codes:"
    print [str(xy[0]) + "-" + str(xy[1]) for xy in xyRanking(stimList)]
