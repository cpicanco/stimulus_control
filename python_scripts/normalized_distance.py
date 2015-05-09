import numpy as np
import matplotlib.pyplot as plt

# possible performance update
# http://codereview.stackexchange.com/questions/52218/possible-optimizations-for-calculating-squared-euclidean-distance

# resolution
width = 1280
height = 768

# scale
SD = 50.0

def get_x_y(array_of_tuples):
	X = [array_of_tuples[point][0] for point in range(len(array_of_tuples))]
	Y = [array_of_tuples[point][1] for point in range(len(array_of_tuples))]
	return X, Y

def random_point(x, y):
	"""
	x, y: float
	
	"""
	rx = np.random.normal(x, SD)
	ry = np.random.normal(y, SD)
	return (rx, ry)

def euclidean_distance(A1, A2):
	"""
	A, B: 1d array of tuples (x, y)

	"""
	Xa, Ya = get_x_y(A1)
	Xb, Yb = get_x_y(A2)
	Xba = np.subtract(Xb, Xa)
	Yba = np.subtract(Yb, Ya)
	return np.sqrt(np.add(np.multiply(Xba, Xba), np.multiply(Yba, Yba)))

def normalized_distance(dAP, dAB):
	"""
	dAB, dAP: 1d array of euclidean_distance
	"""
	return np.divide(dAP, dAB)

# number of random points
n = 10

# gaze points
pP = [ random_point( float(width/2) + (float(width/3)/n) *p, float(height/2)) for p in range(n) ]

# circle 1
pA = [ random_point( float(width/2) - float(width/3), float(height/2) ) for _ in range(n) ]

# circle 2
pB = [ random_point( float(width/2) + float(width/3), float(height/2) ) for _ in range(n) ]

# random source points
#X, Y = get_x_y(pA)
#plt.plot(X, Y, 'r.')

#X, Y = get_x_y(pB)
#plt.plot(X, Y, 'b.')

#X, Y = get_x_y(pP)
#plt.plot(X, Y, 'k.')

#plt.axis([0, width, 0, height])
#plt.show()

norm_dist = normalized_distance(euclidean_distance(pA, pP), euclidean_distance(pA, pB))

plt.plot(norm_dist, 'k.')
plt.axis([0, n, 0, 1])
plt.show()
