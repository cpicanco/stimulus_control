import math

# http://en.wikipedia.org/wiki/Visual_angle
# distances in cm

# object size
S = 70.0

# how far distance
D = 260.0

# visual angle
V = 2 * math.atan( S/(D*2) ) 

print 'Radians:', V
print 'Degrees:', math.degrees(V)