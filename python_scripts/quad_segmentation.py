# Source:
# github.com/bsdnoobz/opencv-code/blob/master/quad-segmentation.cpp
import numpy as np

def computeIntersect(a, b):
    """
    a, b: HoughLinesP line [x1, y1, x2, y2]
    """
    x1, y1, x2, y2 = a[0], a[1], a[2], a[3] 
    x3, y3, x4, y4 = b[0], b[1], b[2], b[3]

    d = float(((x1 - x3) * (y3 - y4)) - ((y1 - y2) * (x3 -x4)))

    if  d != 0.0:
        x = ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)) / d
        y = ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)) / d
        return np.array([x, y])
    else:
        return (-1, -1)

def sortCorners(corners, center):
    """
    corners : list of points 
    center : point
    """
    top = []
    bot = []

    for i in corners:
        if i[1] < center[1]:
            top.append(i)
        else:
            bot.append(i)

    corners = np.zeros(shape=(4,2))

    if (len(top) == 2) and (len(bot) == 2):
        # top left
        if top[0][0] > top[1][0]:
            tl = top[1]
        else:
            tl = top[0]

        # top right
        if top[0][0] > top[1][0]:
            tr = top[0]
        else:
            tr = top[1]

        # botton left
        if bot[0][0] > bot[1][0]:
            bl = bot[1]
        else:
            bl = bot[0]

        # botton right
        if bot[0][0] > bot[1][0]:
            br = bot[0]
        else:
            br = bot[1]

    corners[0] = np.array(tl)
    corners[1] = np.array(tr)
    corners[2] = np.array(br)
    corners[3] = np.array(bl)

    return corners