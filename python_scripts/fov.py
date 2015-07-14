from math import sqrt
# fov in degress, from Logitech C615
diagonalFOV = 74

# in pixels
cameraWidth = 1280
cameraHeight = 720
cameraDiagonal = sqrt((cameraHeight * cameraHeight) + (cameraWidth * cameraWidth))

# we want horizontal and vertical fov
horizontalFOV = (diagonalFOV*cameraWidth)/cameraDiagonal
verticalFOV = (diagonalFOV*cameraHeight)/cameraDiagonal

print 'h:', horizontalFOV, ', v:', verticalFOV
print 'h:', int(horizontalFOV), ', v:', int(verticalFOV)