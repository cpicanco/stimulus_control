"""

    Reveive Pupil timestamps

"""

import sys
import zmq

file_path = sys.argv[1]
index = sys.argv[2]
acode = sys.argv[3]

#file_path = '/home/rafael/git/validation_project/Participante1/Data/Data_001.timestamp'
#index = '0'
#acode = 'Test'

#network setup
context = zmq.Context()
socket = context.socket(zmq.SUB)
socket.connect("tcp://127.0.0.1:5000")

socket.setsockopt(zmq.SUBSCRIBE, '')

#while True:
for i in xrange(0, 1, 1):
    msg = socket.recv()

    
    items = msg.split("\n") 
    msg_type = items.pop(0)
    items = dict([i.split(':') for i in items[:-1] ])

    if msg_type == 'Pupil':
        try:
            with open(file_path, 'a') as timestamps:
                timestamps.write(str((index, items['timestamp'], acode)) + "\n")
            timestamps.close()
            #print "norm_gaze: ", items['norm_gaze']

        except KeyError:
            print 'Error'
    else:
        # process non gaze position events from plugins here
        print 'no Pupil messages'