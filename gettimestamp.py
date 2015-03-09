'''
(*)~----------------------------------------------------------------------------------

 Validation Project (PCRF) - Stimulus Control App
 Copyright (C) 2014,  Carlos Rafael Fernandes Picanco, cpicanco@ufpa.br

 Distributed under the terms of the CC BY-NC-SA License.
 License details are in [http://creativecommons.org/licenses/by-nc-sa/3.0/].

----------------------------------------------------------------------------------~(*)
'''

import sys
import zmq

file_path = sys.argv[1]
# print sys.argv[1]
index = sys.argv[2]
# print sys.argv[2]
acode = sys.argv[3]

# index = '0'
# acode = 'Teste'

#network setup
context = zmq.Context()
socket = context.socket(zmq.SUB)
socket.connect("tcp://10.42.0.43:5500")
#filter messages starting with 'STRING'. '' receives all messages
socket.setsockopt(zmq.SUBSCRIBE, '')

#Time-out
socket.RCVTIMEO = 100

for i in xrange(0, 1, 1):
    msg = socket.recv()
    #print "raw msg:\n", msg

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
            pass
    else:
        # process non gaze position events from plugins here
        pass
    
