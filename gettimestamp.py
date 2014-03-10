'''
//
// Validation Project (PCRF) - Stimulus Control App
// Copyright (C) 2014,  Carlos Rafael Fernandes Picanço, cpicanco@ufpa.br
//
// This file is part of Validation Project (PCRF).
//
// Validation Project (PCRF) is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Validation Project (PCRF) is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Validation Project (PCRF).  If not, see <http://www.gnu.org/licenses/>.
//

'''

import sys
import zmq

#file_path = '/home/validation_study/timestamps'
#index = 0

file_path = sys.argv[1]
#print sys.argv[1]
index = sys.argv[2]
#print sys.argv[2]
acode = sys.argv[3]

#network setup
context = zmq.Context()
socket = context.socket(zmq.SUB)
socket.connect("tcp://127.0.0.1:5000")
#filter messages starting with 'STRING'. '' receives all messages
socket.setsockopt(zmq.SUBSCRIBE, '')
#socket.RCVTIMEO = 100

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
    
