"""

	Broadcast dummy Pupil timestamps

"""	
 
import zmq
#from ctypes import create_string_buffer
from time import sleep

def test_msg():
	test_msg = "Pupil\ntimestamp:1389761135.56\n"
	return test_msg 
 
def main():
	context = zmq.Context()
	socket = context.socket(zmq.PUB)
	#address = create_string_buffer("tcp://127.0.0.1:5020",512)
	address = "tcp://127.0.0.1:5020"
	try:
		#socket.bind(address.value)
		socket.bind(address)
	except zmq.ZMQError:
		print "Could not set Socket."
 
	for i in range(60000):
		socket.send( test_msg() )
		sleep(0.01)
 
	context.destroy()
 
if __name__ == '__main__':
	main() 