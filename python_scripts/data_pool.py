# python
from collections import Sequence
from ast import literal_eval
from collections import Counter
import operator
import os 
import sys
import cv2

# third party 
import numpy as np
import matplotlib.pyplot as plt

# pupil
pupil_base_dir = os.path.abspath(__file__).rsplit('pupil_src', 1)[0]
sys.path.append(os.path.join(pupil_base_dir, 'pupil_src', 'shared_modules'))
from uvc_capture import autoCreateCapture
from methods import denormalize

# self
from vis_circle_on_contours import Vis_Circle_On_Contours
from normalized_distances import euclidean_distance, normalized_distance, get_x_y
from ranking_coordenates import ranking2d

class Global_Container(object):
    pass

#http://stackoverflow.com/questions/6486387/python-implement-list-like-index-access
#class TrialsContainer(object):
#    def __init__(self, length):
#        self.length = length    

class TrialContainer(object):
    def __init__(self):
        self.Angle = None
        self.TargetCircle = None
        self.TimeEvents = []
        self.Timestamps = []
        self.GazePoints = []
        self.CirPoints = []
        self.CirRanking = []
        self.CirAPoints = []
        self.CirBPoints = []

class Data(object):
    """
        Pupil Labs and Stimulus Control App (scapp) data sets and utilities.
    """
    def __init__(self, g_pool):
        self.g_pool = g_pool
        # pupil
        self.capture = self.g_pool.capture

        # 1d array of float
        self.world_timestamps = self.g_pool.world_timestamps
        print 'self.world_timestamps:', type(self.world_timestamps)
        # 1d array of float
        self.eye_timestamps = self.g_pool.eye_timestamps

        # g['timestamp'],g['confidence'],g['norm_pos'][0],g['norm_pos'][1]
        self.gaze_data = self.g_pool.gaze_data

        # p['timestamp'],p['confidence'],p['id'],p['norm_pos'][0],p['norm_pos'][1],p['diameter']
        self.pupil_data = self.g_pool.pupil_data

        # ?
        #self.calibration_points = self.g_pool.calibration_points

        # scapp
        self.load_scapp_output()
        self.load_scapp_report()
        
    def load_scapp_output(self): # timestamps of scapp events
        """
        __________________________________________________________

        dependency: validy self.g_pool.rec_dir + '\scapp_output' path
        __________________________________________________________


        - scapp is an acronymous for Stimulus Control Application
        
        - scapp_output has the following format:
            
            (Trial_No, timestamp, event:session_time)

          where:
            - 'Trial_No' is the trial number as in scapp_report
            - 'timestamp' is the timestamps sent by Pupil Server and received by scapp Client
            - 'event is an scapp event, now there are four types:
                - S  : Starter onset | Trial onset | ITI ending
                - *R : first response after S | Starter ending
                - R  : responses after *R
                - C  : Consequence onset | Trial ending | ITI onset
                - session_time is event occurence time in ms from the session onset.

        > examples:
        > scapp_output 
            ('1', '232.5674', 'S:029367')
            ('1', '232.5675', '*R:029368')
            ('1', '232.5676', 'C:029369')
            ('2', '232.5684', 'S:029377')
            ('2', '232.5685', '*R:029378')
            ('2', '232.5686', 'R:029379')
            ('2', '232.5687', 'C:029380')
     
        > scapp_output loaded
            [  [ ('232.5674', 'S:029367'), ('232.5675', '*R:029368'), ('232.5676', 'C:029369') ],
               [ ('232.5684', 'S:029377'), ('232.5685', '*R:029378'), ('232.5686', 'R:029379'), ('232.5687', 'C:029380') ]  ]

        """
        scapp_output_path = os.path.join(self.g_pool.rec_dir,'scapp_output')      
        self.scapp_output = [[]]
        with open(scapp_output_path, 'r') as scapp_output:
            for line in scapp_output:
                (trial_no, timestamp, event) = literal_eval(line)

                i = int(trial_no)

                if i > len(self.scapp_output):
                    self.scapp_output.append([])
                self.scapp_output[i - 1].append((timestamp, event))

    def load_scapp_report(self):
        """
        __________________________________________________________
        
        dependency: validy self.g_pool.rec_dir + '\scapp_report' path

        report_type: string | 'fpe', 'eos'
        __________________________________________________________

        

           Source Header Names for Feature Positive Effect (fpe) trials:
           
           Trial_No : Trial increment number (unique).              (INT)
           Trial_Id : Trial identification number (can repeat).     (INT)
           TrialNam : Trial String Name.                            (STR)
           ITIBegin : Consequence / Inter Trial Interval onset.     (TIME)
           __ITIEnd : Starter begin / End of Inter Trial Interval   (TIME)
           StartLat : Latency of the starter response.              (TIME)
           StmBegin : Trial stimulus/stimuli onset.                 (TIME)
           _Latency : Latency.                                      (TIME)
           __StmEnd : End of the stimulus/stimuli removal.          (TIME)

           ExpcResp : Expected response / Contingency.              (STR)
                Positiva
                Negativa
           __Result : Type of the Response emmited.                 (STR)
                MISS
                HIT
                NONE
           RespFreq : Number of responses emmited                   (INT)



           Source Header Names for Eye Orientation Study (eos) trials:

           Trial_No : idem
           Trial_Id : idem
           TrialNam : idem
           ITIBegin : idem
           __ITIEnd : idem
           StmBegin : idem
           _Latency : idem
           __StmEnd : idem
           ___Angle : Angle                                          (STR)
                0, 45, 90, 135
           ______X1 : left 1
           ______Y1 : top 1
           ______X2 : left 2
           ______Y2 : top 2
           ExpcResp : Expected response
                0 = no gap/false
                1 = gap/true
           RespFreq : idem

           All time variables are in miliseconds. Counting started
            at the beginning of the session.
        """
        scapp_report_path = os.path.join(self.g_pool.rec_dir,'scapp_report')


        self.scapp_report = np.genfromtxt(scapp_report_path,
            delimiter="\t", missing_values=["NA"], skip_header=6, skip_footer=1,
            filling_values=None, names=True, deletechars='_', autostrip=True,
            dtype=None)

    def get_timestamps_differences(self, timestamps_set):
        """
        timestamps_set : 1d array of floats or integers

            Calculates timestamp_set internal differences.
            It allows to estimate the cameras sample rate (frames per second)
            if timestamps_set == world_timestamps or
            if timestamps_set == eye_timestamps
        """
        return [timestamps_set[t + 1] - timestamps_set[t] for t, _ in enumerate(timestamps_set[0:len(timestamps_set)-1])]             

    def get_scapp_timestamps(self):
        scapp_timestamps = []
        for trial in self.scapp_output:
            for timestamped_event in trial:
                scapp_timestamps.append(float( timestamped_event[0] ))
        return scapp_timestamps

    def get_delivery_differences(self, timestamps1, timestamps2, timestamps1_primer = True):
        """
        timestamps1, timestamps2: string, eye, world or scapp
        timestamps_1_primer: boolean | true:use first item; false: ignore first item 

            World and Eye Timestamps are not perfectly alined in time:
            1) At first, data is sent from a source
                - world frames from the world camera
                - eye frames from the eye camera
            2) Data takes some time to be stamped by the Pupil Capture system.
               Then, we call that it was delivered to Pupil Capture system
               or simply that the data received a timestamp;
            3) This time may vary from frame to frame
            4) This time may vary between a world and eye frame
            
            Delivery or stamping time may vary from source to source.

            So, scapp event timestamps are not perfectly alined
             with the previous timestamps as well.

            1) At first, Pupil Server sends a timestamp
            2) This timestamp can be sent after or before a frame has been timestamped             

            Returns delivery differences between timestamps1 and timestamps2.
        """
        if   'eye'   in timestamps1:
            source_timestamps1 = self.eye_timestamps
        elif 'world' in timestamps1:
            source_timestamps1 = self.world_timestamps
        elif 'scapp' in timestamps1:
            source_timestamps1 = self.get_scapp_timestamps()

        if   'eye'   in timestamps2:
            source_timestamps2 = self.eye_timestamps
        elif 'world' in timestamps2:
            source_timestamps2 = self.world_timestamps
        elif 'scapp' in timestamps2:
            source_timestamps2 = self.get_scapp_timestamps()

        diff = []
        frames = []
    
        for timestamp in source_timestamps1:
            frame_index = np.abs(source_timestamps2 - timestamp).argmin()
            if timestamps1_primer:
                diff.append(source_timestamps2[frame_index] - timestamp)
                frames.append(frame_index)
            elif frame_index != 0:
                diff.append(source_timestamps2[frame_index] - timestamp)
                frames.append(frame_index)
        return diff, frames

    def trial_from_timestamp(self, timestamp):
        """
        timestamp: float

        Returns the nearest trial index associated with a given timestamp.
        """
        for i, trial in enumerate(self.scapp_output):
            trial_begin = float(trial[0][0])
            trial_end = float(trial[-1][0])
            if np.logical_and(trial_end >= timestamp, timestamp >= trial_begin):
            #if trial_end >= timestamp >= trial_begin:
                return i

    def timestamp_from_frame_index(self, frame_index):
        """
        frame_index: integer

        Returns a world timestamp from a given Frame Index.
        """
        if frame_index < len(self.timestamps):
            return self.timestamps[frame_index]


    def frame_index_from_timestamp(self, timestamp, timestamps_to_compare=None):
        """
        timestamp: float

        Uses numpy.argmin to return the nearest World Frame Index
        associated with a given timestamp.          

        http://docs.scipy.org/doc/numpy/reference/generated/numpy.argmin.html
        """
        if timestamps_to_compare:
            return np.abs(timestamps_to_compare - timestamp).argmin() 
        else:
            return np.abs(self.world_timestamps - timestamp).argmin()



    def correlate_world(self):
        '''
        this takes the self.gaze_data list and a world_timestamps list
        and makes a new frames_by_gaze list
        '''
        # create empty list with the size of recorded frames
        frames_by_gaze = [[] for _ in self.gaze_data]
    
        gaze_timestamps = [data_line[0] for data_line in self.gaze_data]

        # for each data line in self.gaze_data
        for timestamp in self.world_timestamps:
            gaze_idx = np.abs(gaze_timestamps - timestamp).argmin()
            frames_by_gaze[gaze_idx].append(timestamp)
        return frames_by_gaze

    def correlate_gaze(self):
        '''
        gaze_list: timestamp | confidence | gaze x | gaze y |
        timestamps: timestamps to correlate gaze data to


        this takes a gaze positions list and a timestamps list and makes a new list
        with the length of the number of recorded frames.
        Each slot conains a list that will have 0, 1 or more assosiated gaze postions.
        '''
        gaze_list = []
        gaze_list.extend(self.gaze_data[:])
        timestamps = []
        timestamps.extend(self.world_timestamps[:])

        gaze_data_by_frame = [[] for _ in timestamps]

        frame_idx = 0
        try:
            data_point = gaze_list.pop(0)
        except:
            #logger.warning("No gaze positons in this recording.")
            return gaze_data_by_frame

        gaze_timestamp = data_point[0]

        while gaze_list:
            # if the current gaze point is before the mean of the current world frame timestamp and the next worldframe timestamp
            try:
                t_between_frames = ( timestamps[frame_idx]+timestamps[frame_idx+1] ) / 2.
            except IndexError:
                break
            if gaze_timestamp <= t_between_frames:
                ts,confidence,x,y, = data_point
                gaze_data_by_frame[frame_idx].append({'norm_gaze':(x,y), 'confidence':confidence, 'timestamp':ts})
                data_point = gaze_list.pop(0)
                gaze_timestamp = data_point[0]
            else:
                frame_idx+=1

        return gaze_data_by_frame

    def save_world_ellipses_data(self, begin=0, end=None):
        """
            begin: world frame index | integer
            end: world frame index > begin | integer

            self.world_ellipses:
            - it is a list with detected ellipses from world frames
            - it has the same size and corresponds in time/index to self.world_timestamps
            - each item has the detected ellipses for each self.capture frame

        """
        # width, height = self.capture.get_size()

        # initialize empty data containers
        world_ellipses = [[] for _ in self.world_timestamps]
        world_contours = [[] for _ in self.world_timestamps]
        world_ppt_data = [[] for _ in self.world_timestamps]

        # begin/end frame handle
        start_frame = begin
        current_frame = start_frame
        if end is None:
            end_frame = self.capture.get_frame_count()
        else:
            end_frame = end
        trimmed_world_timestamps = self.world_timestamps[start_frame:end_frame]
        frames_to_export = len(trimmed_world_timestamps)

        # correlate eye timestamps with world timestamps
        gaze_data_by_frame = self.correlate_gaze()

        # seek to the current frame
        self.capture.seek_to_frame(start_frame)

        # loop for all frames
        print 'begin'
        while frames_to_export - current_frame > 0:
            # get the current frame
            frame = self.capture.get_frame()

            # initialize a dictionary, we need to follow pupil standards
            gaze_data = {}

            # take the gaze data from current frame
            gaze_data['pupil_positions'] = gaze_data_by_frame[frame.index]

            # manual gaze correction
            # the need for that becomes evident by looking at the video on Pupil Player
            # i.e., by eye inspection
            if False:
                for p in gaze_data['pupil_positions']:
                    if p['norm_gaze'] is not None:
                        p['norm_gaze'] = p['norm_gaze'][0] + (- 0.005), p['norm_gaze'][1] + (- 0.08)

            # load the ellipse detector
            # different ilumination conditions may require different parameters
            # parameters defined by eye inspection
            #ellipse_detector = Vis_Circle_On_Contours(self.g_pool, threshold=177, ellipse_size=2.0, expected_contours=9)
            ellipse_detector = Vis_Circle_On_Contours(self.g_pool, threshold=160, ellipse_size=2.0, expected_contours=2)

            # detection occurs through the update method
            ellipse_detector.update(frame, gaze_data)

            # get data from the detector
            world_ellipses[frame.index] = ellipse_detector.ellipses
            world_contours[frame.index] = ellipse_detector.contours
            world_ppt_data[frame.index] = ellipse_detector.ppt_data

            print current_frame
            current_frame += 1

        path = os.path.join(self.g_pool.rec_dir, 'world_ellipses')
        np.save(path,np.asarray(world_ellipses))

        path = os.path.join(self.g_pool.rec_dir, 'world_contours')
        np.save(path, np.asarray(world_contours))

        path = os.path.join(self.g_pool.rec_dir,'world_ppt_data')
        np.save(path, np.asarray(world_ppt_data))
        print 'done'

    def show(self, data_type):
        """
            We will use R for graphs, but for now this is faster. Ugly, though.
        """
        # matplotlib oop
        figure = plt.figure()
        axes = figure.add_axes([0.1, 0.1, 0.8, 0.8])

        if 'differences' in data_type:
            if 'eye' in data_type:
                t = self.eye_timestamps

            elif 'world' in data_type:
                t = self.world_timestamps

            if 'plot' in data_type:
                frames = np.array([i for i in range(len(t)-1)])
                diff = np.array(self.get_timestamps_differences(t))
                axes.plot(frames, diff*1000, 'k.')
                x_label = 'Timestamps ((x + 1) - x)'
                y_label = 'Differences (miliseconds)' 
                if 'eye' in data_type:
                    title = 'Eye Timestamps Internal Differences'
                
                elif 'world' in data_type:
                    title = 'World Timestamps Internal Differences'
                
            
            elif 'histogram' in data_type:
                diff = np.array(self.get_timestamps_differences(t))
                axes.hist(diff*1000, 100)
                y_label = 'Timestamps ((x + 1) - x)'
                x_label = 'Differences (miliseconds)'
                if 'eye' in data_type:
                    title = 'Eye Timestamps Internal Differences'
                
                elif 'world' in data_type:
                    title = 'World Timestamps Internal Differences'
                
            
        if 'delivery' in data_type:
            if 'histogram' in data_type:
                diff, _ = self.get_delivery_differences('scapp', 'world', timestamps1_primer = False)
                diff = np.array(diff)
                axes.hist(diff*1000, 16)
                y_label = 'frames'
                x_label = 'Before world timestamp <- (ms) -> After world timestamp'
                #title = 'World Timestamps Internal Differences'
                title = 'Server timestamp sent before or after Frame Timestamp'

            elif 'plot' in data_type:
                diff, frames = self.get_delivery_differences('scapp', 'world', timestamps1_primer = False)
                diff = np.array(diff)
                axes.plot(frames, diff*1000, 'k.')
                x_label = 'Frame Index'
                y_label = ' Before Frame <- (ms) -> After Frame'
                title = 'Server timestamp sent before or after Frame Timestamp'

        if 'confidence' in data_type:
            confidence = [line[1] for line in self.gaze_data]
            y = confidence
            x = xrange(len(confidence))

            if 'histogram' in data_type:
                bins = [n/float(100) for n in xrange(0,101,10)]
                #hist, bins = np.histogram(y, bins=bins)
                axes.hist(y, bins)
                x_label = 'Confidence'
                y_label = 'Number of Frames'
                title = 'Confidence Histogram'

            # http://matplotlib.org/api/figure_api.html
            elif 'plot' in data_type:
                axes.plot(x, y, 'k.')
                axes.set_ylim([0,np.amax(confidence)])
                x_label = 'Frame Index'
                y_label = 'Confidence'
                title = 'Confidence by Frame Index'

        if 'cummulative' in data_type:
            #https://docs.python.org/2/library/collections.html
            i = 0
            if 'global' in data_type:
                cummulative_hits = [] 
                for trial in self.scapp_report:
                    if trial['Result'] == 'HIT':
                        i += 1
                    cummulative_hits.append(i)

                trials = range(1, len(self.scapp_report) + 1)
                # print trials, cummulative_hits
                axes.step(trials, cummulative_hits)
                x_label = 'Trials'
                y_label = 'Cummulative Hits'
                title = 'Cummulative Hit by Trial'

            if 'by contingency' in data_type:
                i = 0
                cummulative_hits_p = []
                for trial in self.scapp_report:
                    if trial['ExpcResp'] == 'Positiva':
                        if trial['Result'] == 'HIT':
                            i += 1
                        cummulative_hits_p.append(i)

                cummulative_hits_n = []
                i = 0
                for trial in self.scapp_report:
                    if trial['ExpcResp'] == 'Negativa':
                        if trial['Result'] == 'HIT':
                            i += 1
                        cummulative_hits_n.append(i)

                trials = range(1, (len(self.scapp_report)/2) + 1)
                print cummulative_hits_n
                print cummulative_hits_p
                # print trials, cummulative_hits
                pt, = axes.step(trials, cummulative_hits_p)
                nt, = axes.step(trials, cummulative_hits_n)
                axes.legend([pt, nt],["positive","negative"], loc=2)

                x_label = 'Trials'
                y_label = 'Cummulative Hits'
                title = 'Cummulative Hit by Positive and Negative Trials'  


        if 'pupil' in data_type:
            y = self.pupil_data[:,4]
            dy = y.copy()
            dy[1:] = y[1:]-y[0:-1]

            dy, = axes.plot(dy)
            y, = axes.plot(y)
            axes.legend([dy,y],["1st derivative pupil y","pupil y position"])
            x_label = 'x'
            y_label = 'y'
            title = 'simple visualization'

        if 'correlate' in data_type:
            if 'gaze points' in data_type:
                if 'histogram' in data_type:
                    correlated_gaze = self.correlate_gaze()
                    #frames = range(1,len(correlated_gaze)+1)
                    gaze_per_frame = [len(gaze) for gaze in correlated_gaze]
                    axes.hist(gaze_per_frame, 5)
                    x_label = 'x'
                    y_label = 'y'
                    title = 'gaze points per frame'


                if 'plot' in data_type:
                    correlated_gaze = self.correlate_gaze()
                    #frames = range(1,len(correlated_gaze)+1)
                    gaze_per_frame = [len(gaze) for gaze in correlated_gaze]

                    axes.plot(gaze_per_frame, 'k.')
                    x_label = 'x'
                    y_label = 'y'
                    title = 'gaze points per frame'

            if 'frames' in data_type:
                if 'histogram' in data_type:
                    correlated_world = self.correlate_world()

                    frames_per_gaze = [len(gaze) for gaze in correlated_world]
                    print frames_per_gaze
                    axes.hist(frames_per_gaze, 16)
                    x_label = 'x'
                    y_label = 'y'
                    title = 'frames per gaze point'

                if 'plot' in data_type:
                    correlated_world = self.correlate_world()

                    frames_per_gaze = [len(gaze) for gaze in correlated_world]

                    axes.plot(frames_per_gaze, 'k.')
                    x_label = 'x'
                    y_label = 'y'
                    title = 'frames per gaze point'

        if 'process' in data_type:
            if 'plot' in data_type:
                cpu1 = [cpu[0] for cpu in self.g_pool.proc]
                #cpu2 = [cpu[1] for cpu in self.g_pool.proc]
                #cpu3 = [cpu[2] for cpu in self.g_pool.proc]
                #cpu4 = [cpu[3] for cpu in self.g_pool.proc]

                axes.plot(cpu1, 'g.')
                #axes.plot(cpu2, 'b.')
                #axes.plot(cpu3, 'r.')
                #axes.plot(cpu4, 'k.')

                x_label = 'x'
                y_label = 'y'
                title = 'frames per gaze point'


        axes.set_xlabel(x_label)
        axes.set_ylabel(y_label)  
        axes.set_title(title);

        # lets check the results
        plt.show()


if __name__ == "__main__":

    g_pool = Global_Container()

    g_pool.rec_dir = '/home/rafael/pupil-recent/recordings/2015_05_12/000'

    paths = [os.path.join(g_pool.rec_dir, 'gaze_positions.npy'),
             os.path.join(g_pool.rec_dir, 'pupil_positions.npy'),
             os.path.join(g_pool.rec_dir, 'cal_pt_cloud.npy'),
             os.path.join(g_pool.rec_dir, 'eye0_timestamps.npy'),
             os.path.join(g_pool.rec_dir, 'world_timestamps.npy'),
             os.path.join(g_pool.rec_dir, 'world.mkv'),
             os.path.join(g_pool.rec_dir, 'scapp_output'),
             os.path.join(g_pool.rec_dir, 'scapp_report'),
             os.path.join(g_pool.rec_dir, 'proc.npy')]

    # we choose to change some g_pool parameter names
    # original names as in pupil source are commented above these changes
    for i, path in enumerate(paths):
        if os.path.isfile(path):
            if i == 0:
                # g_pool.gaze_positions
                g_pool.gaze_data = np.load(path)
            if i == 1:
                # g_pool.pupil_positions
                g_pool.pupil_data = np.load(path)
            if i == 2:
                # g_pool.gaze_data
                g_pool.calibration_points = np.load(path)
            if i == 3:
                g_pool.eye_timestamps = np.load(path)
            if i == 4:
                # g_pool.timestamps
                g_pool.world_timestamps = np.load(path)
            if i == 5:
                # g_pool.cap
                g_pool.capture = autoCreateCapture(path, timestamps=g_pool.world_timestamps)
            if i == 8:
                # process information, it is not a pupil output
                g_pool.proc = np.load(path)
        else:
            print path, 'file was not found.'

###########################################################################################################  
########################################################################################################### 
########################################################################################################### 
########################################################################################################### 

    # Load data
    data = Data(g_pool)
    
    # Need to run this line once to detect ellipses from frames and save them to 'world_ellipses.npy'
    # it is a dependency.
    #data.save_world_ellipses_data()

    if True:
        #data.save_world_ellipses_data()
        world_ellipses = np.load(os.path.join(g_pool.rec_dir, 'world_ellipses.npy'))

        gaze_data_by_frame = data.correlate_gaze()

        # create a container with the size of the total trials
        Trials = [TrialContainer() for _ in data.scapp_output]

        # fill it with some data
        for n, trial in enumerate(Trials):
            trial.TargetCircle = data.scapp_report[n]['ExpcResp']
            trial.Angle = data.scapp_report[n]['Angle']
            trial.TimeEvents = data.scapp_output[n] 

        # fill it with timestamps, stimuli points and gaze points
        for frame_index, ellipses in enumerate(world_ellipses):
            aTimestamp = data.world_timestamps[frame_index]
            N = data.trial_from_timestamp(aTimestamp) 
            if N >= 0:
                #print N

                Trials[N].Timestamps.append(aTimestamp)
                
                if ellipses:             
                    # all ellipse center points from that timestamp/frame 
                    Trials[N].CirPoints.append([(ellipse[0][0], ellipse[0][1]) for ellipse in ellipses])
                else:
                    # no ellipse was found
                    Trials[N].CirPoints.append(None)
                
                gaze_data = gaze_data_by_frame[frame_index]    
                if gaze_data:
                    for p in gaze_data:
                        if p['norm_gaze'] is not None:
                            # manual gaze correction
                            #p['norm_gaze'] = p['norm_gaze'][0], p['norm_gaze'][1] + (-0.08)

                            # denormalize 
                            dnorm_gaze = denormalize(p['norm_gaze'],(1280, 720),flip_y=True)
                            Trials[N].GazePoints.append(dnorm_gaze)
                else:
                    Trials[N].GazePoints.append(None)

        # All frames from first trial
        print 'Trial 1 with:', len(Trials[0].CirPoints), ' frames.'

        # First response
        timestamps_to_compare = np.array(Trials[0].Timestamps) 
        
        firstResponse = np.abs(timestamps_to_compare - np.float64(Trials[0].TimeEvents[1][0]) ).argmin()
        print firstResponse
        # End
        endLimitedHold = np.abs(timestamps_to_compare - np.float64(Trials[0].TimeEvents[-1][0])).argmin()
        print endLimitedHold
        # 2 seconds interval
        frameInterval = range(firstResponse, endLimitedHold)

        for frame in frameInterval:
            print Trials[0].CirPoints[frame]
            print Trials[0].GazePoints[frame]

        #path = os.path.join(g_pool.rec_dir, 'ecenters')
        #np.savetxt(path, np.asarray(eset), fmt='%2.3f')
        
        # Manual gaze correction
        #pP = [(pP[point][0]- 10, pP[point][1]- 45) for point in range(len(pP))]

        # we aim to reach something like this:
        if False:
            i = 1
            n = 60
            for Trial in Trials:
                number = "%02d" % (i,)
                #n = len(Trial.GazePoints)
                pP = Trial.GazePoints
                pB = Trial.CirBPoints
                pA = Trial.CirAPoints
                if len(pP) == len(pB) == len(pA):
                    X, Y = get_x_y(pA)
                    plt.plot(X, Y, 'r.')

                    X, Y = get_x_y(pB)
                    plt.plot(X, Y, 'b.')

                    X, Y = get_x_y(pP)
                    plt.plot(X, Y, 'k.')
                    
                    plt.axis([0, 1280, 720, 0])
                    plt.title('Distances:' + number)
                    plt.savefig('data/_Orig:' + number + '.png', bbox_inches='tight')
                    plt.close()

                if len(pP) == len(pB) == len(pA):
                    norm_dist = normalized_distance(euclidean_distance(pA, pP), euclidean_distance(pA, pB))
                    plt.plot(norm_dist, 'k.')
                    plt.axis([0, n, -2, 2])
                    plt.title('Normalized distances:' + number)
                    plt.savefig('data/_Norm:' + number +'.png', bbox_inches='tight')
                    plt.close()
                print i
                i += 1
           

    #for line in world_ppt_data[400:500]:
    #    if len(line) == 9:
    #        print line[0][0][0] - line[1][0][0]         


#    for frame_ellipses in data.world_ellipses[300:1000]:
#        if frame_ellipses:
#            for ellipse in frame_ellipses:
#                print ellipse
#                alfa = 1.3
#                center = ( int(round( ellipse[0][0] )), int( round( ellipse[0][1] ))) 
#                axes = ( int( round( ellipse[1][0]/alfa )), int( round( ellipse[1][1]/alfa )))
#                angle = int( round(ellipse[2] ))
#                print center, axes, angle


    #data.show('correlate frames plot')
    #data.show('delivery histogram')
    #data.show('differences world histogram')
    #data.show('differences plot')
    #data.show('delays histogram') 
    #data.show('cummulative by contingency')
    #data.show('process plot')


    #print Counter(data.session_trials['Result'])

    #PositiveResult = [trial['Result'] for trial in data.session_trials if trial['ExpcResp'] == 'Positiva']
    #NegativeResult = [trial['Result'] for trial in data.session_trials if trial['ExpcResp'] == 'Negativa']

    #PositiveResult, NegativeResult = Counter(PositiveResult), Counter(NegativeResult)

    #d_index_pos = PositiveResult['HIT']/float(sum(PositiveResult.values()))
    #d_index_neg = NegativeResult['HIT']/float(sum(NegativeResult.values()))

    #print 'DIP:', d_index_pos, 'DIN:', d_index_neg

    #data.show('delays') 

# print (50952 - 50450)/60

