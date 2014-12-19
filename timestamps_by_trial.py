'''
//
// Validation Project (PCRF) - Eye Tracking Set Up Validation
// Copyright (C) 2014,  Carlos Rafael Fernandes Picanco, cpicanco@ufpa.br
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
import ast

timestamps_by_trial_path = '/home/validation_study/timestamps'

timestamps_by_trial = [[]]
with open(timestamps_by_trial_path) as f:
	for line in f:
		temp = ast.literal_eval(line)

		#temp = [0, 1, 2] = (trial_index, timestamp, a_code)
		i = int(temp[0])
		timestamp = (temp [1], temp[2])


		if (i + 1) > len(timestamps_by_trial):
			timestamps_by_trial.append([])
		timestamps_by_trial[i].append(timestamp)
	f.close

for x in xrange(len(timestamps_by_trial)):
	print str(timestamps_by_trial[x])



	


