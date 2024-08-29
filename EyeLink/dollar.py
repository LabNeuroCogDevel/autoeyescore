import numpy as np
import mne
from mne.preprocessing.eyetracking import read_eyelink_calibration
from mne.viz.eyetracking import plot_gaze
# https://mne.tools/dev/auto_tutorials/preprocessing/90_eyetracking_data.html
et_fpath = "example//eyelink_20240826//sub-steve//ses-01//20240827_DollarReward//sub_steve_ses_01_task_DR_run_1_20244827134803.asc"
cals = read_eyelink_calibration(et_fpath)
raw_et = mne.io.read_raw_eyelink(et_fpath, create_annotations=["blinks"])

x = mne.events_from_annotations(raw_et)

# et_events = mne.find_events(raw_et, min_duration=0.01, shortest_event=1, uint_cast=True)
raw_et.plot(scalings=dict(eyegaze=1e3), block=True)



#raw_et._raw_extras[0]['dfs'].keys()
#dict_keys(['samples', 'fixations', 'saccades', 'blinks', 'messages', 'recording_blocks'])
def raw_et_events(raw_et):
    """
    add trial and expand 'event_msg' of the dataframe embeded in mne-read asc object
    """
    events = raw_et._raw_extras[0]['dfs']['messages']
    events['end'] = np.append(events['time'][1:], raw_et._last_time)
    events['trial'] = events.event_msg.str.extract('TRIALID (\\d+)').ffill()
    events[['event','rew','pos']] = events.event_msg.str.replace('^\\d+_','',regex=True).str.split('_',expand=True).ffill()
    return events

lookup={'iti':0,'ring':1,'cue':2,'dot':3}
def et_eventdf_to_annotatons(events, sfreq=1000):
    """
    MNE style event array [onset idx, duration=0, event_as_int] from events dataframe
    """
    et_events = np.stack([events.time*sfreq, events.time*0, [lookup.get(x,0) for x in events.event ]],axis=1).astype(int)
    return et_events

def sac_into_annotations(raw_et):
    """
    add saccades to MNE annotatoins. maybe unnecessary if 'saccades' option to read_raw_eyelink's create_annotations=[]
    """
    sacs = raw_et._raw_extras[0]['dfs']['saccades']
    sac_annotation = mne.Annotations(sacs.time, sacs.duration, 'saccade', orig_time = raw_et.annotations[0]['orig_time'])
    orig_annotation = raw_et.annotations
    raw_et.set_annotations( orig_annotation + sac_annotation)
    return raw_et

events = raw_et_events(raw_et)
et_events = et_eventdf_to_annotatons(events, sfreq=raw_et.info['sfreq'])
raw_et = sac_into_annotations(raw_et)
raw_et.plot(scalings=dict(eyegaze=1e3), block=True, events=et_events, event_id=lookup)


# epochs = mne.Epochs(raw_et, events=et_events, event_id=lookup, event_repeated='drop')
# epochs[:8].plot(events=et_events, event_id=lookup, block=True)


####

beh_fpath = "example/sub_12008_ses_01_task_DR_run_1_20241809121833.asc"

# TODO: is 'saccades' a valid annotation?
behave_raw_et = mne.io.read_raw_eyelink(beh_fpath, create_annotations=["blinks"])
beh_events_df = raw_et_events(behave_raw_et)
beh_events_arr = et_eventdf_to_annotatons(beh_events_df, sfreq=behave_raw_et.info['sfreq'])
sac_into_annotations(behave_raw_et).plot(scalings=dict(eyegaze=1e3), block=True, events=beh_events_arr, event_id=lookup)
