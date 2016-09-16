# Stimulus Control

[![Join the chat at https://gitter.im/open_behavior_analysis/stimulus_control](https://badges.gitter.im/open_behavior_analysis/stimulus_control.svg)](https://gitter.im/open_behavior_analysis/stimulus_control?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Stimulus Control is a fork of EAM (https://github.com/eep-lab/eam).

- It is aimed at fast programming of behavioral experiments.
- It can be extended to Design, Present, Record and Archive a wide range of behavioral experiments.
- It was designed for Participants, Researchers and Developers.
- It is a prototype to run non time-critical experiments.
- It is being inspired by OpenSesame, Psychopy and Expyriment.

### Some features

  - Text Messages
  - Simple Discriminations
  - Conditional Discriminations
  - Stimuli (antecedents, consequents):
      - Sounds using the (non-free, as in freedom) Bass library (a substitute is in need)
      - Videos using the LCLVLC component and the libvlc library
      - Images (jpg, bmp)

  - Responses (schedules of reinforcements):
      - CRF, EXT
      - FT, VT
      - FR, VR
      - FI, VI,
      - DRH, DRL

  - Designs
    - Free operant
    - Discrete Trials

  - Timestamped Events and some communication with Pupil Software (https://github.com/pupil-labs/pupil);

  - Cross-Platform (tested under Windows (7, 8 and 10), Crunchbang 11, Ubuntu (13.10 and 14.04) and Debian 8.

  - Free software (GPL3) written in Object Free Pascal (Lazarus 1.6.0, FPC 3.0.0).
  - Core code was written using a Behavior Analytic ontology.


### Changes Made

- Adapted from Delphi to Free Pascal compiler and Lazarus development environment
- Replaced old GUI for a new prototype one
- Fixed some timing bugs
- Fixed some bugs related to the TTrial create/free life cycle
- New trial types inherited from TTrial
- Refactoring for better Behavior Analytic ontology fit.
- For more, please check GitHub issues: https://github.com/cpicanco/stimulus_control/issues

### Running from source

https://github.com/cpicanco/validation_project/wiki
