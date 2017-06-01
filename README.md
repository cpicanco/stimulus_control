# Stimulus Control

[![DOI](https://zenodo.org/badge/17609066.svg)](https://zenodo.org/badge/latestdoi/17609066)

Stimulus Control is a fork of EAM (https://github.com/eep-lab/eam).

- It is aimed at fast programming of behavioral experiments.
- It can be extended to Design, Present, Record and Archive a wide range of behavioral experiments.
- It was designed for Participants, Researchers and Developers.
- It is a prototype to run non time-critical experiments.
- It is being inspired by OpenSesame, Psychopy and Expyriment.

**[Download the prototype here](https://github.com/cpicanco/stimulus_control/releases)**.

**[Instructions to compile the prototype here](https://github.com/cpicanco/stimulus_control/wiki)**.

### Some features

  - Text Messages
  - Simple Discriminations
  - Conditional Discriminations
  
  - Task Styles
    - Feature positive effect (uses keyboard as operandum)
    - Go/No-Go (using keyboard or mouse as operandum)
    - MTS (using keyboard or mouse as operandum)
    - SPAN (using keyboard or mouse as operandum, work in progress)
    - and others
    
  - Stimuli (antecedents, consequents):
    - Sounds using the (non-free, as in freedom) Bass library (a substitute is in need)
    - Images (jpg, bmp, png), opaque or transparent
    - Videos using the LCLVLC component and the libvlc library will be implemented.
  
  - Backgrounds
    - Random visual mask
    - Colors
    
  - Responses (schedules of reinforcements):
    - CRF, EXT
    - FT, VT
    - FR, VR
    - FI, VI,
    - DRH, DRL

  - Designs
    - Free operant
    - Discrete Trials
    
  - Performance Assessment/Criteria
    - Consecutive Hits
    - Hit porcentage in Bloc
  
  - Blocs of Trials
    - Create blocs of trials
    - Concatenate blocs of trials
    - Concatenate blocs of trials based on participant's performance (if criteria was reached, go to bloc x, if not go to bloc y)

  - Timestamped Events and some communication with Pupil Software (https://github.com/pupil-labs/pupil);

  - Cross-Platform. Tested under Windows (7, 8 and 10), Crunchbang 11, Ubuntu LTS (13.10, 14.04, 16.04) and Debian 8.

  - Free software, GPL3, compiled with Lazarus RAD IDE (1.6.2, 1.6.4) and FPC (3.0.0, 3.0.2).
  
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

https://github.com/cpicanco/stimulus_control/wiki