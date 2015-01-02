Validation Project
==================

Stimulus Control Apps

//

This is a repository to my own stimulus control programs.

Validation Project (PCRF) is an ongoing project and includes a stimulus control program (validation_study) and some simple (hacked) python scripts. The main program (validation_study) was writen in Object Free Pascal and Delphi with the lazarus 1.0.14 IDE and was compiled on ubuntu 13.10 (sancy), but was thought to be cross-plataform.

The Python Scripts were meant to be interfaces to the Pupil Eye Tracking, open source eye tracking hardware and software platform (https://github.com/pupil-labs/pupil).

The python script 'gettimestamps.py' is part of a(n ugly) solution to produce a 'timestamp' (Pupil output) data structure sinchronized with some of the stimulus control program events. This structure will be useful for further analysis. 

The script 'timestamps_by_trial.py' ilustrates how to read the file 'timestamps' (the validation_study output now) data in python.

//

Este é um repositório para os meus próprios programas de controle de estímulo.

Projeto Validação (PCRF) é um projeto em andamento e inclui um programa de controle de estímulo (validation_study) e alguns scripts simples (hackeados) em python. O programa principal (validation_study) foi escrito em Object Free Pascal e Delphi com a IDE 'Lazarus 1.0.14' e foi compilado no Ubuntu 13.10 (sancy), mas foi pensado para ser multi-plataforma.

Os scripts em Python foram feitos para serem uma interface para o Pupil Eye Tracking, uma plataforma open source que inclui um hardware e um software de monitoramento de movimentos oculares (https://github.com/pupil-labs/pupil).

O script python 'gettimestamps.py' é parte de uma solução (meio feia) para a produção de uma estrutura de 'timestamps' (Pupil output) sincronizada com alguns eventos do programa de controle de estímulos. Essa estrutura setá útil para análises posteriores.

O script 'timestamps_by_trial.py' ilustra como ler o arquivo 'timestamps' (o output do validation_study desta vez) em python.
