# Core Wars

A version of Core Wars written in Haskell.

The program reads in warriors 'programs' from files and places them apart in an initialised MARS (cyclical array of instructions).

The MARS environment is a Map of integers to Instructions. It is stored in the Mars TVar, which all the warriors have access to.

Each warrior is a thread. Each thread has a list of instruction pointers, which correspond to where each of its tasks are.

Each warrior has access to the logger (MVar). Only one of the warriors can take the logger at once.
The logger is taken so that a warrior can print out the instruction it has just performed to the console.

<img src="assets/core-wars-example.png"/>
