# Some useful tips to keep in mind

## VM dump of Smalltalk processes

(thanks Eliot!)
Launch the system in a terminal window. (using the RunCuisOn* script)
Send SIGUSR1 to the process (via kill -USR1 pid).
The vm will dump the stack traces of all processes to stdout.