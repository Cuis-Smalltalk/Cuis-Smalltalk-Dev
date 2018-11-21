'From Cuis 5.0 of 7 November 2016 [latest update: #3485] on 8 November 2018 at 6:23:20 pm'!

!BlockClosure methodsFor: 'scheduling' stamp: 'jmv 11/8/2018 18:23:08'!
fork
	"Create and schedule a Process running the code in the receiver."
	
	"jmv - Do NOT answer the new process.
	
	See http://lists.squeakfoundation.org/pipermail/squeak-dev/2008-February/124960.html
	
	Most times, these methods return before resuming the new process (if priority of new process is less 
	or equal than current). But they might return afterwards.
	
	This means it is very dangerous to use the returned process in code that stores it in some variable 
	and checks for nil to start a new one. If these methods happen to return after the new process is forked,
	chances are the code that starts all this runs again, that variable is nil, and a second process is forked,
	perhaps breaking some shared state. This kind of bug is hard to spot and debug.
	
	Callers wanting the new process object, should call #newProcess, store the answer, and then #resume.
	
	A way to ensure this bug will not ever happen again is just to answer nil"

	self newProcess resume.
	^nil! !

