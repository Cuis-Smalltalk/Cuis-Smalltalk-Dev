'From Cuis7.1 [latest update: #6468] on 13 June 2024 at 3:24:49 pm'!

!BlockClosure methodsFor: 'scheduling' stamp: 'jmv 6/13/2024 15:22:33'!
newProcess
	"Answer a Process running the code in the receiver. The process is not 
	scheduled."
	<primitive: 19> "Simulation guard"
	| p |
	p := Process
		forContext: [
			self value.
			p suspend] asContext
		priority: Processor activePriority.
	^p! !

!BlockClosure methodsFor: 'scheduling' stamp: 'jmv 6/13/2024 15:16:39'!
newProcessForDebugging
	"Answer a Process running the code in the receiver. The process is not 
	scheduled."
	<primitive: 19> "Simulation guard"
	| p |
	p := Process
		forContext: [
			self value.
			p terminate] asContext
		priority: Processor activePriority.
	^p! !

!BlockClosure methodsFor: 'scheduling' stamp: 'jmv 6/13/2024 15:23:32'!
newProcessWith: anArray 
	"Answer a Process running the code in the receiver. The receiver's block 
	arguments are bound to the contents of the argument, anArray. The 
	process is not scheduled."
	<primitive: 19> "Simulation guard"
	| p |
	p := Process
		forContext: [
			self valueWithArguments: anArray.
			p suspend] asContext
		priority: Processor activePriority.
	^p! !

