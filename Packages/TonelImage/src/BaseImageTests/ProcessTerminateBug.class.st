Class {
	#name : 'ProcessTerminateBug',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Processes',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Processes'
}

{ #category : 'tests' }
ProcessTerminateBug >> testSchedulerTermination [
	| process sema gotHere sema2 |
	gotHere _ false.
	sema _ Semaphore new.
	sema2 _ Semaphore new.
	process _ [
		sema signal.
		sema2 wait.
		"will be suspended here"
		gotHere _ true ] newProcess.
	process priority: Processor activeProcess priority.
	process name: 'testSchedulerTermination'.
	process resume.
	sema wait.
	"until process gets scheduled"
	process terminate.
	sema2 signal.
	Processor yield.
	"will give process a chance to continue and horribly screw up"
	self assert: gotHere not.
]

{ #category : 'tests' }
ProcessTerminateBug >> testTerminationDuringNestedUnwind1 [
	"Terminate suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p x1 x2 x3 x4 |
	x1 := x2 := x3 := x4 := false.
	p := 
		[
			[
				[ ] ensure: [
					[Processor activeProcess suspend] ensure: [
						x1 := true]. 
					x2 := true]
			] ensure: [
				x3 := true].
			x4 := true.
		] newProcess.
	p resume.
	Processor yield.
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended.
	self deny: x1 | x2 | x3 | x4.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated.
	self assert: x1 & x2 & x3.
	self deny: x4.
]

{ #category : 'tests' }
ProcessTerminateBug >> testTerminationDuringNestedUnwind2 [
	"Terminate suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p x1 x2 x3 x4 |
	x1 := x2 := x3 := x4 := false.
	p := 
		[
			[
				[ ] ensure: [
					[ ] ensure: [
						Processor activeProcess suspend. 
						x1 := true]. 
					x2 := true]
			] ensure: [
				x3 := true].
			x4 := true.
		] newProcess.
	p resume.
	Processor yield.
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended.
	self deny: x1 | x2 | x3 | x4.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated.
	self assert: x1 & x2 & x3.
	self deny: x4.
]

{ #category : 'tests' }
ProcessTerminateBug >> testTerminationDuringNestedUnwind3 [
	"Terminate runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p x1 x2 x3 x4 |
	x1 := x2 := x3 := x4 := false.
	p := 
		[
			[
				[ ] ensure: [
					[Processor yield] ensure: [
						x1 := true]. 
					x2 := true]
			] ensure: [
				x3 := true].
			x4 := true.
		] newProcess.
	p resume.
	Processor yield.
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady.
	self deny: x1 | x2 | x3 | x4.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated.
	self assert: x1 & x2 & x3.
	self deny: x4.
]

{ #category : 'tests' }
ProcessTerminateBug >> testTerminationDuringNestedUnwind4 [
	"Terminate runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p x1 x2 x3 x4 |
	x1 := x2 := x3 := x4 := false.
	p := 
		[
			[
				[ ] ensure: [
					[ ] ensure: [
						Processor yield. 
						x1 := true]. 
					x2 := true]
			] ensure: [
				x3 := true].
			x4 := true.
		] newProcess.
	p resume.
	Processor yield.
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isReady.
	self deny: x1 | x2 | x3 | x4.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated.
	self assert: x1 & x2 & x3.
	self deny: x4.
]

{ #category : 'tests' }
ProcessTerminateBug >> testTerminationDuringNestedUnwind5 [
	"Terminate active process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p x1 x2 x3 x4 |
	x1 := x2 := x3 := x4 := false.
	p := 
		[
			[
				[ ] ensure: [
					[Processor activeProcess terminate] ensure: [
						x1 := true]. 
					x2 := true]
			] ensure: [
				x3 := true].
			x4 := true.
		] newProcess.
	p resume.
	"now let the process run and make sure all unwind blocks have finished"
	Processor yield.
	self assert: p isTerminated.
	self assert: x1 & x2 & x3.
	self deny: x4.
]

{ #category : 'tests' }
ProcessTerminateBug >> testTerminationDuringNestedUnwind6 [
	"Terminate active process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p x1 x2 x3 x4 |
	x1 := x2 := x3 := x4 := false.
	p := 
		[
			[
				[ ] ensure: [
					[ ] ensure: [
						Processor activeProcess terminate.
						x1 := true]. 
					x2 := true]
			] ensure: [
				x3 := true].
			x4 := true.
		] newProcess.
	p resume.
	"now let the process run and make sure all unwind blocks have finished"
	Processor yield.
	self assert: p isTerminated.
	self assert: x1 & x2 & x3.
	self deny: x4.
]

{ #category : 'tests' }
ProcessTerminateBug >> testTerminationDuringNestedUnwind7 [
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p x1 x2 x3 x4 semaphore |
	x1 := x2 := x3 := x4 := false.
	semaphore := Semaphore new.
	p := 
		[
			[
				[ ] ensure: [
					[semaphore wait] ensure: [
						x1 := true]. 
					x2 := true]
			] ensure: [
				x3 := true].
			x4 := true.
		] newProcess.
	p resume.
	Processor yield.
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isBlocked.
	self deny: x1 | x2 | x3 | x4.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated.
	self assert: x1 & x2 & x3.
	self deny: x4.
]

{ #category : 'tests' }
ProcessTerminateBug >> testTerminationDuringNestedUnwind8 [
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p x1 x2 x3 x4 semaphore |
	x1 := x2 := x3 := x4 := false.
	semaphore := Semaphore new.
	p := 
		[
			[
				[ ] ensure: [
					[ ] ensure: [
						semaphore wait.
						x1 := true]. 
					x2 := true]
			] ensure: [
				x3 := true].
			x4 := true.
		] newProcess.
	p resume.
	Processor yield.
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isBlocked.
	self deny: x1 | x2 | x3 | x4.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated.
	self assert: x1 & x2 & x3.
	self deny: x4.
]

{ #category : 'tests' }
ProcessTerminateBug >> testTerminationDuringUnwind [
	"An illustration of the issue of process termination during unwind.
	This uses a well-behaved unwind block that we should allow to complete
	if at all possible."
	"
	self new testTerminationDuringUnwind
	"
	| unwindStarted unwindFinished p |
	unwindStarted := unwindFinished := false.
	p := [[] ensure:[
			unwindStarted := true.
			Processor yield.
			unwindFinished := true.
		]] newProcess.
	p resume.
	self deny: unwindStarted description: 'Expected unwind block not to have started yet, but it has!'.
	Processor yield.
	self assert: unwindStarted description: 'Expected unwind block to have started by now, but it hasn''t!'.
	self deny: unwindFinished description: 'Expected unwind block not to have finished yet, but it has!'.
	p terminate.
	self assert: unwindFinished description: 'Expected unwind block to have successfully finished by now, but it hasn''t!'.
]

{ #category : 'tests' }
ProcessTerminateBug >> testUnwindFromActiveProcess [
	| sema process |
	sema := Semaphore forMutualExclusion.
	self assert:(sema isSignaled).
	process := [
		sema critical:[
			self deny: sema isSignaled.
			Processor activeProcess terminate.
		]
	] newProcess.
	process priority: Processor userInterruptPriority.
	process name: 'testUnwindFromActiveProcess'.
	process resume.
	self assert: sema isSignaled
]

{ #category : 'tests' }
ProcessTerminateBug >> testUnwindFromForeignProcess [
	| sema process |
	sema := Semaphore forMutualExclusion.
	self assert: sema isSignaled.
	process := [
		sema critical:[
			self deny: sema isSignaled.
			sema wait. "deadlock"
		]
	] newProcess.
	process priority: Processor userInterruptPriority.
	process name: 'testUnwindFromForeignProcess'.
	process resume.
	self deny: sema isSignaled.
	"This is for illustration only - the BlockCannotReturn cannot 
	be handled here (it's truncated already)"
	self shouldnt: [process terminate] raise: BlockCannotReturn.
	self assert: sema isSignaled
]
