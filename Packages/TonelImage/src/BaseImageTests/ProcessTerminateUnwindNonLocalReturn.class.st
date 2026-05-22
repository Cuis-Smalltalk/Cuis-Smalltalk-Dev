Class {
	#name : 'ProcessTerminateUnwindNonLocalReturn',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Processes',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Processes'
}

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux1A: anArray sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		self aux1Ainner: anArray.
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux1Ainner: anArray [


		[ ] ensure: [
			[Processor activeProcess suspend] ensure: [
				^anArray at: 1 put: true].
			anArray at: 2 put: true]

]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux1B: anArray sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		self aux1Binner: anArray
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux1Binner: anArray [

	[ ] ensure: [
		[Processor activeProcess suspend] ensure: [
			anArray at: 1 put: true].
		^anArray at: 2 put: true]
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux1C: anArray sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[Processor activeProcess suspend] ensure: [
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		^anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux1D: anArray sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[Processor activeProcess suspend] ensure: [
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		anArray at: 3 put: true].
	^anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux2A: anArray sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		self aux2Ainner: anArray
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux2Ainner: anArray [

	[ ] ensure: [
		[] ensure: [
			Processor activeProcess suspend.
			^anArray at: 1 put: true].
		anArray at: 2 put: true]
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux2B: anArray sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		self aux2Binner: anArray
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux2Binner: anArray [

	[ ] ensure: [
		[] ensure: [
			Processor activeProcess suspend.
			anArray at: 1 put: true].
		^anArray at: 2 put: true]
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux2C: anArray sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[] ensure: [
				Processor activeProcess suspend.
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		^anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux2D: anArray sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[] ensure: [
				Processor activeProcess suspend.
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		anArray at: 3 put: true].
	^anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux3A: anArray sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		self aux3Ainner: anArray
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux3Ainner: anArray [

	[ ] ensure: [
		[Processor yield] ensure: [
			^anArray at: 1 put: true].
		anArray at: 2 put: true]
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux3B: anArray sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		self aux3Binner: anArray
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux3Binner: anArray [

	[ ] ensure: [
		[Processor yield] ensure: [
			anArray at: 1 put: true].
		^anArray at: 2 put: true]
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux3C: anArray sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[Processor yield] ensure: [
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		^anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux3D: anArray sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[Processor yield] ensure: [
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		anArray at: 3 put: true].
	^anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux4A: anArray sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		self aux4Ainner: anArray
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux4Ainner: anArray [

	[ ] ensure: [
		[] ensure: [
			Processor yield.
			^anArray at: 1 put: true].
		anArray at: 2 put: true]
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux4B: anArray sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		self aux4Binner: anArray
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux4Binner: anArray [

	[ ] ensure: [
		[] ensure: [
			Processor yield.
			anArray at: 1 put: true].
		^anArray at: 2 put: true]
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux4C: anArray sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[] ensure: [
				Processor yield.
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		^anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux4D: anArray sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[] ensure: [
				[ sentinel second ] whileFalse: [
					Processor yield ].
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		anArray at: 3 put: true].
	^anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux5A: anArray [

	[
		self aux5Ainner: anArray
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux5Ainner: anArray [

	[ ] ensure: [
		[Processor activeProcess terminate] ensure: [
			^anArray at: 1 put: true].
		anArray at: 2 put: true]
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux5B: anArray [

	[
		self aux5Binner: anArray
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux5Binner: anArray [

	[ ] ensure: [
		[Processor activeProcess terminate] ensure: [
			anArray at: 1 put: true].
		^anArray at: 2 put: true]
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux5C: anArray [

	[
		[ ] ensure: [
			[Processor activeProcess terminate] ensure: [
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		^anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux5D: anArray [

	[
		[ ] ensure: [
			[Processor activeProcess terminate] ensure: [
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		anArray at: 3 put: true].
	^anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux6A: anArray [

	[
		self aux6Ainner: anArray
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux6Ainner: anArray [

	[ ] ensure: [
		[] ensure: [
			Processor activeProcess terminate.
			^anArray at: 1 put: true].
		anArray at: 2 put: true]
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux6B: anArray [

	[
		self aux6Binner: anArray
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux6Binner: anArray [

	[ ] ensure: [
		[] ensure: [
			Processor activeProcess terminate.
			anArray at: 1 put: true].
		^anArray at: 2 put: true]
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux6C: anArray [

	[
		[ ] ensure: [
			[] ensure: [
				Processor activeProcess terminate.
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		^anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux6D: anArray [

	[
		[ ] ensure: [
			[] ensure: [
				Processor activeProcess terminate.
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		anArray at: 3 put: true].
	^anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux7A: anArray semaphore: semaphore sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		self aux7Ainner: anArray semaphore: semaphore
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux7Ainner: anArray semaphore: semaphore [

	[ ] ensure: [
		[semaphore wait] ensure: [
			^anArray at: 1 put: true].
		anArray at: 2 put: true]
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux7B: anArray semaphore: semaphore sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		self aux7Binner: anArray semaphore: semaphore
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux7Binner: anArray semaphore: semaphore [

	[ ] ensure: [
		[semaphore wait] ensure: [
			anArray at: 1 put: true].
		^anArray at: 2 put: true]
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux7C: anArray semaphore: semaphore sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[semaphore wait] ensure: [
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		^anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux7D: anArray semaphore: semaphore sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[semaphore wait] ensure: [
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		anArray at: 3 put: true].
	^anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux8A: anArray semaphore: semaphore sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		self aux8Ainner: anArray semaphore: semaphore
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux8Ainner: anArray semaphore: semaphore [

	[ ] ensure: [
		[] ensure: [
			semaphore wait.
			^anArray at: 1 put: true].
		anArray at: 2 put: true]
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux8B: anArray semaphore: semaphore sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		self aux8Binner: anArray semaphore: semaphore
	] ensure: [
		anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux8Binner: anArray semaphore: semaphore [

	[ ] ensure: [
		[] ensure: [
			semaphore wait.
			anArray at: 1 put: true].
		^anArray at: 2 put: true]
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux8C: anArray semaphore: semaphore sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[] ensure: [
				semaphore wait.
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		^anArray at: 3 put: true].
	anArray at: 4 put: true
]

{ #category : 'aux' }
ProcessTerminateUnwindNonLocalReturn >> aux8D: anArray semaphore: semaphore sentinel: sentinel [

	sentinel at: 1 put: true.
	[
		[ ] ensure: [
			[] ensure: [
				semaphore wait.
				anArray at: 1 put: true].
			anArray at: 2 put: true]
	] ensure: [
		anArray at: 3 put: true].
	^anArray at: 4 put: true
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test1AResume [
	"Resume suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux1A: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	p resume.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a third & a fourth description: 'assert #4 failed'.
	self deny: a second description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test1ATerminate [
	"Terminate suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux1A: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a third description: 'assert #4 failed'.
	self deny: a second | a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test1BResume [
	"Resume suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux1B: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	p resume.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test1BTerminate [
	"Terminate suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux1B: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test1CResume [
	"Resume suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux1C: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	p resume.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test1CTerminate [
	"Terminate suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux1C: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test1DResume [
	"Resume suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux1D: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	p resume.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test1DTerminate [
	"Terminate suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux1D: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test2AResume [
	"Resume suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux2A: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	p resume.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a third & a fourth description: 'assert #4 failed'.
	self deny: a second description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test2ATerminate [
	"Terminate suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux2A: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a third description: 'assert #4 failed'.
	self deny: a second | a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test2BResume [
	"Resume suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux2B: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	p resume.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test2BTerminate [
	"Terminate suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux2B: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test2CResume [
	"Resume suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux2C: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	p resume.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test2CTerminate [
	"Terminate suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux2C: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test2DResume [
	"Resume suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux2D: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	p resume.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test2DTerminate [
	"Terminate suspended process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux2D: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is suspended and none of the unwind blocks has finished yet"
	self assert: p isSuspended description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test3AResume [
	"Resume runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux3A: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now let the process run and make sure all unwind blocks have finished"
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a third & a fourth description: 'assert #4 failed'.
	self deny: a second description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test3ATerminate [
	"Terminate runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux3A: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #2 failed'.
	self assert: a first & a third description: 'assert #3 failed'.
	self deny: a second | a fourth description: 'assert #4 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test3BResume [
	"Resume runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux3B: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now let the process run and make sure all unwind blocks have finished"
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test3BTerminate [
	"Terminate runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux3B: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test3CResume [
	"Resume runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux3C: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now let the process run and make sure all unwind blocks have finished"
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test3CTerminate [
	"Terminate runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux3C: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test3DResume [
	"Resume runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux3D: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now let the process run and make sure all unwind blocks have finished"
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test3DTerminate [
	"Terminate runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux3D: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test4AResume [
	"Resume runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux4A: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self deny: a first description: 'assert #2.1 failed'.
	self deny: a second description: 'assert #2.2 failed'.
	self deny: a third description: 'assert #2.3 failed'.
	self deny: a fourth description: 'assert #2.4 failed'.
	"now let the process run and make sure all unwind blocks have finished"
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a third & a fourth description: 'assert #4 failed'.
	self deny: a second description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test4ATerminate [
	"Terminate runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux4A: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a third description: 'assert #4 failed'.
	self deny: a second | a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test4BResume [
	"Resume runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux4B: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now let the process run and make sure all unwind blocks have finished"
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test4BTerminate [
	"Terminate runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux4B: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test4CResume [
	"Resume runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux4C: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'Aux process should have yielded and become ready'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'Aux process should have yielded before modifying state'.
	"now let the process run and make sure all unwind blocks have finished"
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'Aux process should have finished running'.
	self assert: a first & a second & a third description: 'Array with wrong contents: ', a printString.
	self deny: a fourth description: 'Last element should have not been updated'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test4CTerminate [
	"Terminate runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux4C: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test4DResume [
	"Resume runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false with: false.
	p := [
			self aux4D: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	s at: 2 put: true.
	"now let the process run and make sure all unwind blocks have finished"
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test4DTerminate [
	"Terminate runnable process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a s |
	a := Array new: 4 withAll: false.
	s := Array with: false with: false.
	p := [
			self aux4D: a sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is runnable and none of the unwind blocks has finished yet"
	self assert: p isReady description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	s at: 2 put: true.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test5ATerminate [
	"Terminate active process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a |
	a := Array new: 4 withAll: false.
	p := [
			self aux5A: a
		] newProcess.
	p resume.
	"now let the process run and make sure all unwind blocks have finished"
	Processor yield.
	self assert: p isTerminated.
	self assert: a first & a third.
	self deny: a second | a fourth.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test5BTerminate [
	"Terminate active process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a |
	a _ Array new: 4 withAll: false.
	p := [
			self aux5B: a
		] newProcess.
	p resume.
	"now let the process run and make sure all unwind blocks have finished"
	Processor yield.
	self assert: p isTerminated.
	self assert: a first & a second & a third.
	self deny: a fourth.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test5CTerminate [
	"Terminate active process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a |
	a _ Array new: 4 withAll: false.
	p := [
			self aux5C: a
		] newProcess.
	p resume.
	"now let the process run and make sure all unwind blocks have finished"
	Processor yield.
	self assert: p isTerminated.
	self assert: a first & a second & a third.
	self deny: a fourth.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test5DTerminate [
	"Terminate active process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a |
	a _ Array new: 4 withAll: false.
	p := [
			self aux5D: a
		] newProcess.
	p resume.
	"now let the process run and make sure all unwind blocks have finished"
	Processor yield.
	self assert: p isTerminated.
	self assert: a first & a second & a third.
	self deny: a fourth.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test6ATerminate [
	"Terminate active process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a |
	a _ Array new: 4 withAll: false.
	p := [
			self aux6A: a
		] newProcess.
	p resume.
	"now let the process run and make sure all unwind blocks have finished"
	Processor yield.
	self assert: p isTerminated.
	self assert: a first & a third.
	self deny: a second | a fourth.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test6BTerminate [
	"Terminate active process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a |
	a _ Array new: 4 withAll: false.
	p := [
			self aux6B: a
		] newProcess.
	p resume.
	"now let the process run and make sure all unwind blocks have finished"
	Processor yield.
	self assert: p isTerminated.
	self assert: a first & a second & a third.
	self deny: a fourth.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test6CTerminate [
	"Terminate active process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a |
	a _ Array new: 4 withAll: false.
	p := [
			self aux6C: a
		] newProcess.
	p resume.
	"now let the process run and make sure all unwind blocks have finished"
	Processor yield.
	self assert: p isTerminated.
	self assert: a first & a second & a third.
	self deny: a fourth.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test6DTerminate [
	"Terminate active process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a |
	a _ Array new: 4 withAll: false.
	p := [
			self aux6D: a
		] newProcess.
	p resume.
	"now let the process run and make sure all unwind blocks have finished"
	Processor yield.
	self assert: p isTerminated.
	self assert: a first & a second & a third.
	self deny: a fourth.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test7AResume [
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux7A: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	semaphore signal.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a third & a fourth description: 'assert #4 failed'.
	self deny: a second description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test7ATerminate [
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux7A: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a third description: 'assert #4 failed'.
	self deny: a second | a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test7BResume [
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux7B: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	semaphore signal.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test7BTerminate [
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux7B: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test7CResume [
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux7C: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	semaphore signal.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test7CTerminate [
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux7C: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test7DResume [
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux7D: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	semaphore signal.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test7DTerminate [
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux7D: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test8AResume [
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux8A: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	semaphore signal.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a third & a fourth description: 'assert #4 failed'.
	self deny: a second description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test8ATerminate [
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux8A: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a third description: 'assert #4 failed'.
	self deny: a second | a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test8BResume [
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux8B: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	semaphore signal.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test8BTerminate [
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux8B: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test8CResume [
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux8C: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	semaphore signal.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test8CTerminate [
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux8C: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test8DResume [
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux8D: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now resume the process and make sure all unwind blocks have finished"
	semaphore signal.
	[ p isReady ] whileTrue: [ Processor yield ].
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third & a fourth description: 'assert #4 failed'.
]

{ #category : 'testing' }
ProcessTerminateUnwindNonLocalReturn >> test8DTerminate [
	"Terminate blocked process.
	Test all nested unwind blocks are correctly unwound; 
	all unwind blocks halfway 	through their execution should be completed."

	| p a semaphore s |
	semaphore := Semaphore new.	
	a := Array new: 4 withAll: false.
	s := Array with: false.
	p := [
			self aux8D: a semaphore: semaphore sentinel: s
		] newProcess.
	p resume.
	"Usually #resume returns before actually resuming the process.
	But sometimes it may have actually activated it."
	[s first] whileFalse: [
		Processor yield ].
	"make sure p is blocked and none of the unwind blocks has finished yet"
	self assert: p isBlocked description: 'assert #1 failed'.
	self assert: (a noneSatisfy: [ :b | b ]) description: 'assert #2 failed'.
	"now terminate the process and make sure all unwind blocks have finished"
	p terminate.
	self assert: p isTerminated description: 'assert #3 failed'.
	self assert: a first & a second & a third description: 'assert #4 failed'.
	self deny: a fourth description: 'assert #5 failed'.
]
