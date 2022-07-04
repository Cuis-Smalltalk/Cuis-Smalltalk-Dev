'From Cuis 6.0 [latest update: #5350] on 4 July 2022 at 5:26:53 pm'!

!UISupervisor class methodsFor: 'services' stamp: 'jar 7/4/2022 17:08:26'!
isUIProcessRunning
	"Is the currently running process the UI Process?"

	^UIProcess isRunning! !


!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 7/4/2022 17:05:34'!
newProcessIfUI: suspendedProcess
	"Answer the UI we created a new process for.
	If not an UI process, still guarantee that the UI process is runnable."

	suspendedProcess animatedUI ifNotNil: [ :guiRootObject |
		self spawnNewMorphicProcessFor: guiRootObject.
		^guiRootObject ].
	^nil
	"
	|s|
	s := Semaphore new.
	[1/0. s signal] fork.
	s wait
	"! !

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 7/4/2022 17:24:14'!
spawnNewMorphicProcessFor: guiRootObject
	"
	Safe to run anytime, as the old Morphic process will end by itself.
	UISupervisor spawnNewMorphicProcessFor: UISupervisor ui.
	"
	| previousUIProcess |
	previousUIProcess _ UIProcess.
	UIProcess _ guiRootObject runProcess.
	previousUIProcess ifNotNil: [ previousUIProcess animatedUI: nil ].
	UIProcess resume! !


!WorldMorph methodsFor: 'interaction loop' stamp: 'jar 7/4/2022 17:07:42'!
mainLoop

	
	self clearWaitDelay.
	canvas isNil ifTrue: [
		self setMainCanvas ].
	self redrawNeeded.
	[
		self doOneCycle.
		UISupervisor isUIProcessRunning ]
			whileTrue: []! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
[
	UISupervisor ui ifNotNil: [ :morphicWorld |
		UISupervisor stopUIProcess.
		UISupervisor spawnNewMorphicProcessFor: morphicWorld ].
] forkAt: ProcessorScheduler userBackgroundPriority!

