'From Cuis 6.0 [latest update: #5346] on 2 July 2022 at 4:46:25 pm'!

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 7/2/2022 16:45:28'!
newProcessIfUI: suspendedProcess
	"Answer the UI we created a new process for.
	If not an UI process, still guarantee that the UI process is runnable."

	suspendedProcess animatedUI ifNotNil: [ :guiRootObject |
		self spawnNewMorphicProcessFor: guiRootObject.
		^guiRootObject ].
	UISupervisor newUIProcessIfNeeded ifNotNil: [ :oldUIProcess |
		oldUIProcess isTerminated ifFalse: [
			oldUIProcess terminate ]].
	^nil
	"
	|s|
	s := Semaphore new.
	[1/0. s signal] fork.
	s wait
	"! !

