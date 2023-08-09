'From Cuis 6.0 [latest update: #5976] on 9 August 2023 at 11:51:53 am'!

!SystemDictionary methodsFor: 'memory space' stamp: 'jmv 8/9/2023 11:47:49'!
lowSpaceWatcher
	"Wait until the low space semaphore is signalled, then take appropriate actions."

	| p |
	self garbageCollectMost <= self lowSpaceThreshold ifTrue: [
		self garbageCollect <= self lowSpaceThreshold ifTrue: [
			"free space must be above threshold before starting low space watcher"
			^ Smalltalk primitiveBeep ]].

	LowSpaceSemaphore := Semaphore new.
	self primLowSpaceSemaphore: LowSpaceSemaphore.
	self primSignalAtBytesLeft: self lowSpaceThreshold.  "enable low space interrupts"

	LowSpaceSemaphore wait.  "wait for a low space condition..."

	self primSignalAtBytesLeft: 0.  "disable low space interrupts"
	self primLowSpaceSemaphore: nil.
	LowSpaceProcess := nil.
	"Note: user now unprotected until the low space watcher is re-installed"

	p := (Smalltalk specialObjectsArray at: 23) ifNil: [
		"In case the LowSpaceSemaphore is signaled not by the VM but by someone else."
		Processor preemptedProcess ].
	Smalltalk specialObjectsArray at: 23 put: nil.
	p isTerminated ifFalse: [
		p suspend.
		Debugger openProcess: p context: p suspendedContext label: 'Space is low' ]! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	Smalltalk installLowSpaceWatcher!

