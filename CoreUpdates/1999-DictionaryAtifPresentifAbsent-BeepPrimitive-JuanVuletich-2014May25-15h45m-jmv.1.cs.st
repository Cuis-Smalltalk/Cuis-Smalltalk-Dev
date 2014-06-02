'From Cuis 4.2 of 25 July 2013 [latest update: #1996] on 25 May 2014 at 3:57:40.828987 pm'!

!Dictionary methodsFor: 'accessing' stamp: 'jmv 5/25/2014 15:54'!
at: key ifPresent: presentBlock ifAbsent: absentBlock
	"Answer the value associated with the key or, if key isn't found,
	answer the result of evaluating aBlock."
	"
		Smalltalk at: #zork ifPresent: [ :cls | (cls name, ' present') print ] ifAbsent: [ 'zork absent' print ]
		Smalltalk at: #Number ifPresent: [ :cls | (cls name, ' present') print ] ifAbsent: [ 'Number absent' print ]
	"

	| assoc |
	assoc _ array at: (self findElementOrNil: key).
	assoc ifNil: [ ^ absentBlock value ].
	^ presentBlock value: assoc value! !


!SystemDictionary methodsFor: 'ui' stamp: 'jmv 5/25/2014 15:49'!
primitiveBeep
	"Make a primitive beep. Not to be called directly."
	"
	Smalltalk primitiveBeep
	"

	<primitive: 140>
	self primitiveFailed! !


!Dictionary methodsFor: 'accessing' stamp: 'jmv 5/25/2014 15:51'!
at: key ifAbsent: absentBlock
	"Answer the value associated with the key or, if key isn't found,
	answer the result of evaluating aBlock."

	| assoc |
	assoc _ array at: (self findElementOrNil: key).
	assoc ifNil: [ ^ absentBlock value ].
	^ assoc value! !

!Dictionary methodsFor: 'accessing' stamp: 'jmv 5/25/2014 15:51'!
at: key ifPresent: presentBlock
	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| v |
	v _ self at: key ifAbsent: [ ^ nil ].
	^ presentBlock value: v
! !


!SystemDictionary methodsFor: 'memory space' stamp: 'jmv 5/25/2014 15:56'!
lowSpaceWatcher
	"Wait until the low space semaphore is signalled, then take appropriate actions."

	self garbageCollectMost <= self lowSpaceThreshold ifTrue: [
		self garbageCollect <= self lowSpaceThreshold ifTrue: [
			"free space must be above threshold before starting low space watcher"
			^ Smalltalk primitiveBeep ]].

	LowSpaceSemaphore _ Semaphore new.
	self primLowSpaceSemaphore: LowSpaceSemaphore.
	self primSignalAtBytesLeft: self lowSpaceThreshold.  "enable low space interrupts"

	LowSpaceSemaphore wait.  "wait for a low space condition..."

	self primSignalAtBytesLeft: 0.  "disable low space interrupts"
	self primLowSpaceSemaphore: nil.
	LowSpaceProcess _ nil.
	"Note: user now unprotected until the low space watcher is re-installed"

	ProjectX currentInterruptNameX: 'Space is low'! !

!SystemDictionary methodsFor: 'ui' stamp: 'jmv 5/25/2014 15:54'!
beep
	"
	Smalltalk beep
	"
	Preferences soundsEnabled ifTrue: [
		Smalltalk
			at: #SampledSound
			ifPresent: [ :cls | cls beep ]
			ifAbsent: [ self primitiveBeep ]]! !

