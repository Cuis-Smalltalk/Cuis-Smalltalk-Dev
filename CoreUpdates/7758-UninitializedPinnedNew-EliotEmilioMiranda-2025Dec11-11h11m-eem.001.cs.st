'From Cuis7.5 [latest update: #7659] on 11 December 2025 at 11:09:35 am'!

!Behavior methodsFor: 'instance creation' stamp: 'eem 12/11/2025 11:07:17'!
uninitializedPinnedNew: sizeRequested
	"Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested. In contrast with basicNew:
	 do not initialize the new instance with zeros (because pointer objects must
	 be initialized the primitive fails for all pointer classes). Fail if this class is not
	 bits indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Optional.
	
	 If the primitive fails because space is low then the scavenger will run before the
	 method is activated.  Check args and retry via handleFailingBasicNew: if they're OK."

	<primitive: 583 error: ec>
	(ec == #'insufficient object memory' or: [ec == #'bad argument']) ifTrue:
		[^self handleFailingUninitializedPinnedNew: sizeRequested].
	ec ifNil:
		[^(self basicNew: sizeRequested) pin; yourself].
	self isVariable ifFalse:
		[self error: self printString, ' cannot have variable sized instances'].
	self isBits ifFalse:
		[self error: self printString, ' cannot be instantiated with uninitialized contents'].
	self primitiveFailed! !

!Behavior methodsFor: 'private' stamp: 'eem 12/11/2025 11:09:22'!
handleFailingFailingUninitializedPinnedNew: sizeRequested
	"This uninitializedPinnedNew: gets sent after handleFailingUninitializedPinnedNew: has
	 done a full garbage collection and possibly grown memory.  If this uninitializedPinnedNew:
	 fails then the system really is low on space, so raise the OutOfMemory signal.

	 Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested. In contrast with basicNew:
	 do not initialize the new instance with zeros. Fail if this class is not
	 bits indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Optional."
	<primitive: 583>
	(sizeRequested isInteger and: [ sizeRequested > 0 ])
		ifTrue: [ OutOfMemory signal ]
		ifFalse: [ self error: 'sizeRequested must be a positive integer' ].
	^ self uninitializedPinnedNew: sizeRequested"retry if user proceeds".! !

!Behavior methodsFor: 'private' stamp: 'eem 12/11/2025 11:09:07'!
handleFailingUninitializedPinnedNew: sizeRequested
	"handleFailingUninitializedPinnedNew: gets sent after uninitializedPinnedNew: has
	 failed and allowed a scavenging garbage collection to occur.  The scavenging collection
	 will have happened as the VM is activating the (failing) uninitializedPinnedNew:.  If
	 handleFailingUninitializedPinnedNew: fails then the scavenge failed to reclaim sufficient
	 space and a global garbage collection is required.  Retry after garbage
	 collecting and growing memory if necessary.

	 Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested. In contrast with basicNew:
	 do not initialize the new instance with zeros. Fail if this class is not
	 bits indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Optional."

	<primitive: 583>
	| bytesRequested |
	bytesRequested := self byteSizeOfInstanceOfSize: sizeRequested.
	Smalltalk garbageCollect < bytesRequested ifTrue:
		[Smalltalk growMemoryByAtLeast: bytesRequested].
	"retry after global garbage collect and possible grow"
	^self handleFailingFailingUninitializedPinnedNew: sizeRequested! !
