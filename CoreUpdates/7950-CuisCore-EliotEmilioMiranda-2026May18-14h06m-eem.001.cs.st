'From Cuis7.7 [latest update: #7925] on 18 May 2026 at 2:36:30 pm'!

!Behavior methodsFor: 'instance creation' stamp: 'eem 18/May/2026 14:06:39'!
basicPinnedNew: sizeRequested
	"Primitive. Answer an instance of this class with the number of indexable variables
	 specified by the argument, sizeRequested, that is pinned in old space.  Fail if this
	 class is not indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Essential. See Object documentation whatIsAPrimitive.
	
	 If the primitive fails because space is low then the scavenger will run before the
	 method is activated.  Check args and retry via handleFailingBasicNew: if they're OK."

	<primitive: 581 error: ec>
	(ec == #'insufficient object memory' or: [ec == #'bad argument']) ifTrue:
		[^self handleFailingBasicPinnedNew: sizeRequested].
	self isVariable ifFalse:
		[self error: self printString, ' cannot have variable sized instances'].
	self primitiveFailed! !

!Behavior methodsFor: 'private' stamp: 'eem 18/May/2026 14:36:04'!
handleFailingBasicPinnedNew: sizeRequested
	"handleFailingBasicNew: gets sent after basicPinnedNew: has failed and
	 allowed a scavenging garbage collection to occur.  But since a pinned
	 object lives in old space the scavenge is irrelevant. So retry after
	 garbage collecting and growing memory if necessary.

	 Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested.  Fail if this class is not
	 indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 581>
	| bytesRequested |
	bytesRequested := self byteSizeOfInstanceOfSize: sizeRequested.
	Smalltalk garbageCollect < bytesRequested ifTrue:
		[Smalltalk growMemoryByAtLeast: bytesRequested].
	"retry after global garbage collect and possible grow"
	^self handleFailingFailingBasicPinnedNew: sizeRequested! !

!Behavior methodsFor: 'private' stamp: 'eem 18/May/2026 14:33:40'!
handleFailingFailingBasicPinnedNew: sizeRequested
	"This basicPinnedNew: gets sent after handleFailingBasicPinnedNew: has done a full
	 garbage collection and possibly grown memory.  If this basicPinnedNew: fails
	 then the system really is low on space, so raise the OutOfMemory signal.

	 Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested.  Fail if this class is not
	 indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Essential. See Object documentation whatIsAPrimitive."
	"space must be low."
	<primitive: 581>
	(sizeRequested isInteger and: [ sizeRequested > 0 ])
		ifTrue: [ OutOfMemory signal ]
		ifFalse: [ self error: 'sizeRequested must be a positive integer' ].
	^ self basicPinnedNew: sizeRequested"retry if user proceeds".! !

