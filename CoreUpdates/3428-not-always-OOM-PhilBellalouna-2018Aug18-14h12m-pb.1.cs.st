'From Cuis 5.0 of 7 November 2016 [latest update: #3402] on 18 August 2018 at 2:44:44 pm'!

!Behavior methodsFor: 'private' stamp: 'pb 8/18/2018 14:12:12'!
handleFailingFailingBasicNew: sizeRequested
	"This basicNew: gets sent after handleFailingBasicNew: has done a full
	 garbage collection and possibly grown memory.  If this basicNew: fails
	 then the system really is low on space, so raise the OutOfMemory signal.

	 Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested.  Fail if this class is not
	 indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Essential. See Object documentation whatIsAPrimitive."
	"space must be low."
	<primitive: 71>
	(sizeRequested isInteger and: [ sizeRequested > 0 ])
		ifTrue: [ OutOfMemory signal ]
		ifFalse: [ self error: 'sizeRequested must be a positive integer' ].
	^ self basicNew: sizeRequested"retry if user proceeds".! !

