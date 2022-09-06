'From Cuis 6.0 [latest update: #5460] on 27 August 2022 at 12:27:01 am'!

!ContextPart methodsFor: 'query' stamp: 'jar 8/27/2022 00:26:17'!
isDone
	"Answer if the receiver has no unexecuted code left."

	self isDead ifTrue: [^true].
	self isBottomContext ifTrue: [^self atEnd].
	self atEnd ifTrue: [^self sender isDone].
	^false! !


!Process methodsFor: 'testing' stamp: 'jar 8/26/2022 18:17:14'!
isTerminated
	"Answer if the receiver is terminated. A healthy process is considered 
	 terminated if the suspendedContext has no unexecuted code left."

	self isRunning ifTrue: [^ false].
	^suspendedContext isNil or: [suspendedContext isDone]! !

