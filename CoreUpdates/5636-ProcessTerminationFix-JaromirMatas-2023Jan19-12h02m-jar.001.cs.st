'From Cuis 6.0 [latest update: #5635] on 19 January 2023 at 12:08:51 pm'!

!ContextPart methodsFor: 'query' stamp: 'jar 1/19/2023 11:55:19'!
isDone
	"Answer if the receiver has no unexecuted code left.
	Written carefully to avoid suspension points.
	See https://lists.cuis.st/mailman/archives/cuis-dev/2023-January/006814.html"

	^sender ifNil: [self isDeadOrAtEnd] ifNotNil: [:ctx | ctx isDone]! !

