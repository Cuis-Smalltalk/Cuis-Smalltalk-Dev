'From Cuis 6.0 [latest update: #5644] on 21 January 2023 at 12:18:43 pm'!

!ContextPart methodsFor: 'query' stamp: 'jar 1/20/2023 20:17:51'!
isDone
	"Answer if the receiver has no unexecuted code left.
	Written carefully to avoid suspension points.
	See https://lists.cuis.st/mailman/archives/cuis-dev/2023-January/006814.html
	See https://lists.cuis.st/mailman/archives/cuis-dev/2023-January/006818.html"

	^pc
		ifNil: [ true ]
		ifNotNil: [ :aPC | (self atEnd: aPC) and: [
			sender
				ifNil: [ true ]
				ifNotNil: [ :ctx | ctx isDone]]]! !

