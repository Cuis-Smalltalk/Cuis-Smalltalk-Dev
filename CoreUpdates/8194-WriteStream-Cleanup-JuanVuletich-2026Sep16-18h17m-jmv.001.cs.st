'From Cuis7.9 [latest update: #8193] on 16 September 2026 at 6:18:02 pm'!

!LimitedWriteStream methodsFor: 'accessing' stamp: 'jmv 16/Sep/2026, 7:20:26 pm (UTC)'!
nextPut: anObject 
	"Ensure that the limit is not exceeded"

	position >= limit
		ifTrue: [limitBlock value]
		ifFalse: [super nextPut: anObject]! !

