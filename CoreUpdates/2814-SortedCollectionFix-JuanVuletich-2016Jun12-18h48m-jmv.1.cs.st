'From Cuis 4.2 of 25 July 2013 [latest update: #2811] on 12 June 2016 at 6:50:47.836693 pm'!

!SortedCollection methodsFor: 'accessing' stamp: 'jmv 6/12/2016 18:50'!
indexOf: anElement startingAt: start ifAbsent: exceptionBlock
	| sortBlockToUse firstCandidate |
	sortBlockToUse _ sortBlock ifNil: [[:a :b | a <= b ]].
	"if first element to check is it, answer so."
	firstCandidate _ self at: start.
	firstCandidate = anElement ifTrue: [ ^ start ].
	"if first element to check is already too late, answer 'NotHere' "
	(sortBlockToUse value: anElement value: firstCandidate) ifTrue: [
		^ exceptionBlock value ].
	"Ok. Look for it."
	^self
		findBinaryIndex: [ :some | 
			some = anElement
				ifTrue: [0]
				ifFalse: [ (sortBlockToUse value: anElement value: some) ifTrue: [-1] ifFalse: [1]]]
		do: [ :i | i ]
		ifNone: [ :i1 :i2 | exceptionBlock value ]! !

