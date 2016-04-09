'From Cuis 4.2 of 25 July 2013 [latest update: #2731] on 9 April 2016 at 7:39:28.900476 pm'!

!Number methodsFor: 'intervals'!
to: stop
	"Answer an Interval from the receiver up to the argument, stop, 
	incrementing by 1."

	^Interval from: self to: stop by: 1! !

!Number methodsFor: 'intervals'!
to: stop by: step
	"Answer an Interval from the receiver up to the argument, stop, 
	incrementing by step."

	^Interval from: self to: stop by: step! !


!TextModelMorph methodsFor: 'model access' stamp: 'jmv 1/4/2012 15:23'!
setSelection: sel

	sel == #all
		ifTrue: [ self editor selectAll ]
		ifFalse: [
			sel
				ifNil: [ self editor selectFrom:1 to: 0 ]
				ifNotNil: [ self editor selectFrom: sel first to: sel last ]].
	self scrollSelectionIntoView! !

!methodRemoval: Array #rangeIncludes:!
Array removeSelector: #rangeIncludes:!
