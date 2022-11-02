'From Cuis 6.0 [latest update: #5546] on 2 November 2022 at 3:15:01 pm'!

!Bag methodsFor: 'statistics' stamp: 'jmv 11/2/2022 15:13:33'!
product: aBlock
	| answer |
	self emptyCheck.
	self withOccurrencesDo: [ :each :times | | v |
		v _ aBlock value: each.
		answer _ answer isNil
			ifTrue: [v raisedTo: times]
			ifFalse: [answer * (v raisedTo: times)]].
	^ answer! !

!Bag methodsFor: 'statistics' stamp: 'jmv 11/2/2022 15:14:23'!
sum: aBlock ifEmpty: emptyBlockOrValue
	| answer |
	self isEmpty ifTrue: [^ emptyBlockOrValue value].
	self withOccurrencesDo: [ :each :times | | v |
		v _ aBlock value: each.
		answer _ answer isNil
			ifTrue: [v * times]
			ifFalse: [v * times + answer]].
	^ answer! !

