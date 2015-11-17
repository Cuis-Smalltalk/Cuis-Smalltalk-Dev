'From Cuis 4.2 of 25 July 2013 [latest update: #2571] on 15 November 2015 at 4:11:06.452637 am'!

!Bag methodsFor: 'adding' stamp: 'len 6/1/1998 03:21'!
addAll: aCollection
	"Refer to the comment in Collection|addAll:."

	(aCollection isKindOf: Bag) ifFalse: [^ super addAll: aCollection].
	aCollection contents associationsDo: [ :each |
		self add: each key withOccurrences: each value].
	^ aCollection! !

!Bag methodsFor: 'enumerating' stamp: 'len 6/1/1998 03:23'!
detect: aBlock ifNone: exceptionBlock 
	"Refer to the comment in Collection|detect:ifNone:."

	contents keysDo: [:each | (aBlock value: each) ifTrue: [^ each]].
	^ exceptionBlock value! !

!Bag methodsFor: 'enumerating' stamp: 'len 6/1/1998 03:25'!
select: aBlock 
	"Refer to the comment in Collection|select:."

	| newCollection |
	newCollection _ self species new.
	contents associationsDo: [:each |
		(aBlock value: each key)
			ifTrue: [newCollection add: each key withOccurrences: each value]].
	^ newCollection! !

!Bag methodsFor: 'private'!
contents
	^contents
! !

