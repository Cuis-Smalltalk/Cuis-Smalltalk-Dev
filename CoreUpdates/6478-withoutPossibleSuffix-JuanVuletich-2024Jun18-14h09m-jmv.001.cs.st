'From Cuis7.1 [latest update: #6477] on 18 June 2024 at 2:09:31 pm'!

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/18/2024 12:50:58'!
withoutPossibleSuffix: aString
	"
	'Elvis' withoutPossibleSuffix: 'vis'
	'Elvis' withoutPossibleSuffix: 'Paul'
	"
	^(self endsWith: aString)
		ifTrue: [ self copyFrom: 1 to: self size - aString size ]
		ifFalse: [self]! !

