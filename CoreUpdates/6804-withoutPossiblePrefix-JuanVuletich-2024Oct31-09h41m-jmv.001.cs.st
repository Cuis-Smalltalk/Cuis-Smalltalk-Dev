'From Cuis7.1 [latest update: #6803] on 31 October 2024 at 9:41:54 am'!

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 10/30/2024 12:59:33'!
withoutPossiblePrefix: aString
	"
	'Elvis' withoutPossiblePrefix: 'El'
	'Elvis' withoutPossiblePrefix: 'Paul'
	"
	^(self beginsWith: aString)
		ifTrue: [ self copyFrom: aString size+1 to: self size ]
		ifFalse: [self]! !

