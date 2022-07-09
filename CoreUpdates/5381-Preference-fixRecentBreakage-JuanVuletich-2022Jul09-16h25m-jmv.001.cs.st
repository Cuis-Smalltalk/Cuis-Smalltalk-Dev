'From Cuis 6.0 [latest update: #5380] on 9 July 2022 at 4:27:37 pm'!

!Preference methodsFor: 'accessing' stamp: 'hlsf 5/17/2022 21:49:10'!
accept: aVisitor
	| visitMethod |
	self isTypeAClass 
		ifTrue: [ visitMethod _ ('visit', type name, 'Preference:') asSymbol ]
		ifFalse: [visitMethod _ ('visit', type species asString, 'Preference:') asSymbol ].
	^ aVisitor perform: visitMethod with: self! !


!Preference methodsFor: 'accessing' stamp: 'jmv 7/9/2022 16:26:16'!
type
	(type == String) ifTrue: [ type _ CharacterSequence ].
	^ type! !

