'From Cuis7.9 [latest update: #8210] on 23 September 2026 at 3:38:51 pm'!

!MethodDefinitionChange methodsFor: 'accessing' stamp: 'jmv 23/Sep/2026, 6:33:25 pm (UTC)'!
changeClassName

	^meta
		ifTrue: [ class, ' class' ]
		ifFalse: [ class ]! !

