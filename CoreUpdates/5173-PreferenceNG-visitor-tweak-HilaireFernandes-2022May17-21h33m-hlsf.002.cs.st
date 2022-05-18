'From Cuis 6.0 [latest update: #5171] on 17 May 2022 at 9:49:40 pm'!

!PreferenceNG methodsFor: 'accessing' stamp: 'hlsf 5/17/2022 21:49:10'!
accept: aVisitor
	| visitMethod |
	self isTypeAClass 
		ifTrue: [ visitMethod _ ('visit', type name, 'Preference:') asSymbol ]
		ifFalse: [visitMethod _ ('visit', type species asString, 'Preference:') asSymbol ].
	^ aVisitor perform: visitMethod with: self! !

