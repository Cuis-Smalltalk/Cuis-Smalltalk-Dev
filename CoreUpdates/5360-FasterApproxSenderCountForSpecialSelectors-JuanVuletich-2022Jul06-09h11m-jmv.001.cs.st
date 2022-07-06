'From Cuis 6.0 [latest update: #5359] on 6 July 2022 at 9:11:34 am'!

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 7/6/2022 09:02:50'!
numberOfSendersOf: aSymbol
	"Answer the count of all the methods that call on aLiteral.
	[ (Smalltalk numberOfSendersOf: #open:label:) ] timeToRun
	[ (Smalltalk numberOfSendersOf: #==) ] timeToRun
	"
	| count specialFlag specialByte |
	count _ 0.
	specialFlag _ self hasSpecialSelector: aSymbol ifTrueSetByte: [ :b | specialByte _ b ].
	self allBehaviorsDo: [ :class |
		class selectorsAndMethodsDo: [ :sel :method |
			((method hasLiteral: aSymbol) or: [specialFlag and: [(method scanFor: specialByte)]])
				ifTrue: [ count _ count + 1 ]]].
	^ count! !

