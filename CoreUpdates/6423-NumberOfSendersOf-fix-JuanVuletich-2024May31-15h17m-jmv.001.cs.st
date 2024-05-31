'From Cuis7.1 [latest update: #6421] on 31 May 2024 at 3:19:27 pm'!

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/31/2024 11:39:29'!
allCallsOn: aLiteral   "Smalltalk browseAllCallsOn: #open:label:."
	"Answer a Collection of all the methods that call on aLiteral."
	| aCollection specialFlag specialByte |

	#(23 48 'fred' (new open:label:)) size.
	"Example above should find #open:label:, though it is deeply embedded here."

	aCollection := OrderedCollection new.
	specialFlag := self hasSpecialSelector: aLiteral ifTrueSetByte: [:b | specialByte := b ].
	self allBehaviorsDo: [ :class |
		class addMethodsTo: aCollection thatReferenceTo: aLiteral special: specialFlag byte: specialByte ].
	
	^ aCollection! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/31/2024 11:46:31'!
numberOfSendersOf: aSymbol
	"Answer the count of all the methods that call on aLiteral.
	[ (Smalltalk numberOfSendersOf: #open:label:) ] timeToRun
	[ (Smalltalk numberOfSendersOf: #==) ] timeToRun
	"
	| count specialFlag specialByte |
	count := 0.
	specialFlag := self hasSpecialSelector: aSymbol ifTrueSetByte: [ :b | specialByte := b ].
	self allBehaviorsDo: [ :class |
		count := count + (class whichSelectorsReferTo: aSymbol special: specialFlag byte: specialByte) size ].
	^ count! !

