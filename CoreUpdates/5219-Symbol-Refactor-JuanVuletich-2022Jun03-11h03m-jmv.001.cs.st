'From Cuis 6.0 [latest update: #5218] on 3 June 2022 at 11:04:09 am'!

!String class methodsFor: 'system' stamp: 'jmv 6/3/2022 10:29:45'!
symbolClassToUse
	"Answer the class to create symbols from us"
	^Symbol! !


!Symbol class methodsFor: 'private' stamp: 'jmv 6/3/2022 10:28:45'!
basicFromString: aString
	"Private use. Answers an instance that is not yet interned.
	Can break the Symbol invariant: forAllSymbols, (s1 = s2) => (s1 == s2)
	Please don't call lightly."

	^(self basicNew: aString size) string: aString! !


!Symbol methodsFor: 'comparing' stamp: 'jmv 6/3/2022 10:34:38'!
= another

	"Use == between two symbols..."
	self == another ifTrue: [
		^ true].  "Was == "
	another isSymbol ifTrue: [
		^ false].  "Was not ==, therefore can't be = "

	"Otherwise use string =..."
	^ super = another! !


!Symbol class methodsFor: 'instance creation' stamp: 'jmv 6/3/2022 10:40:03'!
intern: aStringOrSymbol

	^(self lookup: aStringOrSymbol)
		ifNil:
			[
				NewSymbols add:
					(aStringOrSymbol isSymbol
						ifTrue: [ aStringOrSymbol ]
						ifFalse: [ aStringOrSymbol class symbolClassToUse basicFromString: aStringOrSymbol ])
			]! !

!methodRemoval: Symbol #is: stamp: 'jmv 6/3/2022 11:03:51'!
Symbol removeSelector: #is:!
