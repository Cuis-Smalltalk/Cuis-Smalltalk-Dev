'From Cuis 4.2 of 25 July 2013 [latest update: #1867] on 14 December 2013 at 4:06:37.753403 pm'!

!Morph methodsFor: 'accessing - properties' stamp: 'sqr 12/14/2013 16:05'!
valueOfProperty: aSymbol ifPresentDo: aBlock
	"If the receiver has a property of the given name, evaluate
	aBlock on behalf of the value of that property"

	| value |
	properties ifNil: [^ self ].
	value := self valueOfProperty: aSymbol ifAbsent: [^self].
	^aBlock value: value! !

