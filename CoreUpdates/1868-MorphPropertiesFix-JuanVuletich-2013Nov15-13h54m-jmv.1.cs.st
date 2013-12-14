'From Cuis 4.2 of 25 July 2013 [latest update: #1867] on 15 November 2013 at 1:58:04.676818 pm'!

!Morph methodsFor: 'accessing - properties' stamp: 'jmv 11/13/2013 08:24'!
valueOfProperty: aSymbol ifPresentDo: aBlock
	"If the receiver has a property of the given name, evaluate
	aBlock on behalf of the value of that property"
	properties ifNil: [^ self ].
	^ aBlock
		value: (properties at: aSymbol ifAbsent: [ ^ self ])! !

