'From Cuis7.9 [latest update: #8309] on 3 November 2026 at 10:41:04 am'!

!EncoderForSistaV1 class methodsFor: 'behavior support' stamp: 'eem 27/Feb/2025, 10:40:00 am (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel05ClassAndMethodCreation}!
maxAccessibleInstSize
	"Answer the maximum number of accessible named instance variables supported by the bytecode set.
	C.f. genPushInstVarLong:, genStoreInstVarLong:, and genStorePopInstVarLong:"
	^65536! !


!EncoderForV3PlusClosures class methodsFor: 'behavior support' stamp: 'eem 27/Feb/2025, 10:41:00 am (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel05ClassAndMethodCreation}!
maxAccessibleInstSize
	"Answer the maximum number of accessible named instance variables supported by the bytecode set.
	C.f. genPushInstVarLong:, genStoreInstVarLong:, and genStorePopInstVarLong:"
	^256! !


!SmallInteger methodsFor: 'bit manipulation' stamp: 'nice 31/Aug/2019, 1:06:00 am (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
highBitOfMagnitude
	"Answer the index of the high order bit of the receiver, or zero if the  
	receiver is zero.
	If the receiver is negative, answer the high bit of the receiver negated."
	<primitive: 575>
	self < 0 ifTrue: [^self negated highBit].
	^super highBitOfMagnitude! !


!SmallInteger class methodsFor: 'instance creation' stamp: 'tk 20/Apr/1999, 5:17:00 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
basicNew

	self error: 'SmallIntegers can only be created by performing arithmetic'! !

!SmallInteger class methodsFor: 'instance creation' stamp: 'tk 20/Apr/1999, 5:18:00 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
new

	self basicNew	"generates an error"! !


!Character methodsFor: 'printing' stamp: 'eem 3/Apr/2017, 6:21:00 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
printAsLiteralOn: aStream
	aStream nextPut: $$; nextPut: self! !

!Character methodsFor: 'printing' stamp: 'eem 24/Jan/2025, 1:24:58 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
printOn: aStream
	| integerValue |
	((integerValue := self codePoint) > 32 and: [integerValue ~= 127]) ifTrue:
		[^self printAsLiteralOn: aStream].
	(self class constantNameFor: self)
		ifNotNil: [ :name | aStream nextPutAll: 'Character '; nextPutAll: name ]
		ifNil: [ aStream nextPutAll: 'Character value: '; print: integerValue ]! !


!Character class methodsFor: 'private' stamp: 'eem 23/Jan/2025, 9:03:34 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
constantNameFor: aCharacter
	self constantNames do:
		[ :constantName |
		(self perform: constantName) == aCharacter ifTrue:
			[^constantName]].
	^nil! !

!Character class methodsFor: 'private' stamp: 'jmv 3/Nov/2026, 1:05:28 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
constantNames
	NamedCharactersMap ifNil: [ ^#() ].
	^ #(backQuote backspace cr delete doubleQuote end escape home insert lf newPage noBreakSpace null pageUp singleQuote smalltalkDownArrow smalltalkLeftArrow smalltalkRightArrow smalltalkUpArrow space tab zeroWidthSpace)! !

