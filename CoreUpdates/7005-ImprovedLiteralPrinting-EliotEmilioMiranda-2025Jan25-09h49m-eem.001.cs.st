'From Cuis7.3 [latest update: #7004] on 25 January 2025 at 10:14:01 am'!

!Object methodsFor: 'testing' stamp: 'ul 11/23/2010 13:28'!
shouldBePrintedAsLiteral

	^self isLiteral! !

!Object methodsFor: 'testing' stamp: 'eem 1/25/2025 09:51:32'!
shouldBePrintedAsLiteralVisiting: aSet
	"aSet is for detecting circular references.
	See implementors and senders."
	^self isLiteral! !


!Array methodsFor: 'printing' stamp: 'eem 4/3/2017 15:42'!
printAsBraceFormOn: aStream
	aStream nextPut: ${.
	self do: [:el | el printOn: aStream] separatedBy: [ aStream nextPutAll: ' . '].
	aStream nextPut: $}! !

!Array methodsFor: 'printing' stamp: 'eem 4/3/2017 15:35'!
printAsLiteralOn: aStream
	aStream nextPut: $#; nextPut: $(.
	self do: [:each| each printAsLiteralOn: aStream] separatedBy: [aStream space].
	aStream nextPut: $)
! !

!Array methodsFor: 'testing' stamp: 'eem 1/25/2025 09:52:21'!
shouldBePrintedAsLiteral
	"Detect circular references."
	^self class == Array
	  and: [self shouldBePrintedAsLiteralVisiting: (IdentitySet newWithRoomForMoreThan: 8)]! !

!Array methodsFor: 'testing' stamp: 'eem 11/2/2011 11:01'!
shouldBePrintedAsLiteralVisiting: aSet
	self class == Array ifFalse:
		[^false].
	(aSet includes: self) ifTrue:
		[^false].
	aSet add: self.
	^self allSatisfy: [:each | each shouldBePrintedAsLiteralVisiting: aSet]! !


!ByteArray methodsFor: 'printing' stamp: 'eem 4/3/2017 15:18'!
printAsLiteralOn: aStream
	aStream nextPut: $#; nextPut: $[.
	self do: [:each| each storeOn: aStream]
		separatedBy: [aStream nextPut: $ ].
	aStream nextPut: $]! !


!String methodsFor: 'printing' stamp: 'eem 4/3/2017 15:30'!
printAsLiteralOn: aStream
	"Print inside string quotes, doubling embedded quotes."
	self storeOn: aStream! !


!Boolean methodsFor: 'printing' stamp: 'eem 4/3/2017 15:44'!
printAsLiteralOn: aStream
	^self printOn: aStream! !


!Float methodsFor: 'printing' stamp: 'eem 4/3/2017 15:27'!
printAsLiteralOn: aStream
	^self storeOn: aStream base: 10! !


!Integer methodsFor: 'printing' stamp: 'eem 4/3/2017 15:26'!
printAsLiteralOn: aStream
	^self printOn: aStream base: 10! !


!UndefinedObject methodsFor: 'printing' stamp: 'eem 4/3/2017 15:32'!
printAsLiteralOn: aStream
	^self printOn: aStream! !


!Character methodsFor: 'testing' stamp: 'eem 1/24/2025 10:54:35'!
shouldBePrintedAsLiteral

	| integerValue |
	(integerValue := self codePoint) < 33 ifTrue: [ ^false ].
	^integerValue ~= 127! !

!Character methodsFor: 'testing' stamp: 'eem 11/2/2011 11:03'!
shouldBePrintedAsLiteralVisiting: aSet

	^self shouldBePrintedAsLiteral! !

!Character methodsFor: 'printing' stamp: 'eem 4/3/2017 15:21'!
printAsLiteralOn: aStream
	aStream nextPut: $$; nextPut: self! !


!Character class methodsFor: 'private' stamp: 'eem 1/23/2025 18:03:34'!
constantNameFor: aCharacter
	self constantNames do:
		[ :constantName |
		(self perform: constantName) == aCharacter ifTrue:
			[^constantName]].
	^nil! !

!Character class methodsFor: 'private' stamp: 'eem 1/23/2025 18:02:02'!
constantNames
	^ #(backQuote backspace cr delete doubleQuote end enter escape home insert lf newPage noBreakSpace null pageUp singleQuote smalltalkDownArrow smalltalkLeftArrow smalltalkRightArrow smalltalkUpArrow space tab zeroWidthSpace)! !


!Object methodsFor: 'printing' stamp: 'eem 1/24/2025 10:35:19'!
isLiteral
	"Answer whether the receiver has a literal text form recognized by the compiler, and
	 preferrably typeable by a human user. The literal form must be provided by #storeOn:"

	^false! !


!Array methodsFor: 'printing' stamp: 'eem 1/25/2025 09:58:52'!
printOn: aStream
	self class == Array ifFalse:
		[^super printOn: aStream].
	self == Smalltalk specialObjectsArray ifTrue:
		[^aStream nextPutAll: 'Smalltalk specialObjectsArray' ].
	self shouldBePrintedAsLiteral
		ifTrue: [self printAsLiteralOn: aStream]
		ifFalse: [self printAsBraceFormOn: aStream]! !

!Array methodsFor: 'printing' stamp: 'eem 4/3/2017 15:42'!
storeOn: aStream
	"Use the literal form if possible."
	self shouldBePrintedAsLiteral
		ifTrue:  [self printAsLiteralOn: aStream]
		ifFalse: [super storeOn: aStream]! !


!ByteArray methodsFor: 'printing' stamp: 'eem 4/3/2017 15:20'!
printOn: aStream
	self shouldBePrintedAsLiteral ifFalse:
		[super printOn: aStream.
		 aStream space].
	self printAsLiteralOn: aStream! !

!ByteArray methodsFor: 'printing' stamp: 'eem 4/3/2017 15:19'!
storeOn: aStream
	self shouldBePrintedAsLiteral
		ifTrue: [self printAsLiteralOn: aStream]
		ifFalse: [super storeOn: aStream]! !


!Character methodsFor: 'printing' stamp: 'eem 1/24/2025 10:24:58'!
printOn: aStream
	| integerValue |
	((integerValue := self codePoint) > 32 and: [integerValue ~= 127]) ifTrue:
		[^self printAsLiteralOn: aStream].
	(self class constantNameFor: self)
		ifNotNil: [ :name | aStream nextPutAll: 'Character '; nextPutAll: name ]
		ifNil: [ aStream nextPutAll: 'Character value: '; print: integerValue ]! !

!Character methodsFor: 'printing' stamp: 'eem 1/24/2025 10:27:09'!
storeOn: aStream
	"Common character literals are preceded by '$', however special need to be encoded differently: for some this might be done by using one of the shortcut constructor methods for the rest we have to create them by ascii-value."

	self shouldBePrintedAsLiteral ifTrue:
		[^self printAsLiteralOn: aStream].
	(self class constantNameFor: self)
		ifNotNil:
			[ :name | aStream nextPutAll: self class name; space; nextPutAll: name]
		ifNil:
			[aStream nextPutAll: '(Character value: '; print: self codePoint; nextPut: $)]! !

