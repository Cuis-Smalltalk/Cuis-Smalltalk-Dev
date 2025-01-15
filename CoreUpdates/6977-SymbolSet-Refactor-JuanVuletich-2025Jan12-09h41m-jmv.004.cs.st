'From Cuis7.3 [latest update: #6976] on 12 January 2025 at 9:46:33 am'!

!SymbolSet methodsFor: 'lookup' stamp: 'jmv 1/10/2025 11:35:59'!
add: anObject
	"Include anObject as one of the receiver's elements, but only if
	not already present. Answer anObject."
	
	| hash |
	hash := anObject hash.
	(self basicLookup: anObject withHash: hash) ifNil: [
		self basicAdd: anObject withHash: hash ].
	^anObject! !

!SymbolSet methodsFor: 'lookup' stamp: 'jmv 1/10/2025 11:54:12'!
lookup: anObject ifAbsentAddFrom: aBlock
	"Look for an element equal to anObject.
	If found, answer it.
	If not found, evaluate aBlock, add the result and answer it."
	
	| newObject |
	^(self lookup: anObject) ifNil: [
		newObject := aBlock value.
		self basicAdd: newObject withHash: newObject hash.
		newObject ].! !


!Symbol class methodsFor: 'instance creation' stamp: 'jmv 1/10/2025 11:53:15'!
intern: aStringOrSymbol

	^self accessProtect critical:
		[
			Symbols lookup: aStringOrSymbol ifAbsentAddFrom:
				[
					| preferByteStringForAscii |
					aStringOrSymbol isSymbol
						ifTrue: [ aStringOrSymbol ]
						ifFalse: [
							preferByteStringForAscii := aStringOrSymbol.
							(aStringOrSymbol isUnicodeString and: [ aStringOrSymbol isAscii ])
								ifTrue: [preferByteStringForAscii := aStringOrSymbol asByteString].
							preferByteStringForAscii class symbolClassToUse basicFromString: preferByteStringForAscii ].
				]
			].! !


!SymbolSet methodsFor: 'private' stamp: 'jmv 1/10/2025 11:16:10'!
rehashSymbolClass: aClass
"quisiera eliminar"
	aClass allInstances do:
		[:symbol | self basicInternNew: symbol withHash: symbol hash]! !

!SymbolSet methodsFor: 'private - lookup' stamp: 'jmv 1/10/2025 11:16:05'!
basicInternNew: aStringOrSymbol withHash: anInteger
"quisiera eliminar"
	| interned preferByteStringForAscii |
	interned := aStringOrSymbol isSymbol
		ifTrue: [ aStringOrSymbol ]
		ifFalse: [
			preferByteStringForAscii := aStringOrSymbol.
			(aStringOrSymbol isUnicodeString and: [ aStringOrSymbol isAscii ])
				ifTrue: [preferByteStringForAscii := aStringOrSymbol asByteString].
			preferByteStringForAscii class symbolClassToUse basicFromString: preferByteStringForAscii ].
	self basicAdd: interned withHash: anInteger.
	^interned! !

!methodRemoval: SymbolSet #basicIntern:withHash: stamp: 'jmv 1/12/2025 09:45:29'!
SymbolSet removeSelector: #basicIntern:withHash:!
!methodRemoval: SymbolSet #intern: stamp: 'jmv 1/12/2025 09:45:22'!
SymbolSet removeSelector: #intern:!
