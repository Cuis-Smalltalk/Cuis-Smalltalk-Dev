"
I am a Form that is a possible appearance for a mouse cursor.  My size is always 16x16, ever since the original implementation on the Alto.

Predefined cursors should accessed via #cursorAt: which caches Cursor instances.  For example "Cursor cursorAt: #normalCursorWithMask".  You can also dynamically add your own cursors or modify existing ones via #cursorAt:put: as desired.
"
Class {
	#name : 'Cursor',
	#superclass : 'Form',
	#classVars : [
		'CurrentCursor',
		'CursorDict'
	],
	#category : 'Graphics-Display Objects',
	#package : 'Graphics',
	#tag : 'Display Objects'
}

{ #category : 'instance creation' }
Cursor class >> blankCursor [
	"Answer the instance of me that is all white."
	^ self new.
]

{ #category : 'instance creation' }
Cursor class >> bottomLeftCursor [
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1111111111111100
		2r1111111111111100
		2r0000000000000000
		2r0000000000000000)
	offset: `0@ -16`
]

{ #category : 'instance creation' }
Cursor class >> bottomRightCursor [
	^  Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r1111111111111100
		2r1111111111111100
		2r0000000000000000
		2r0000000000000000)
	offset: `-16@ -16`
]

{ #category : 'instance creation' }
Cursor class >> cornerCursor [
	"Answer the instance of me that is the shape of the bottom right corner 
	of a rectangle."
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r1111111111111100
		2r1111111111111100
		2r0000000000000000
		2r0000000000000000)
	offset: `-16@ -16`
]

{ #category : 'instance creation' }
Cursor class >> crossHairCursor [
	"Answer the instance of me that is the shape of a cross."
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000000000000000
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0111111111111100
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0000000000000000
		2r0000000000000000)
	offset: `-7@ -7`
]

{ #category : 'current cursor' }
Cursor class >> currentCursor [
	"Answer the instance of Cursor that is the one currently displayed."
	^CurrentCursor
]

{ #category : 'current cursor' }
Cursor class >> currentCursor: aCursor  [
	"Make the instance of cursor, aCursor, be the current cursor. Display it. 
	Create an error if the argument is not a Cursor."

	CurrentCursor := aCursor.
	(Preferences at: #biggerCursors)
		ifTrue: [
			[ ^aCursor asBigCursor installCursor] 
				on: Error do: nil  "fall through "].
	aCursor installCursor
]

{ #category : 'accessing' }
Cursor class >> cursorAt: cursorKey [
	^ self
		cursorAt: cursorKey
		ifAbsent: [ self defaultCursor ].
]

{ #category : 'accessing' }
Cursor class >> cursorAt: cursorKey ifAbsent: aBlock [
	^ (CursorDict
		at: cursorKey
		ifAbsent: [
			(self respondsTo: cursorKey) ifTrue: [ | newCursor |
				newCursor := self perform: cursorKey.
				newCursor hasMask ifFalse: [ newCursor := newCursor withMask ].
				self
					cursorAt: cursorKey
					put: newCursor ]]) ifNil: aBlock.
]

{ #category : 'accessing' }
Cursor class >> cursorAt: cursorKey put: aCursor [
	^ CursorDict at: cursorKey put: aCursor
]

{ #category : 'accessing' }
Cursor class >> defaultCursor [
	"Answer the instance of me that is the shape of an arrow slanted left."
	^ (Preferences at: #biggerCursors)
		ifTrue: [ CursorWithAlpha biggerNormal ]
		ifFalse: [ self cursorAt: #normalCursorWithMask ].
]

{ #category : 'instance creation' }
Cursor class >> downCursor [
	"Answer the instance of me that is the shape of an arrow facing 
	downward."
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0011000000000000
		2r0011000000000000
		2r0011000000000000
		2r0011000000000000
		2r0011000000000000
		2r0011000000000000
		2r0011000000000000
		2r1111110000000000
		2r0111100000000000
		2r0011000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000)
	offset: `0@0`
]

{ #category : 'instance creation' }
Cursor class >> executeCursor [
	"Answer the instance of me that is the shape of an arrow slanted left 
	with a star next to it."
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r1000000000100000
		2r1100000000100000
		2r1110000001110000
		2r1111001111111110
		2r1111100110001100
		2r1111110010001000
		2r1111111011111000
		2r1111000011011000
		2r1101100110001100
		2r1001100100000100
		2r0000110000000000
		2r0000110000000000
		2r0000011000000000
		2r0000011000000000
		2r0000000000000000
		2r0000000000000000)
	offset: `0@0`
]

{ #category : 'class initialization' }
Cursor class >> initialize [
	CursorDict := Dictionary new.

]

{ #category : 'instance creation' }
Cursor class >> markerCursor [
	"Answer the instance of me that is the shape of a small ball."
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0111000000000000
		2r1111100000000000
		2r1111100000000000
		2r0111000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000)
	offset: `0@0`
]

{ #category : 'instance creation' }
Cursor class >> menuCursor [
	"Answer the instance of me that is the shape of a menu."
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r1111111111100000
		2r1000000000100000
		2r1010011000100000
		2r1000000000100000
		2r1101001101100000
		2r1111111111100000
		2r1000000000100000
		2r1011001010100000
		2r1000000000100000
		2r1010110010100000
		2r1000000000100000
		2r1010010100100000
		2r1000000000100000
		2r1111111111100000
		2r0000000000000000
		2r1111111111100000)
	offset: `0@0`
]

{ #category : 'instance creation' }
Cursor class >> moveCursor [
	"Answer the instance of me that is the shape of a cross inside a square."
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r1111111111111100
		2r1111111111111100
		2r1100001100001100
		2r1100001100001100
		2r1100001100001100
		2r1100001100001100
		2r1111111111111100
		2r1111111111111100
		2r1100001100001100
		2r1100001100001100
		2r1100001100001100
		2r1100001100001100
		2r1111111111111100
		2r1111111111111100
		2r0000000000000000
		2r0000000000000000)
	offset: `0@0`
]

{ #category : 'instance creation' }
Cursor class >> new [

	^ self extent: `16 @ 16`
]

{ #category : 'instance creation' }
Cursor class >> normalCursor [
	"Answer the instance of me that is the shape of an arrow slanted left."
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r1000000000000000
		2r1100000000000000
		2r1110000000000000
		2r1111000000000000
		2r1111100000000000
		2r1111110000000000
		2r1111111000000000
		2r1111100000000000
		2r1111100000000000
		2r1001100000000000
		2r0000110000000000
		2r0000110000000000
		2r0000011000000000
		2r0000011000000000
		2r0000001100000000
		2r0000001100000000)
	offset: `0@0`
]

{ #category : 'instance creation' }
Cursor class >> normalCursorWithMask [
	"Cursor initNormalWithMask.  Cursor normalCursor show"
	"Next two lines work simply for any cursor..."
	"
	self initNormal.
	NormalCursor := CursorWithMask derivedFrom: NormalCursor.
	"
	"But for a good looking cursor, you have to tweak things..."
	^ ((CursorWithMask
		small1BitExtent: `16@16`
		pixelBits: #(
			2r0000000000000000
			2r0100000000000000
			2r0110000000000000
			2r0111000000000000
			2r0111100000000000
			2r0111110000000000
			2r0111111000000000
			2r0111111100000000
			2r0111111110000000
			2r0111110000000000
			2r0110110000000000
			2r0100011000000000
			2r0000011000000000
			2r0000001100000000
			2r0000001100000000
			2r0000000000000000)
		offset: `-1@ -1`)
	setMaskForm: 
		(Form
			small1BitExtent: `16@16`
			pixelBits: #(
				2r1100000000000000
				2r1110000000000000
				2r1111000000000000
				2r1111100000000000
				2r1111110000000000
				2r1111111000000000
				2r1111111100000000
				2r1111111110000000
				2r1111111111000000
				2r1111111111100000
				2r1111111000000000
				2r1110111100000000
				2r1100111100000000
				2r1000011110000000
				2r0000011110000000
				2r0000001110000000)
			offset: `0@0`))
]

{ #category : 'instance creation' }
Cursor class >> originCursor [
	"Answer the instance of me that is the shape of the top left corner of a rectangle."
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r1111111111111100
		2r1111111111111100
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r0000000000000000
		2r0000000000000000)
	offset: `0@0`
]

{ #category : 'instance creation' }
Cursor class >> readCursor [
	"Answer the instance of me that is the shape of eyeglasses."
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000000000000000
		2r0000000000000000
		2r0001000000001000
		2r0010100000010100
		2r0100000000100000
		2r1111101111100000
		2r1000010000100000
		2r1000010000100000
		2r1011010110100000
		2r0111101111000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000)
	offset: `0@0`
]

{ #category : 'instance creation' }
Cursor class >> resizeBottomCursor [
	^ self cursorAt: #resizeTopCursor
]

{ #category : 'instance creation' }
Cursor class >> resizeBottomLeftCursor [
	^ self cursorAt: #resizeTopRightCursor
]

{ #category : 'instance creation' }
Cursor class >> resizeBottomRightCursor [
	^ self cursorAt: #resizeTopLeftCursor
]

{ #category : 'instance creation' }
Cursor class >> resizeForEdge: aSymbol  [
	"Cursor resizeForEdge: #top"
	"Cursor resizeForEdge: #bottomLeft"

	"Do not erase this literal, as it helps 'senders' workproperly, and it protects these methods from accidental deletion"
	self nominallyUnsent: #(#resizeBottomCursor #resizeBottomLeftCursor #resizeBottomRightCursor #resizeLeftCursor #resizeRightCursor #resizeTopCursor #resizeTopLeftCursor #resizeTopRightCursor ).
	
	^ self perform: ('resize' , aSymbol capitalized, 'Cursor') asSymbol
]

{ #category : 'instance creation' }
Cursor class >> resizeLeftCursor [
	^ `(Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000000000000000
		2r0000010010000000
		2r0000010010000000
		2r0000010010000000
		2r0001010010100000
		2r0011010010110000
		2r0111010010111000
		2r1111110011111100
		2r0111010010111000
		2r0011010010110000
		2r0001010010100000
		2r0000010010000000
		2r0000010010000000
		2r0000010010000000
		2r0000000000000000
		2r0000000000000000)
	offset: `-7@ -7`) withMask`.
]

{ #category : 'instance creation' }
Cursor class >> resizeRightCursor [
	^ self cursorAt: #resizeLeftCursor
]

{ #category : 'instance creation' }
Cursor class >> resizeTopCursor [
	^ `(Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000000100000000
		2r0000001110000000
		2r0000011111000000
		2r0000111111100000
		2r0000000100000000
		2r0111111111111100
		2r0000000000000000
		2r0000000000000000
		2r0111111111111100
		2r0000000100000000
		2r0000111111100000
		2r0000011111000000
		2r0000001110000000
		2r0000000100000000
		2r0000000000000000
		2r0000000100000000)
	offset: `-7@ -7`) withMask`.
]

{ #category : 'instance creation' }
Cursor class >> resizeTopLeftCursor [
	^ `(Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000000000000000
		2r0111110000010000
		2r0111100000100000
		2r0111000001000100
		2r0110100010001000
		2r0100010100010000
		2r0000001000100000
		2r0000010001000000
		2r0000100010000000
		2r0001000101000100
		2r0010001000101100
		2r0000010000011100
		2r0000100000111100
		2r0000000001111100
		2r0000000000000000
		2r0000000000000000)
	offset: `-7@ -7`) withMask`.
]

{ #category : 'instance creation' }
Cursor class >> resizeTopRightCursor [
	^ `(Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000000000000000
		2r0001000001111100
		2r0000100000111100
		2r0100010000011100
		2r0010001000101100
		2r0001000101000100
		2r0000100010000000
		2r0000010001000000
		2r0000001000100000
		2r0100010100010000
		2r0110100010001000
		2r0111000001000000
		2r0111100000100000
		2r0111110000000000
		2r0000000000000000
		2r0000000000000000)
	offset: `-7@ -7`) withMask`.
]

{ #category : 'instance creation' }
Cursor class >> rightArrowCursor  [
	"Answer the instance of me that is the shape of an arrow pointing to the right."
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000011000000000
		2r0000011110000000
		2r0000011111100000
		2r1111111111111000
		2r0000011111100000
		2r0000011110000000
		2r0000011000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000)
	offset: `0@0`
]

{ #category : 'instance creation' }
Cursor class >> squareCursor [
	"Answer the instance of me that is the shape of a square."
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000001111000000
		2r0000001111000000
		2r0000001111000000
		2r0000001111000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000)
	offset: `-8@ -8`
]

{ #category : 'class initialization' }
Cursor class >> startUp [
	self currentCursor: self currentCursor
]

{ #category : 'instance creation' }
Cursor class >> targetCursor [
	"Answer the instance of me that is the shape of a gunsight."
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000011111000000
		2r0001100100110000
		2r0010000100001000
		2r0100000100000100
		2r0100001110000100
		2r1000000100000010
		2r1000100100100010
		2r1111111011111110
		2r1000100100100010
		2r1000000100000010
		2r0100001110000100
		2r0100000100000100
		2r0010000100001000
		2r0001100100110000
		2r0000011111000000
		2r0000000000000000)
	offset: `-7@ -7`
]

{ #category : 'instance creation' }
Cursor class >> topLeftCursor [
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r1111111111111100
		2r1111111111111100
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r0000000000000000
		2r0000000000000000)
	offset: `0@0`
]

{ #category : 'instance creation' }
Cursor class >> topRightCursor [
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r1111111111111100
		2r1111111111111100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000001100
		2r0000000000000000
		2r0000000000000000)
	offset: `-16@0`
]

{ #category : 'instance creation' }
Cursor class >> upCursor [
	"Answer the instance of me that is the shape of an arrow facing upward."
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0011000000000000
		2r0111100000000000
		2r1111110000000000
		2r0011000000000000
		2r0011000000000000
		2r0011000000000000
		2r0011000000000000
		2r0011000000000000
		2r0011000000000000
		2r0011000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000
		2r0000000000000000)
	offset: `0@0`
]

{ #category : 'instance creation' }
Cursor class >> waitCursor [
	"Answer the instance of me that is the shape of an Hourglass (was in the 
	shape of three small balls)."
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r1111111111111100
		2r1000000000000100
		2r0100000000001000
		2r0010000000010000
		2r0001110011100000
		2r0000111111000000
		2r0000011110000000
		2r0000011110000000
		2r0000100101000000
		2r0001000100100000
		2r0010000110010000
		2r0100001111001000
		2r1000111111110100
		2r1111111111111100
		2r0000000000000000
		2r1111111111111100)
	offset: `0@0`
]

{ #category : 'instance creation' }
Cursor class >> webLinkCursor [
	"Return a cursor that can be used for emphasizing web links"
	^ (CursorWithMask
		small1BitExtent: `16@16`
		pixelBits: #(
			2r0000110000000000
			2r0001001000000000
			2r0001001000000000
			2r0001001000000000
			2r0001001000000000
			2r0001001110110110
			2r0001001001001001
			2r0111001001001001
			2r1001001001001001
			2r1001001001001001
			2r1000000000000001
			2r1000000000000001
			2r1100000000000011
			2r0100000000000010
			2r0110000000000110
			2r0011111111111100)
		offset: `-5@0`)
	 setMaskForm: (Form
		small1BitExtent: `16@16`
		pixelBits: #(
			2r0000110000000000
			2r0001111000000000
			2r0001111000000000
			2r0001111000000000
			2r0001111000000000
			2r0001111110110110
			2r0001111111111111
			2r0111111111111111
			2r1111111111111111
			2r1111111111111111
			2r1111111111111111
			2r1111111111111111
			2r1111111111111111
			2r0111111111111110
			2r0111111111111110
			2r0011111111111100)
		offset: `0@0`)
]

{ #category : 'instance creation' }
Cursor class >> writeCursor [
	"Answer the instance of me that is the shape of a pen writing."
	^ Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000000000011000
		2r0000000000111100
		2r0000000001001000
		2r0000000010010000
		2r0000000100100000
		2r0000001001000100
		2r0000010010000100
		2r0000100100001100
		2r0001001000010000
		2r0010010000010000
		2r0111100000001000
		2r0101000011111000
		2r1110000110000000
		2r0111111100000000
		2r0000000000000000
		2r0000000000000000)
	offset: `0@0`
]

{ #category : 'displaying' }
Cursor >> activateCursor [
	"Make the hardware's mouse cursor look like the receiver"

	Cursor currentCursor: self
]

{ #category : 'converting' }
Cursor >> asBigCursor [
	"Big cursors are 32 bits deep (ARGB premultiplied)"
	(width = 16 and: [ height = 16 ])
		ifTrue:[ ^self enlargedBy: 2 ].
	^self
]

{ #category : 'converting' }
Cursor >> asCursorForm [
	| form |
	form := Form extent: self extent depth: 8.
	form fillShape: self fillColor: `Color black` at: offset negated.
	^ form offset: offset
]

{ #category : 'converting' }
Cursor >> enlargedBy: scale [
	"Big cursors are 32 bits deep (ARGB premultiplied)"
	| big |
	scale = 1 ifTrue: [^self].
	big := CursorWithAlpha extent: self extent * scale depth: 32.
	(self asCursorForm magnifyBy: scale) displayOn: big.
	big offset: (self offset - 0.5 * scale min: `0@0` max: big extent negated) asIntegerPoint.
	big fallback: self.
	^ big
]

{ #category : 'testing' }
Cursor >> hasMask [
	^false
]

{ #category : 'primitives' }
Cursor >> installCursor [
	"Primitive. Tell the interpreter to use the receiver as the current cursor 
	image. Fail if the receiver does not match the size expected by the 
	hardware. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 101>
	self primitiveFailed
]

{ #category : 'primitives' }
Cursor >> installCursorWithMask: maskForm [
	"Primitive. Tell the interpreter to use the receiver as the current cursor image with the given mask Form. Both the receiver and the mask should have extent 16@16 and a depth of one. The mask and cursor bits are combined as follow:
			mask	cursor	effect
			 0		  0		transparent (underlying pixel shows through)
			 1		  1		opaque black
			 1		  0		opaque white
			 0		  1		invert the underlying pixel"
"Essential. See Object documentation whatIsAPrimitive."

	<primitive: 101>
	self primitiveFailed

]

{ #category : 'testing' }
Cursor >> is: aSymbol [
	^ aSymbol == #Cursor or: [ super is: aSymbol ]
]

{ #category : 'printing' }
Cursor >> printOn: aStream  [
	self storeOn: aStream base: 2
]

{ #category : 'displaying' }
Cursor >> showWhile: aBlock  [
	"While evaluating the argument, aBlock, make the receiver be the cursor 
	shape."

	| oldcursor |
	oldcursor := Cursor currentCursor.
	self activateCursor.
	^aBlock ensure: [oldcursor activateCursor]

]

{ #category : 'converting' }
Cursor >> withMask [
	^CursorWithMask derivedFrom: self
]
