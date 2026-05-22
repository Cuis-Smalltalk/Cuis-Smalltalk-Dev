"
A TextLine embodies the layout of a line of composed text.
	left right top bottom		The full line rectangle
	firstIndex lastIndex			Starting and stopping indices in the full text
	internalSpaces				Number of spaces to share paddingWidth
	paddingWidth				Number of pixels of extra space in full line
	baseline					Distance of baseline below the top of the line
	isFirstLine					A boolean indicating if it is the first line in a paragraph
TextLine's rather verbose message protocol is required for compatibility with the old CharacterScanners.
"
Class {
	#name : 'TextLine',
	#superclass : 'Object',
	#instVars : [
		'left',
		'right',
		'top',
		'bottom',
		'firstIndex',
		'lastIndex',
		'internalSpaces',
		'paddingWidth',
		'baseline',
		'isFirstLine'
	],
	#category : 'Morphic-Text Support',
	#package : 'Morphic',
	#tag : 'Text Support'
}

{ #category : 'instance creation' }
TextLine class >> start: startInteger stop: stopInteger internalSpaces: spacesInteger paddingWidth: padWidthInteger [
	"Answer an instance of me with the arguments as the start, stop points, 
	number of spaces in the line, and width of the padding."
	| line |
	line := self new firstIndex: startInteger lastIndex: stopInteger.
	^ line internalSpaces: spacesInteger paddingWidth: padWidthInteger
]

{ #category : 'comparing' }
TextLine >> = line [
 	self == line ifTrue: [ ^ true ].

	self species == line species
		ifFalse: [ ^ false ].

	^((firstIndex = line first and: [ lastIndex = line last ])
		and: [ internalSpaces = line internalSpaces ])
		and: [ paddingWidth = line paddingWidth ]
]

{ #category : 'accessing' }
TextLine >> baseline [
	^ baseline
]

{ #category : 'accessing' }
TextLine >> bottom [
	^ bottom
]

{ #category : 'accessing' }
TextLine >> bottomRight [
	^ right@bottom
]

{ #category : 'accessing' }
TextLine >> first [
	^ firstIndex
]

{ #category : 'private' }
TextLine >> firstIndex: firstInteger lastIndex: lastInteger [
	firstIndex := firstInteger.
	lastIndex := lastInteger
]

{ #category : 'comparing' }
TextLine >> hash [
	"#hash is re-implemented because #= is re-implemented"
	^firstIndex hash bitXor: lastIndex hash
]

{ #category : 'initialization' }
TextLine >> initialize [
	isFirstLine := false
]

{ #category : 'accessing' }
TextLine >> internalSpaces [
	"Answer the number of spaces in the line."

	^internalSpaces
]

{ #category : 'accessing' }
TextLine >> internalSpaces: spacesInteger  [
	"Set the number of spaces in the line to be spacesInteger."

	internalSpaces := spacesInteger
]

{ #category : 'private' }
TextLine >> internalSpaces: spacesInteger paddingWidth: padWidthInteger [

	internalSpaces := spacesInteger.
	paddingWidth := padWidthInteger
]

{ #category : 'testing' }
TextLine >> isEmptyLine [
	^false
]

{ #category : 'accessing' }
TextLine >> isFirstLine [
	^isFirstLine
]

{ #category : 'accessing' }
TextLine >> isFirstLine: aBoolean [
	isFirstLine := aBoolean
]

{ #category : 'scanning' }
TextLine >> justifiedPadFor: spaceIndex  [
	"Compute the width of pad for a given space in a line of justified text."

	| pad |
	internalSpaces = 0 ifTrue: [^0].
	pad := paddingWidth // internalSpaces.
	spaceIndex <= (paddingWidth \\ internalSpaces)
		ifTrue: [^pad + 1]
		ifFalse: [^pad]
]

{ #category : 'accessing' }
TextLine >> last [
	^ lastIndex
]

{ #category : 'accessing' }
TextLine >> left [
	^ left
]

{ #category : 'accessing' }
TextLine >> leftMargin [
	"This has to get fixed -- store during composition"
	^ self left
]

{ #category : 'accessing' }
TextLine >> leftMargin: lm [
	left := lm
]

{ #category : 'accessing' }
TextLine >> leftMarginForAlignment: alignmentCode [
	alignmentCode = CharacterScanner rightFlushCode ifTrue: [^ self left + paddingWidth].
	alignmentCode = CharacterScanner centeredCode ifTrue: [
		^ self left + (paddingWidth//2)].
	^ self left  "leftFlush and justified"
]

{ #category : 'accessing' }
TextLine >> lineHeight [
	^ bottom - top
]

{ #category : 'private' }
TextLine >> lineHeight: height baseline: ascent [
	bottom := top + height.
	baseline := ascent
]

{ #category : 'accessing' }
TextLine >> paddingWidth [
	"Answer the amount of space to be added to the font."

	^paddingWidth
]

{ #category : 'accessing' }
TextLine >> paddingWidth: padWidthInteger  [
	"Set the amount of space to be added to the font to be padWidthInteger."

	paddingWidth := padWidthInteger
]

{ #category : 'printing' }
TextLine >> printOn: aStream [
	super printOn: aStream.
	aStream space; print: firstIndex; nextPutAll: ' to: '; print: lastIndex
]

{ #category : 'accessing' }
TextLine >> rectangle [
	^ self topLeft corner: self bottomRight
]

{ #category : 'accessing' }
TextLine >> rectangle: lineRectangle [
	left := lineRectangle left.
	right := lineRectangle right.
	top := lineRectangle top.
	bottom := lineRectangle bottom
]

{ #category : 'accessing' }
TextLine >> right [
	^ right
]

{ #category : 'accessing' }
TextLine >> rightMargin [
	"This has to get fixed -- store during composition"
	^ self right
]

{ #category : 'updating' }
TextLine >> slideIndexBy: delta andMoveTopTo: newTop [
	"Relocate my character indices and y-values.
	Used to slide constant text up or down in the wake of a text replacement."

	firstIndex := firstIndex + delta.
	lastIndex := lastIndex + delta.
	bottom := bottom + (newTop - top).
	top := newTop.

]

{ #category : 'accessing' }
TextLine >> stop: stopInteger  [
	"Set the stopping point in the string of the line to be stopInteger."

	lastIndex := stopInteger
]

{ #category : 'accessing' }
TextLine >> top [
	^ top
]

{ #category : 'accessing' }
TextLine >> topLeft [
	^ left @ top
]

{ #category : 'accessing' }
TextLine >> width [
	^ right - left
]
