"
My instances hold the state associated with scanning text. My subclasses scan characters for specified purposes, such as computing a CharacterBlock or placing characters into Forms.

My instances are usually transient, lasting only during the scanning operation, and therefore don't need a TextModel, but just a Text with the current contents.
"
Class {
	#name : 'CharacterScanner',
	#superclass : 'Object',
	#instVars : [
		'destX',
		'lastIndex',
		'destY',
		'stopConditions',
		'text',
		'alignment',
		'leftMargin',
		'rightMargin',
		'font',
		'line',
		'runStopIndex',
		'spaceCount',
		'spaceWidth',
		'tabWidth',
		'kern',
		'paragraphStyle',
		'defaultFont',
		'tabCount'
	],
	#classVars : [
		'DefaultStopConditions',
		'StopConditionsWithPaddedSpace',
		'StopConditionsWithSpace'
	],
	#category : 'Graphics-Text',
	#package : 'Graphics',
	#tag : 'Text'
}

{ #category : 'constants' }
CharacterScanner class >> centeredCode [
	^2
]

{ #category : 'constants' }
CharacterScanner class >> crossedXCode [
	^258
]

{ #category : 'cached state access' }
CharacterScanner class >> defaultStopConditions [
	DefaultStopConditions ifNil: [
		self initialize ].
	^DefaultStopConditions
]

{ #category : 'constants' }
CharacterScanner class >> endOfRunCode [
	^ 257
]

{ #category : 'class initialization' }
CharacterScanner class >> initialize [
	"
	CharacterScanner initialize
	"
	| stopConditions |
	stopConditions := Array new: 258.
	stopConditions atAllPut: nil.
	stopConditions at: Character space codePoint + 1 put: nil.
	stopConditions at: Character tab codePoint + 1 put: #tab.

	"This line makes lf be shown as a newLine"
	stopConditions at: Character lf codePoint + 1 put: #doNewLine.

	"This line makes cr be shown as a newLine"
	stopConditions at: Character cr codePoint + 1 put: #doNewLine.

	stopConditions at: CharacterScanner endOfRunCode put: #endOfRun.
	stopConditions at: CharacterScanner crossedXCode put: #crossedX.
	DefaultStopConditions := stopConditions.

	StopConditionsWithSpace := DefaultStopConditions copy.
	StopConditionsWithSpace at: Character space codePoint + 1 put: #space.

	StopConditionsWithPaddedSpace := DefaultStopConditions copy.
	StopConditionsWithPaddedSpace at: Character space codePoint + 1 put: #paddedSpace.
	
	
]

{ #category : 'constants' }
CharacterScanner class >> justifiedCode [
	^3
]

{ #category : 'constants' }
CharacterScanner class >> leftFlushCode [
	^0
]

{ #category : 'cached state access' }
CharacterScanner class >> releaseClassCachedState [
	DefaultStopConditions := nil.
	StopConditionsWithPaddedSpace := nil.
	StopConditionsWithSpace := nil
]

{ #category : 'constants' }
CharacterScanner class >> rightFlushCode [
	^1
]

{ #category : 'cached state access' }
CharacterScanner class >> stopConditionsWithPaddedSpace [
	StopConditionsWithPaddedSpace ifNil: [
		self initialize ].
	^StopConditionsWithPaddedSpace
]

{ #category : 'cached state access' }
CharacterScanner class >> stopConditionsWithSpace [
	StopConditionsWithSpace ifNil: [
		self initialize ].
	^StopConditionsWithSpace
]

{ #category : 'private' }
CharacterScanner >> backgroundColor: ignored [
	"Overridden in MorphicScanner"
]

{ #category : 'initialization' }
CharacterScanner >> defaultFont: aFont [
	defaultFont := aFont
]

{ #category : 'stop conditions' }
CharacterScanner >> doNewLine [
	^self subclassResponsibility
]

{ #category : 'initialization' }
CharacterScanner >> initialize [
	tabCount := destX := destY := leftMargin := rightMargin := 0.
]

{ #category : 'initialization' }
CharacterScanner >> lineSpacingFactor: aNumber [

]

{ #category : 'scanning' }
CharacterScanner >> placeEmbeddedObject [
	(text attributesAt: lastIndex) do: [ :attr |
		attr anchoredFormOrMorph ifNotNil: [ :m |
			self placeEmbeddedObject: m ]].
]

{ #category : 'scanning' }
CharacterScanner >> placeEmbeddedObject: anchoredFormOrMorph [
	"Place the anchoredMorph or return false if it cannot be placed.
	In any event, advance destX by its width."

	| w |
	w := (anchoredFormOrMorph is: #Morph)
		ifTrue: [ anchoredFormOrMorph fullBoundsInOwner width ]
		ifFalse: [ anchoredFormOrMorph width ].
	destX := destX + w.
	(destX > rightMargin and: [ lastIndex ~= line first ])
		"Won't fit, but  not at start of a line. Start a new line with it"
		ifTrue: [ ^ false].
	lastIndex := lastIndex + 1.
	^ true
]

{ #category : 'scanning' }
CharacterScanner >> scanCharactersFrom: startIndex to: stopIndex in: anUnicodeString rightX: rightX stopConditions: stops [
	"This is the inner loop of text display.
	March through source String from startIndex to stopIndex. If any character is flagged with a
	non-nil entry in stops, then return the corresponding value.
	Determine width of each character.
	If dextX would exceed rightX, then return stops at: 258.
	Advance destX by the width of the character. If stopIndex has been
	reached, then return stops at: 257."

	lastIndex := startIndex.
	anUnicodeString from: startIndex to: stopIndex doWithUtf8Bytes: [ :byte1 :byte2 :byte3 :byte4 |
		| nextDestX |
		"stops are only defined for the first 128 characters
		(entries 128 to 255 are always nil).
		It is OK to just use the first byte in UTF-8"
		(stops at: byte1 + 1) ifNotNil: [ :stop | ^stop ].
		nextDestX := destX + (font widthOfUtf8Byte1: byte1 b2: byte2 b3: byte3 b4: byte4).
		nextDestX > rightX ifTrue: [
			^stops at: CharacterScanner crossedXCode ].
			destX := nextDestX.
			lastIndex := lastIndex + 1 ].
	lastIndex := stopIndex.
	^ stops at: CharacterScanner endOfRunCode
]

{ #category : 'private' }
CharacterScanner >> setFont [
	| attributes |
	"Set the font and other emphasis.
	In fact set actual ParagraphStyle (or nil), alignment, font and emphasis"

	font := text fontAt: lastIndex default: defaultFont.
	attributes := text attributesAt: lastIndex.
	text
		withAttributeValues: attributes
		do: [ :familyNameInText :pointSizeInText :emphasisInText :colorInText :alignmentInText :characterStyleInText :paragraphStyleInText :backgroundColorInText :lineSpacingFactor |
			colorInText ifNotNil: [ self textColor: colorInText ].
			self backgroundColor: backgroundColorInText.
			alignment := alignmentInText.
			paragraphStyle := paragraphStyleInText.
			self lineSpacingFactor: lineSpacingFactor ].
	
	"Hardcoded color for TextAction"
	attributes do: [ :attribute |
		attribute forTextActionInfoDo: [ :info |
			self textColor: TextAction textActionColor ]].

	"Install various parameters from the font."
	spaceWidth := font widthOf: Character space.
	tabWidth := font pointSize * 3.
]

{ #category : 'stop conditions' }
CharacterScanner >> tab [
	^self subclassResponsibility
]

{ #category : 'stop conditions' }
CharacterScanner >> tabDestX [
	"This is the basic method of adjusting destX for a tab."

	^paragraphStyle
		ifNotNil: [
			paragraphStyle
				nextTabXFrom: destX
				leftMargin: leftMargin
				rightMargin: rightMargin ]
		ifNil: [
			"Just use next tab stop"
			(destX roundDownTo: tabWidth) + tabWidth min: rightMargin ].
]

{ #category : 'private' }
CharacterScanner >> text: aText [
	text := aText.
	paragraphStyle := nil
]

{ #category : 'private' }
CharacterScanner >> textColor: ignored [
	"Overridden in MorphicScanner"
]
