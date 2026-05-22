"
AbstractFont defines the generic interface that all fonts need to implement.
See
https://en.wikipedia.org/wiki/Font.
https://en.wikipedia.org/wiki/Computer_font
See TrueTypeFont.
"
Class {
	#name : 'AbstractFont',
	#superclass : 'Object',
	#category : 'Graphics-Text',
	#package : 'Graphics',
	#tag : 'Text'
}

{ #category : 'constants' }
AbstractFont class >> boldCode [
	^1
]

{ #category : 'ui' }
AbstractFont class >> fromUser [
	"
	Query the user for a Font (FontFamily + pointSize).
	 AbstractFont fromUser
	"
	^self fromUser: FontFamily defaultFamilyAndPointSize 
]

{ #category : 'ui' }
AbstractFont class >> fromUser: priorFont [
	"
	Query the user for a Font (FontFamily + pointSize).
	AbstractFont fromUser
	"
	"Present a menu of available fonts, and if one is chosen, return it.
	Otherwise return nil.
	Show only baseFonts i.e. FamilyName, pointSize (but do not include emphasis, such as italic or bold)"

	| fontList fontMenu active ptMenu label spec |
	fontList := FontFamily familyNames.
	fontMenu := MenuMorph new defaultTarget: self.
	fontList do: [:fontName | 
			active := priorFont familyName sameAs: fontName.
			ptMenu := MenuMorph new defaultTarget: self.
			(FontFamily pointSizesFor:fontName ) do: [ :pt | 
					(active and: [pt = priorFont pointSize]) 
						ifTrue: [label := '<on>']
						ifFalse: [label := '<off>'].
					label := label , pt printString , ' pt'.
					ptMenu 
						add: label
						target: fontMenu
						action: #modalSelection:
						argument: { 
								fontName.
								pt}].
			active ifTrue: [label := '<on>'] ifFalse: [label := '<off>'].
			label := label , fontName.
			fontMenu add: label subMenu: ptMenu].
	spec := fontMenu invokeModal.
	spec ifNil: [^nil].
	^FontFamily familyName: spec first pointSize: spec last
]

{ #category : 'constants' }
AbstractFont class >> italicCode [
	^2
]

{ #category : 'constants' }
AbstractFont class >> normalCode [
	^0
]

{ #category : 'constants' }
AbstractFont class >> struckThroughCode [
	^8
]

{ #category : 'constants' }
AbstractFont class >> subscriptCode [
	^32
]

{ #category : 'constants' }
AbstractFont class >> superscriptCode [
	^16
]

{ #category : 'constants' }
AbstractFont class >> underlinedCode [
	^4
]

{ #category : 'measuring' }
AbstractFont >> approxWidthOfText: aText [
	"Return the width of aText -- quickly, and a little bit dirty.
	Used by lists morphs containing Text objects to get a quick,
	fairly accurate measure of the width of a list item."
	| w |
	(aText isNil or: [ aText isEmpty ]) ifTrue: [ ^ 0 ].
	w := self
		widthOfString: aText string
		from: 1
		to: aText size.
	"If the text has no emphasis, just return the string size.  If it is empasized, 
	just approximate the width by adding about 20% to the width"
	^ ((aText runLengthFor: 1) = aText size and: [ (aText emphasisAt: 1) = 0 ])
		ifTrue: [ w ]
		ifFalse: [ w * 6 // 5 ]
]

{ #category : 'accessing' }
AbstractFont >> ascent [

	self subclassResponsibility 
]

{ #category : 'measuring' }
AbstractFont >> averageCharWidth [

	^ ((self widthOf: $A) + (self widthOf: $i) / 2) asIntegerOrFloat
	
]

{ #category : 'derivatives - emphasis' }
AbstractFont >> bold [
	^self emphasized: AbstractFont boldCode
]

{ #category : 'derivatives - emphasis' }
AbstractFont >> boldItalic [
	^self emphasized: AbstractFont boldCode+AbstractFont italicCode
]

{ #category : 'derivatives - emphasis' }
AbstractFont >> emphasized: code [
	^self subclassResponsibility
]

{ #category : 'derivatives - emphasis' }
AbstractFont >> italic [
	^self emphasized: AbstractFont italicCode
]

{ #category : 'measuring' }
AbstractFont >> normalizedWidthOf: aCharacter [
	"Return the width of the given character, irrespective of point size."
	^ (self widthOf: aCharacter) / self pointSize
]

{ #category : 'accessing' }
AbstractFont >> pointSize [
	self subclassResponsibility.
]

{ #category : 'caching' }
AbstractFont >> releaseCachedState [
	
]

{ #category : 'derivatives - emphasis' }
AbstractFont >> struckThrough [
	^self emphasized: AbstractFont struckThroughCode
]

{ #category : 'derivatives - emphasis' }
AbstractFont >> underlined [
	^self emphasized: AbstractFont underlinedCode
]

{ #category : 'measuring' }
AbstractFont >> widthOf: aCharacter [
	"Return the width of the given character"
	^self subclassResponsibility
]

{ #category : 'measuring' }
AbstractFont >> widthOfString: aString [
	aString ifNil:[^0].
	^self widthOfString: aString from: 1 to: aString size.
"
	FontFamily defaultFamilyAndPointSize widthOfString: 'zort' 
"
]

{ #category : 'measuring' }
AbstractFont >> widthOfString: aString from: startIndex to: stopIndex [
	"Measure the length of the given string between start and stop index"
	| character resultX |
	resultX := 0.
	startIndex to: stopIndex do:[:i | 
		character := aString at: i.
		resultX := resultX + (self widthOf: character)].
	^resultX
]

{ #category : 'measuring' }
AbstractFont >> widthOfStringOrText: aStringOrText [
    aStringOrText ifNil:[^0].
    ^(aStringOrText is: #Text)
        ifTrue: [ self approxWidthOfText: aStringOrText ]
        ifFalse: [ self widthOfString: aStringOrText ] 
]
