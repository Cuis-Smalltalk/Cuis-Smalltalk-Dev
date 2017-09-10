'From Cuis 5.0 of 7 November 2016 [latest update: #3168] on 10 September 2017 at 4:29:09 pm'!
!classDefinition: #TextBackgroundColor category: #'System-TextAttributes'!
TextAttribute subclass: #TextBackgroundColor
	instanceVariableNames: 'color'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-TextAttributes'!

!TextBackgroundColor commentStamp: '<historical>' prior: 0!
A TextBackgroundColor encodes a highlight (background) color change applicable over a given range of text.!

!classDefinition: #ShoutTextBackgroundColor category: #'System-TextAttributes'!
TextBackgroundColor subclass: #ShoutTextBackgroundColor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-TextAttributes'!

!ShoutTextBackgroundColor commentStamp: '<historical>' prior: 0!
Just for code styler (Shout)!

!classDefinition: #MorphicScanner category: #'Graphics-Text'!
CharacterScanner subclass: #MorphicScanner
	instanceVariableNames: 'canvas lineY foregroundColor defaultColor topLeft ignoreColorChanges backgroundColor '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Text'!

!TextAttribute methodsFor: 'iterating' stamp: 'jmv 9/7/2017 16:45:20'!
forTextBackgroundColorDo: aBlock
	"No action is the default"! !


!TextBackgroundColor methodsFor: 'accessing' stamp: 'jmv 9/7/2017 16:41:55'!
color
	^ color! !

!TextBackgroundColor methodsFor: 'accessing' stamp: 'jmv 9/7/2017 16:41:55'!
color: aColor
	color _ aColor! !

!TextBackgroundColor methodsFor: 'comparing' stamp: 'jmv 9/7/2017 16:41:55'!
= other
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other color = color]! !

!TextBackgroundColor methodsFor: 'comparing' stamp: 'jmv 9/7/2017 16:41:55'!
hash
	^ color hash! !

!TextBackgroundColor methodsFor: 'printing' stamp: 'jmv 9/7/2017 16:41:55'!
printOn: strm
	super printOn: strm.
	strm nextPutAll: ' code: '; print: color! !

!TextBackgroundColor methodsFor: 'scanning' stamp: 'jmv 9/7/2017 16:41:55'!
dominates: other
	^ other class == self class! !

!TextBackgroundColor methodsFor: 'iterating' stamp: 'jmv 9/7/2017 16:45:12'!
forTextBackgroundColorDo: aBlock
	aBlock value: color! !

!TextBackgroundColor methodsFor: 'testing' stamp: 'jmv 9/7/2017 16:44:40'!
isSet
	"Do not include Color black, as it is the default color."
	^color isTransparent not! !


!TextBackgroundColor class methodsFor: 'constants' stamp: 'jmv 9/7/2017 16:41:55'!
black
	^ self new color: Color black! !

!TextBackgroundColor class methodsFor: 'constants' stamp: 'jmv 9/7/2017 16:41:55'!
blue
	^ self new color: Color blue! !

!TextBackgroundColor class methodsFor: 'constants' stamp: 'jmv 9/7/2017 16:41:55'!
cyan
	^ self new color: Color cyan! !

!TextBackgroundColor class methodsFor: 'constants' stamp: 'jmv 9/7/2017 16:41:55'!
gray
	^ self new color: Color gray! !

!TextBackgroundColor class methodsFor: 'constants' stamp: 'jmv 9/7/2017 16:41:55'!
green
	^ self new color: Color green! !

!TextBackgroundColor class methodsFor: 'constants' stamp: 'jmv 9/7/2017 16:41:55'!
magenta
	^ self new color: Color magenta! !

!TextBackgroundColor class methodsFor: 'constants' stamp: 'jmv 9/7/2017 16:41:55'!
red
	^ self new color: Color red! !

!TextBackgroundColor class methodsFor: 'constants' stamp: 'jmv 9/7/2017 16:41:55'!
white 
	^ self new color: Color white! !

!TextBackgroundColor class methodsFor: 'constants' stamp: 'jmv 9/7/2017 16:41:55'!
yellow
	^ self new color: Color yellow! !

!TextBackgroundColor class methodsFor: 'instance creation' stamp: 'jmv 9/7/2017 16:41:55'!
color: aColor
	^ self new color: aColor! !


!ShoutTextBackgroundColor methodsFor: 'testing' stamp: 'jmv 9/7/2017 16:42:03'!
isForShout
	"True if to be removed from code before styling"
	^true! !


!CharacterScanner methodsFor: 'private' stamp: 'jmv 9/7/2017 17:00:23'!
backgroundColor: ignored
	"Overridden in MorphicScanner"! !


!MorphicScanner methodsFor: 'private' stamp: 'jmv 9/7/2017 16:56:38'!
backgroundColor: aColor
	backgroundColor _ aColor! !


!Text methodsFor: 'emphasis' stamp: 'jmv 9/10/2017 16:28:51'!
withAttributeValues: attributes do: aBlock
	"Evaluate aBlock with the values of various attributes that affect text formatting, applied in the correct order
	The order is (each overwriting the previous one)
	1) basic defaults
	2) ParagraphStyleReferene
	3) CharacterStyleReference
	4) TextFontReference
	5) TextEmphasis"

	| paragraphStyle characterStyle familyName pointSize emphasis alignment color backgroundColor |
	paragraphStyle _ nil.
	characterStyle _ nil.
	familyName _ nil.
	pointSize _ nil.
	emphasis _ 0.
	alignment _ 0.
	color _ nil.
	backgroundColor _ nil.
	
	"ParagraphStyle is the first to set several values"
	attributes do: [ :attribute |
		attribute forParagraphStyleReferenceDo: [ :s |
			paragraphStyle _ s.
			familyName _ s familyName.
			pointSize _ s pointSize.
			emphasis _ s emphasis.
			alignment _ s alignment.
			s color ifNotNil: [ :c | color _ c ]]].

	"CharacterStyle, if present, can override font and color"
	attributes do: [ :attribute |
		attribute forCharacterStyleReferenceDo: [ :s |
			characterStyle _ s.
			familyName _ s familyName.
			pointSize _ s pointSize.
			emphasis _ s emphasis.
			s color ifNotNil: [ :c | color _ c ]]].

	"These will not interfere with each other, and all of them take precedence over previous values"
	attributes do: [ :attribute |
		attribute forFontFamilyAndSizeDo: [ :fn :ps | familyName _ fn. pointSize _ ps ].
		attribute forTextEmphasisDo: [ :e | emphasis _ emphasis bitOr: e ].
		attribute forTextColorDo: [ :c | color _ c ].
		attribute forTextBackgroundColorDo: [ :c | backgroundColor _ c ].
		attribute forTextAlignmentDo: [ :a | alignment _ a ].
	].
	
	"Done. Now evaluate the block."
"change all senders!!!!!!!!!!!!"
	aBlock numArgs = 8 ifTrue: [
		^aBlock valueWithArguments: { familyName. pointSize. emphasis. color. alignment. characterStyle. paragraphStyle. backgroundColor }
		].
	^aBlock valueWithArguments: { familyName. pointSize. emphasis. color. alignment. characterStyle. paragraphStyle }! !


!BitBlt methodsFor: 'accessing' stamp: 'jmv 9/6/2017 20:40:02'!
destX: x destY: y width: w height: h
	destX _ x.
	destY _ y.
	width _ w.
	height _ h.! !


!CharacterScanner methodsFor: 'private' stamp: 'jmv 9/7/2017 17:19:12'!
setFont
	| attributes |
	"Set the font and other emphasis.
	In fact set actual ParagraphStyle (or nil), alignment, font and emphasis"

	self setActualFont: (text fontAt: lastIndex default: defaultFont).
	attributes _ text attributesAt: lastIndex.
	text
		withAttributeValues: attributes
		do: [ :familyNameInText :pointSizeInText :emphasisInText :colorInText :alignmentInText :characterStyleInText :paragraphStyleInText :backgroundColorInText |
			colorInText ifNotNil: [ self textColor: colorInText ].
			self backgroundColor: backgroundColorInText.
			alignment _ alignmentInText.
			paragraphStyle _ paragraphStyleInText ].
	
	"Hardcoded color for TextAction"
	attributes do: [ :attribute |
		attribute forTextActionInfoDo: [ :info |
			self textColor: TextAction textActionColor ]].

	"Install various parameters from the font."
	spaceWidth _ font widthOf: Character space.
	tabWidth _ (font widthOf: $a) * 3.
	xTable _ font xTable.
	map _ font characterToGlyphMap! !

!CharacterScanner methodsFor: 'private' stamp: 'jmv 9/6/2017 20:40:09'!
textColor: ignored
	"Overridden in MorphicScanner"! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 9/10/2017 16:28:26'!
displayLine: textLine textTopLeft: textTopLeft leftInRun: leftInRun
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."

	"textTopLeft is relative to the morph currently being drawn"
	| stopCondition nowLeftInRun startIndex string lastPos |

	topLeft _ textTopLeft.
	line _ textLine.
	lineY _ line top + textTopLeft y.
	rightMargin _ line rightMargin + textTopLeft x.
	lastIndex _ line first.
	leftInRun <= 0 ifTrue: [
		self setFont.
		self setStopConditions ].
	leftMargin _ (line leftMarginForAlignment: alignment) + textTopLeft x.
	destX _ leftMargin.
	destY _ lineY + line baseline - font ascent.

	textLine isEmptyLine ifTrue: [
		textLine paragraphStyle ifNotNil: [ :ps |
			ps = paragraphStyle ifFalse: [
				foregroundColor _ defaultColor.
				self setActualFont: ps font.
				ps color ifNotNil: [ :color | self textColor: color ].
				alignment _ ps alignment.
				paragraphStyle _ ps.
				spaceWidth _ font widthOf: Character space.
				tabWidth _ (font widthOf: $a) * 3.
				xTable _ font xTable.
				map _ font characterToGlyphMap.
				self setStopConditions.
				text ifNotNil: [ destY _ lineY + line baseline - font ascent ]]].
		self displayBulletIfAppropriateFor: textLine textLeft: textTopLeft x.
		^leftInRun ].

	self displayBulletIfAppropriateFor: textLine textLeft: textTopLeft x.

	lastIndex _ line first.
	leftInRun <= 0
		ifTrue: [nowLeftInRun _ text runLengthFor: lastIndex]
		ifFalse: [nowLeftInRun _ leftInRun].
	runStopIndex _ lastIndex + (nowLeftInRun - 1) min: line last.
	spaceCount _ 0.
	string _ text string.

	self placeEmbeddedObject.
	[
		startIndex _ lastIndex.
		lastPos _ destX@destY.
		stopCondition _ self
			scanCharactersFrom: lastIndex to: runStopIndex
			in: string rightX: rightMargin stopConditions: stopConditions
			kern: font baseKern.
		backgroundColor ifNotNil: [
			canvas
				fillRectangle: (lastPos corner: destX @ (line bottom +  textTopLeft y))
				color: backgroundColor ].
		lastIndex >= startIndex ifTrue: [
			canvas  
				drawString: string
				from: startIndex
				to: lastIndex
				at: lastPos
				font: font
				color: foregroundColor ].
		"see setStopConditions for stopping conditions for displaying."
		(self perform: stopCondition) ifTrue: [
			"Number of characters remaining in the current run"
			^ runStopIndex - lastIndex ]
	] repeat! !

!classDefinition: #MorphicScanner category: #'Graphics-Text'!
CharacterScanner subclass: #MorphicScanner
	instanceVariableNames: 'canvas lineY foregroundColor defaultColor topLeft backgroundColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Text'!
