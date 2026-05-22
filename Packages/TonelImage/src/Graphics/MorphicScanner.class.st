"
My instances are used to scan text and display it on the screen or in a hidden form.
"
Class {
	#name : 'MorphicScanner',
	#superclass : 'CharacterScanner',
	#instVars : [
		'canvas',
		'lineY',
		'foregroundColor',
		'defaultColor',
		'topLeft',
		'backgroundColor',
		'blockNestingStartX'
	],
	#category : 'Graphics-Text',
	#package : 'Graphics',
	#tag : 'Text'
}

{ #category : 'private' }
MorphicScanner >> backgroundColor: aColor [
	backgroundColor := aColor
]

{ #category : 'accessing' }
MorphicScanner >> canvas: aCanvas [

	canvas := aCanvas
]

{ #category : 'stop conditions' }
MorphicScanner >> crossedX [
	"This condition will sometimes be reached 'legally' during display, when, 
	for instance the space that caused the line to wrap actually extends over 
	the right boundary. This character is allowed to display, even though it 
	is technically outside or straddling the clipping ectangle since it is in 
	the normal case not visible and is in any case appropriately clipped by 
	the scanner."

	^ true 
]

{ #category : 'scanning' }
MorphicScanner >> displayBulletIfAppropriateFor: textLine textLeft: texLeft [
	"texLeft is relative to the morph currently being drawn"

	| paragraphEnd count pattern |
	paragraphStyle ifNotNil: [
		(textLine isFirstLine and: [ paragraphStyle isListStyle ]) ifTrue: [
			pattern := paragraphStyle listBulletPattern.
			"Count how many paragraphs before this one already used the pattern"
			count := 0.
			paragraphEnd := textLine first-1.
			[
			paragraphEnd > 0 and: [ ((text paragraphStyleOrNilAt: paragraphEnd) ifNotNil: [ :ps | ps listBulletPattern ]) = pattern ]] whileTrue: [
				count := count + 1.
				paragraphEnd := text string endOfParagraphBefore: paragraphEnd ].
			"Our number in the list, is one more than the count of previous contiguous paragraphs with this pattern"
			self
				displayBulletTextLeft: texLeft
				number: count + 1]]
]

{ #category : 'scanning' }
MorphicScanner >> displayBulletTextLeft: texLeft number: bulletNumber [
	"texLeft is relative to the morph currently being drawn"

	| pattern i c j s bullet bulletPos bulletSize prefix |
	pattern := paragraphStyle listBulletPattern.
	bullet := pattern.
	(i := pattern indexOf: $%) > 0
		ifTrue: [ bullet := bulletNumber printString]
		ifFalse: [
			(i := pattern indexOf: $z) > 0
				ifTrue: [ bullet := String with: (Character codePoint: 96 + bulletNumber) ]
				ifFalse: [
					(i := pattern indexOf: $Z) > 0
						ifTrue: [ bullet := String with: (Character codePoint: 64 + bulletNumber) ]]].
	prefix := 0.
	i > 0 ifTrue: [
		c := pattern at: i.
		j := i.
		s := pattern size.
		[ j <= s and: [ (pattern at: j) = c ] ] whileTrue: [ j := j + 1 ].
		j := j - 1.
		bulletSize := j-i+1.
		prefix := bulletSize - bullet size max: 0.
		bullet size > bulletSize ifTrue: [
			bullet := bullet copyFrom: bullet size - bulletSize + 1 to: bullet size ].
		bullet := (pattern copyFrom: 1 to: i-1), bullet, (pattern copyFrom: j+1 to: pattern size) ].
	bulletPos := paragraphStyle firstIndent + texLeft + ((font widthOf: $9) * prefix)@destY.
	canvas
		drawString: bullet
		from: 1
		to: bullet size
		at: bulletPos
		font: font
		color: foregroundColor
]

{ #category : 'scanning' }
MorphicScanner >> displayLine: textLine textTopLeft: textTopLeft leftInRun: leftInRun [
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."

	"textTopLeft is relative to the morph currently being drawn"
	| stopCondition nowLeftInRun startIndex string lastPos tabBackColor |

	topLeft := textTopLeft.
	line := textLine.
	lineY := line top + textTopLeft y.
	rightMargin := line rightMargin + textTopLeft x.
	lastIndex := line first.
	tabCount := 0.
	leftInRun <= 0 ifTrue:
		[
			self setFont.
			self setStopConditions ].
	leftMargin := (line leftMarginForAlignment: alignment) + textTopLeft x.
	destX := leftMargin.
	destY := lineY + line baseline - font ascent.

	textLine isEmptyLine ifTrue: [
		textLine paragraphStyle ifNotNil: 
			[ :ps |
			ps = paragraphStyle ifFalse: 
				[
					foregroundColor := defaultColor.
					font := ps font.
					ps color ifNotNil: [ :color | self textColor: color ].
					alignment := ps alignment.
					paragraphStyle := ps.
					spaceWidth := font widthOf: Character space.
					self setStopConditions.
					text ifNotNil: [ destY := lineY + line baseline - font ascent ]]].
		self displayBulletIfAppropriateFor: textLine textLeft: textTopLeft x.
		^leftInRun ].

	self displayBulletIfAppropriateFor: textLine textLeft: textTopLeft x.

	leftInRun <= 0
		ifTrue: [nowLeftInRun := text runLengthFor: lastIndex]
		ifFalse: [nowLeftInRun := leftInRun].
	runStopIndex := lastIndex + (nowLeftInRun - 1) min: line last.
	spaceCount := 0.
	string := text string asUnicodeString.

	self placeEmbeddedObject.
	[
		startIndex := lastIndex.
		lastPos := destX@destY.
	
		stopCondition := self
			scanCharactersFrom: lastIndex to: runStopIndex
			in: string rightX: rightMargin stopConditions: stopConditions.
		backgroundColor ifNotNil: [
			"If this is not a #tab, update block nesting information. It it is a #tab, use it to draw background."
			stopCondition = #tab
				ifFalse:
				[ | indexForThisColor |
					
					(text hasShoutBackgroundColorAt: startIndex) ifTrue: [
						indexForThisColor := blockNestingStartX findFirst: [ :pair | pair second = backgroundColor ].
						indexForThisColor > 0
							ifTrue: [
								blockNestingStartX := blockNestingStartX first: indexForThisColor.
								(blockNestingStartX at: indexForThisColor) at: 1 put: (((blockNestingStartX at: indexForThisColor) at: 1) min: lastPos x)]
							ifFalse: [blockNestingStartX add: { lastPos x. backgroundColor }]]]
				ifTrue:
				[
					| indexForThisX |
					indexForThisX := blockNestingStartX findLast: [ :pair | pair first <= destX ].
					indexForThisX > 0 ifTrue: [ tabBackColor := (blockNestingStartX at: indexForThisX) second ].
					tabBackColor ifNotNil: [
						canvas
							fillRectangle: (destX @ (line top + textTopLeft y) corner: self tabDestX @ (line bottom + textTopLeft y))
							color: tabBackColor ]].
			canvas
				fillRectangle: (lastPos x @ (line top + textTopLeft y) corner: (startIndex > line last ifFalse: [ destX ] ifTrue: [ rightMargin ]) @ (line bottom + textTopLeft y))
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
	] repeat
]

{ #category : 'stop conditions' }
MorphicScanner >> doNewLine [
	"When a newLine is encountered, simply increment the pointer 
	into the paragraph."

	lastIndex := lastIndex + 1.
	^false
]

{ #category : 'stop conditions' }
MorphicScanner >> endOfRun [
	"The end of a run in the display case either means that there is actually 
	a change in the style (run code) to be associated with the string or the 
	end of this line has been reached."
	| runLength |
	lastIndex = line last ifTrue: [^true].
	runLength := text runLengthFor: (lastIndex := lastIndex + 1).
	runStopIndex := lastIndex + (runLength - 1) min: line last.
	self setFont.
	self setStopConditions.
	self placeEmbeddedObject.
	^ false
]

{ #category : 'initialization' }
MorphicScanner >> initialize [
	super initialize.
	blockNestingStartX := OrderedCollection new.
]

{ #category : 'stop conditions' }
MorphicScanner >> paddedSpace [
	"Each space is a stop condition when the alignment is right justified. 
	Padding must be added to the base width of the space according to 
	which space in the line this space is and according to the amount of 
	space that remained at the end of the line when it was composed."

	spaceCount := spaceCount + 1.
	destX := destX + spaceWidth + (line justifiedPadFor: spaceCount).
	lastIndex := lastIndex + 1.
	^ false
]

{ #category : 'scanning' }
MorphicScanner >> placeEmbeddedObject: anchoredFormOrMorph [

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [^ false].
	(anchoredFormOrMorph is: #Morph)
		ifTrue: [
			anchoredFormOrMorph morphPosition:
				(destX@ (lineY+line baseline)) + (anchoredFormOrMorph morphPosition-anchoredFormOrMorph fullBoundsInOwner corner) roundedHAFZ.
			anchoredFormOrMorph show.
			canvas fullDraw: anchoredFormOrMorph ]
		ifFalse: [
			destY := lineY.
			canvas
				image: anchoredFormOrMorph
				at: destX - anchoredFormOrMorph width @ (destY + line baseline - anchoredFormOrMorph height) ].
	^ true
]

{ #category : 'private' }
MorphicScanner >> setFont  [
	foregroundColor := defaultColor.
	super setFont.  "Sets font and emphasis bits, and maybe foregroundColor"
	text ifNotNil: [ destY := lineY + line baseline - font ascent ]
]

{ #category : 'stop conditions' }
MorphicScanner >> setStopConditions [
	"Set the font and the stop conditions for the current run."

	stopConditions := alignment = CharacterScanner justifiedCode
		ifTrue: [ self class stopConditionsWithPaddedSpace ]
		ifFalse: [ self class defaultStopConditions ]
]

{ #category : 'stop conditions' }
MorphicScanner >> tab [
	destX := self tabDestX.
	lastIndex := lastIndex + 1.
	tabCount := tabCount + 1.
	^ false
]

{ #category : 'private' }
MorphicScanner >> text: t foreground: foreColor [
	text := t.
	paragraphStyle := nil.
	foregroundColor := defaultColor := foreColor
]

{ #category : 'private' }
MorphicScanner >> textColor: textColor [
	foregroundColor := textColor
]
