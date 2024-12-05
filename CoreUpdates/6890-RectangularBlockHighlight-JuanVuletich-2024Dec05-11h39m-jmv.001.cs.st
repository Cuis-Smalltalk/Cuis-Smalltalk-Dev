'From Cuis7.1 [latest update: #6888] on 5 December 2024 at 11:41:02 am'!
!classDefinition: #MorphicScanner category: #'Graphics-Text'!
CharacterScanner subclass: #MorphicScanner
	instanceVariableNames: 'canvas lineY foregroundColor defaultColor topLeft backgroundColor blockNestingStartX '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Text'!

!Text methodsFor: 'emphasis testing' stamp: 'jmv 12/5/2024 11:35:03'!
hasShoutBackgroundColorAt: index
	^(self attributesAt: index)
		anySatisfy: [ :each | each isBackgroundColor & each isForShout].! !


!MorphicScanner methodsFor: 'initialization' stamp: 'jmv 12/4/2024 15:56:50'!
initialize
	super initialize.
	blockNestingStartX := OrderedCollection new.! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 12/4/2024 16:18:05'!
displayLine: textLine textTopLeft: textTopLeft leftInRun: leftInRun
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
					self setActualFont: ps font.
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
					indexForThisColor := blockNestingStartX findFirst: [ :pair | pair second = backgroundColor ].
					indexForThisColor > 0
						ifTrue: [
							blockNestingStartX := blockNestingStartX first: indexForThisColor.
							(blockNestingStartX at: indexForThisColor) at: 1 put: (((blockNestingStartX at: indexForThisColor) at: 1) min: lastPos x)]
						ifFalse: [blockNestingStartX add: { lastPos x. backgroundColor }]]
				ifTrue:
				[
					| indexForThisX |
					indexForThisX := blockNestingStartX findLast: [ :pair | pair first <= destX ].
					indexForThisX > 0 ifTrue: [ tabBackColor := (blockNestingStartX at: indexForThisX) second ].
					tabBackColor ifNotNil: [
						canvas
							fillRectangle: (destX @ lastPos y corner: self tabDestX @ (line bottom + textTopLeft y))
							color: tabBackColor ]].
			canvas
				fillRectangle: (lastPos corner: (startIndex > line last ifFalse: [ destX ] ifTrue: [ rightMargin ]) @ (line bottom + textTopLeft y))
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


!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 12/5/2024 11:37:56'!
drawTextComposition: aTextComposition at: drawingPosition extent: anExtent color: c selectionColor: sc avoidOverhang: avoidLastNotFullyVisibleLine
	"strict boolean controls whether to avoid or include the last line, if it doesn't fully fit in visibleTextBounds"

	| text lines displayScanner leftInRun line possiblyVisible firstLineIndex lastLineIndex |

	text := aTextComposition textComposed.
	lines := aTextComposition lines.
	possiblyVisible := currentTransformation boundsOfInverseTransformOf: clipRect.
	possiblyVisible := (possiblyVisible translatedBy: drawingPosition negated) intersect: (0@0 extent: anExtent).

	displayScanner := MorphicScanner new
		defaultFont: aTextComposition defaultFont;
		text: text
		foreground: c.
	displayScanner canvas: self.

	leftInRun := 0.
	"Take clipRect into account. Extrememly fast scrolls and redraws of huge files (like .sources)"
	firstLineIndex := aTextComposition lineIndexForPoint: (possiblyVisible topLeft max: `0@0`).
	"If this line includes Block Indent highlight, include previous lines until one that doesn't.
	This is needed for correct preparation of blockNestingStartX"
	[firstLineIndex > 1 and: [
		text hasShoutBackgroundColorAt: (lines at: firstLineIndex) first]] whileTrue:
			[
				firstLineIndex := firstLineIndex - 1 ].

	lastLineIndex := aTextComposition lineIndexForPoint: (possiblyVisible bottomRight min: anExtent).
	avoidLastNotFullyVisibleLine ifTrue: [
		line := lines at: lastLineIndex.
		line top + line baseline > anExtent y ifTrue: [
			lastLineIndex := lastLineIndex - 1 ]].
	firstLineIndex to: lastLineIndex do: [ :i |
		line := lines at: i.
		aTextComposition
			displaySelectionInLine: line
			on: self
			textTopLeft: drawingPosition
			selectionColor: sc.
		leftInRun := displayScanner displayLine: line textTopLeft: drawingPosition leftInRun: leftInRun ].! !

!classDefinition: #MorphicScanner category: #'Graphics-Text'!
CharacterScanner subclass: #MorphicScanner
	instanceVariableNames: 'canvas lineY foregroundColor defaultColor topLeft backgroundColor blockNestingStartX'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Text'!
