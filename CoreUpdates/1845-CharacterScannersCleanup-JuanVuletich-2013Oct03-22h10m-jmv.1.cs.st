'From Cuis 4.2 of 25 July 2013 [latest update: #1844] on 7 October 2013 at 5:51:56.416357 pm'!

!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 10/7/2013 17:48'!
displayLine: textLine paragraphTopLeft: paragraphTopLeft leftInRun: leftInRun
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."

	"paragraphTopLeft is relative to the morph currently being drawn"
	| stopCondition nowLeftInRun startIndex string lastPos priorFont |

	paraTopLeft _ paragraphTopLeft.
	line _ textLine.
	lineY _ line top + paragraphTopLeft y.
	rightMargin _ line rightMargin + paragraphTopLeft x.
	lastIndex _ line first.
	leftInRun <= 0 ifTrue: [self setStopConditions].
	leftMargin _ (line leftMarginForAlignment: alignment) + paragraphTopLeft x.
	destX _ leftMargin.
	destY _ lineY + line baseline - font ascent.

	textLine isEmptyLine ifTrue: [
		textLine paragraphStyle ifNotNil: [ :ps |
			ps = paragraphStyle ifFalse: [
				""
				foregroundColor _ paragraphColor.
				priorFont _ font.
				self setActualFont: ps font.
				ps color ifNotNil: [ :color | self textColor: color ].
				alignment _ ps alignment.
				paragraphStyle _ ps.
				priorFont ifNotNil: [ destX _ destX + priorFont descentKern ].
				destX _ destX - font descentKern.
				kern _ 0 - font baseKern.
				spaceWidth _ font widthOf: Character space.
				xTable _ font xTable.
				map _ font characterToGlyphMap.
				stopConditions _ DefaultStopConditions.
				text ifNotNil: [ destY _ lineY + line baseline - font ascent ]
				""
			]
		].
		self displayBulletIfAppropriateFor: textLine paragraphTopLeft: paragraphTopLeft.
		^leftInRun ].

	self displayBulletIfAppropriateFor: textLine paragraphTopLeft: paragraphTopLeft.

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
			kern: kern.
		lastIndex >= startIndex ifTrue: [
			canvas  
				drawString: string
				from: startIndex
				to: lastIndex
				at: lastPos
				font: font
				color: foregroundColor
				kern: kern ].
		"see setStopConditions for stopping conditions for displaying."
		(self perform: stopCondition) ifTrue: [
			"Number of characters remaining in the current run"
			^ runStopIndex - lastIndex ]
	] repeat.! !

!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 10/7/2013 17:48'!
placeEmbeddedObject: anchoredFormOrMorph

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [^ false].
	(anchoredFormOrMorph is: #Morph)
		ifTrue: [
			"This method should be redone calling reasonable protocol on the canvas.
			Also check use of global coordinates..."
			self flag: #jmvVer2.
			anchoredFormOrMorph morphPositionInWorld:
				((destX - anchoredFormOrMorph morphWidth)@
				(lineY+ line baseline - anchoredFormOrMorph morphHeight)) -
					paraTopLeft ]
		ifFalse: [
			destY _ lineY.
			canvas
				image: anchoredFormOrMorph
				at: destX - anchoredFormOrMorph width @ (destY + line baseline - anchoredFormOrMorph height) ].
	^ true! !

!MorphicScanner methodsFor: 'stop conditions' stamp: 'jmv 10/7/2013 17:47'!
endOfRun
	"The end of a run in the display case either means that there is actually 
	a change in the style (run code) to be associated with the string or the 
	end of this line has been reached."
	| runLength |
	lastIndex = line last ifTrue: [^true].
	runLength _ text runLengthFor: (lastIndex _ lastIndex + 1).
	runStopIndex _ lastIndex + (runLength - 1) min: line last.
	self setStopConditions.
	self placeEmbeddedObject.
	^ false! !


!TextComposer methodsFor: 'as yet unclassified' stamp: 'jmv 10/4/2013 22:37'!
composeAllRectangles: rectangles

	| charIndexBeforeLine numberOfLinesBefore |

	actualHeight _ 1.
	charIndexBeforeLine _ currCharIndex.
	numberOfLinesBefore _ lines size.
	self composeEachRectangleIn: rectangles.

	currentY _ currentY + actualHeight.
	currentY > extentForComposing y ifTrue: [
		"Oops -- the line is really too high to fit -- back out"
		currCharIndex _ charIndexBeforeLine.
		lines size - numberOfLinesBefore timesRepeat: [ lines removeLast ].
		^ self ].
	
	"It's OK -- the line still fits."
	maxRightX _ maxRightX max: scanner rightX.
	1 to: rectangles size - 1 do: [ :i |
		"Adjust heights across rectangles if necessary"
		(lines at: lines size - rectangles size + i)
			lineHeight: lines last lineHeight
			baseline: lines last baseline ].

	anythingComposed _ true.
	isFirstLine _ currCharIndex = 1 or: [
		 (theText at: currCharIndex-1) isLineSeparator]! !

!methodRemoval: CompositionScanner #columnBreak!
CompositionScanner removeSelector: #columnBreak!
!methodRemoval: CharacterScanner #columnBreak!
CharacterScanner removeSelector: #columnBreak!
