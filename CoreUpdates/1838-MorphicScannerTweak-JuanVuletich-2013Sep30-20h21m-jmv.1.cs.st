'From Cuis 4.2 of 25 July 2013 [latest update: #1837] on 30 September 2013 at 8:25:05.373298 pm'!

!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 9/30/2013 20:20'!
displayLine: textLine paragraphTopLeft: paragraphTopLeft leftInRun: leftInRun
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."

	"paragraphTopLeft is relative to the morph currently being drawn"
	| done stopCondition nowLeftInRun startIndex string lastPos priorFont |

	paraTopLeft _ paragraphTopLeft.
	line _ textLine.
	lineY _ line top + paragraphTopLeft y.
	rightMargin _ line rightMargin + paragraphTopLeft x.
	lastIndex _ line first.
	leftInRun <= 0 ifTrue: [self setStopConditions].
	leftMargin _ (line leftMarginForAlignment: alignment) + paragraphTopLeft x.
	destX _ runX _ leftMargin.
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
	done _ false.
	string _ text string.

	self placeEmbeddedObject.
	[ done ] whileFalse: [
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
		done _ self perform: stopCondition ].
	^ runStopIndex - lastIndex   "Number of characters remaining in the current run"! !

