'From Cuis7.3 [latest update: #6896] on 6 December 2024 at 7:45:28 pm'!

!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 12/6/2024 19:44:09'!
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

