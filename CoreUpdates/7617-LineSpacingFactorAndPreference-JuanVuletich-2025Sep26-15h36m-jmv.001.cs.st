'From Cuis7.5 [latest update: #7616] on 26 September 2025 at 3:47:18 pm'!
!classDefinition: #CompositionScanner category: #'Graphics-Text'!
CharacterScanner subclass: #CompositionScanner
	instanceVariableNames: 'spaceX spaceIndex lineHeight baseline lineHeightAtSpace baselineAtSpace lastLineBreakingSpace lineSpacingFactor '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Text'!

!CharacterScanner methodsFor: 'initialization' stamp: 'jmv 9/25/2025 18:08:20'!
lineSpacingFactor: aNumber! !


!CompositionScanner methodsFor: 'scanning' stamp: 'jmv 9/26/2025 14:44:44'!
setFont 

	| oldBaseline oldLineHeightMinusBaseline newBaseline newLineHeight |
	super setFont.  "Sets font and emphasis bits, and maybe foregroundColor".
	"Split the extra leading due to lineSpacingFactor > 1, 70% above ascent, 30% below descent."
	newBaseline := font ascent + (lineSpacingFactor - 1 * 0.7 * font lineSpacing).
	newLineHeight := font lineSpacing * lineSpacingFactor.
	lineHeight
		ifNil: [
			baseline := newBaseline.
			lineHeight := newLineHeight ]
		ifNotNil: [
			oldBaseline := baseline.
			oldLineHeightMinusBaseline := lineHeight - oldBaseline.
			baseline := baseline max: newBaseline.
			lineHeight := lineHeight max: baseline + (oldLineHeightMinusBaseline max: newLineHeight -newBaseline) ].! !

!CompositionScanner methodsFor: 'accessing' stamp: 'jmv 9/25/2025 18:08:11'!
lineSpacingFactor: aNumber
	lineSpacingFactor := aNumber! !

!CompositionScanner methodsFor: 'initialization' stamp: 'jmv 9/26/2025 15:46:36'!
initialize
	super initialize.
	lineSpacingFactor := Preferences at: #defaultLineSpacingFactor.! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 9/25/2025 17:47:21'!
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
	] repeat! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 9/26/2025 15:46:09'!
prefGui
	^ #( 
		#(balloonHelpEnabled true )
		#(biggerCursors false )		
		#(cacheDisplayContentWhenMovingMorphs true )
		#(cheapWindowReframe false )		
		#(clickGrabsMorphs false )	
		#(ctrlArrowsScrollHorizontally false ) 
		#(drawKeyboardFocusIndicator true )
		#(focusFollowsMouse false )
		#(focusIndicatorWidth 1)
		#(fullScreenLeavesDeskMargins true )
		#(haloEnclosesFullBounds true )
		#(halosShowCoordinateSystem true )
		#(menuKeyboardControl true )
		#(optionalButtons true )		
		#(selectiveHalos true )
		#(tapAndHoldEmulatesButton2 true )
		#(tileResizerInWindowMenu true )
		#(wantsMenuIcons true )
		#(autoReplaceNamedCharacters true)
		#(spaceTabOpensAutoCompletion true)
		#(defaultLineSpacingFactor 1.15)
	)! !

!methodRemoval: CompositionScanner #setActualFont: stamp: 'jmv 9/26/2025 15:37:34'!
CompositionScanner removeSelector: #setActualFont:!
!classDefinition: #CompositionScanner category: #'Graphics-Text'!
CharacterScanner subclass: #CompositionScanner
	instanceVariableNames: 'spaceX spaceIndex lineHeight baseline lineHeightAtSpace baselineAtSpace lastLineBreakingSpace lineSpacingFactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Text'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	Preferences name: #defaultLineSpacingFactor category: #gui value: 1.15.
	Preferences saveToDisk: #defaultLineSpacingFactor.!

