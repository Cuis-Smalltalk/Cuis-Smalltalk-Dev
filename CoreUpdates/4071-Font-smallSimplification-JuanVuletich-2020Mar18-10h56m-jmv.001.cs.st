'From Cuis 5.0 [latest update: #4070] on 18 March 2020 at 10:57:34 am'!

!CharacterScanner methodsFor: 'scanning' stamp: 'jmv 1/25/2014 13:42'!
scanCharactersFrom: startIndex to: stopIndex in: sourceString rightX: rightX stopConditions: stops kern: kernValue
	"Primitive. This is the inner loop of text display--but see
	scanCharactersFrom: to:rightX: which would get the string,
	stopConditions and displaying from the instance. March through source
	String from startIndex to stopIndex. If any character is flagged with a
	non-nil entry in stops, then return the corresponding value. Determine
	width of each character from xTable, indexed by map.
	If dextX would exceed rightX, then return stops at: 258.
	Advance destX by the width of the character. If stopIndex has been
	reached, then return stops at: 257. Optional.
	See Object documentation whatIsAPrimitive."
	| nextDestX char |
	<primitive: 103>
	lastIndex _ startIndex.
	[ lastIndex <= stopIndex ]
		whileTrue: [
			char _ sourceString at: lastIndex.
			"stops are only defined for the first 256 characters.
			If we (ever) handle Character like objects beyond those in ISO-8859-15,
			thenf #iso8859s15Code shound answer nil!!"
			char iso8859s15Code ifNotNil: [ :code |
				(stops at: code + 1) ifNotNil: [ :stop | ^stop ]].
			nextDestX _ destX + (font widthOf: char).
			nextDestX > rightX ifTrue: [
				^stops at: CharacterScanner crossedXCode ].
			destX _ nextDestX.
			lastIndex _ lastIndex + 1 ].
	lastIndex _ stopIndex.
	^ stops at: CharacterScanner endOfRunCode! !


!CompositionScanner methodsFor: 'scanning' stamp: 'jmv 9/2/2018 23:27:51'!
composeFrom: startIndex inRectangle: lineRectangle firstLine: firstLine leftSide: leftSide rightSide: rightSide

	"Answer an instance of TextLineInterval that represents the next line in the paragraph."
	| runLength stopCondition xtraSpaceBefore spaceAfterParagraph |
	
	lastTabIndex _ lastIndex _ startIndex.	"scanning sets last index"
	destY _ lineRectangle top.
	lineHeight _ baseline _ 0.  "Will be increased by setFont"
	self setFont.
	self setStopConditions.

	"Set up margins"
	leftMargin _ lineRectangle left.
	rightMargin _ lineRectangle right.
	xtraSpaceBefore _ 0.
	spaceAfterParagraph _ 0.
	paragraphStyle ifNotNil: [
		leftSide ifTrue: [
			leftMargin _ leftMargin +
				((firstLine and: [ paragraphStyle isListStyle not ])
					ifTrue: [ paragraphStyle firstIndent ]
					ifFalse: [ paragraphStyle restIndent ])].
		rightSide ifTrue: [
			rightMargin _ rightMargin - paragraphStyle rightIndent].
		firstLine ifTrue: [ xtraSpaceBefore _ paragraphStyle spaceBefore ].
		spaceAfterParagraph _ paragraphStyle spaceAfter ].
	lastTabX _ destX _ spaceX _ leftMargin.

	runLength _ text runLengthFor: startIndex.
	runStopIndex _ lastIndex + runLength - 1.
	line _ (TextLine start: lastIndex stop: 0 internalSpaces: 0 paddingWidth: 0)
				rectangle: lineRectangle.
	line isFirstLine: firstLine.
	spaceCount _ 0.
	lastLineBreakingSpace _ 0.
	leftMargin _ destX.
	line leftMargin: leftMargin.

	self placeEmbeddedObject.
	[
			stopCondition _ self scanCharactersFrom: lastIndex to: runStopIndex
				in: text string rightX: rightMargin stopConditions: stopConditions
				kern: font baseKern.
			"See setStopConditions for stopping conditions for composing."
			(self perform: stopCondition) ifTrue: [
				^ line 
					lineHeight: lineHeight + xtraSpaceBefore + 
						(stopCondition == #doNewLine ifTrue: [spaceAfterParagraph] ifFalse: [0]) 
					baseline: baseline + xtraSpaceBefore ]
	] repeat! !

!methodRemoval: AbstractFont #rightOffsetAt: stamp: 'jmv 3/18/2020 10:57:05'!
AbstractFont removeSelector: #rightOffsetAt:!
!methodRemoval: AbstractFont #leftOffsetAt: stamp: 'jmv 3/18/2020 10:57:05'!
AbstractFont removeSelector: #leftOffsetAt:!
