'From Cuis 5.0 [latest update: #4219] on 15 June 2020 at 8:08:32 pm'!
!classDefinition: #CharacterScanner category: #'Graphics-Text'!
Object subclass: #CharacterScanner
	instanceVariableNames: 'destX lastIndex xTable destY stopConditions text alignment leftMargin rightMargin font line runStopIndex spaceCount spaceWidth tabWidth kern paragraphStyle defaultFont lastTabX lastTabIndex tabCount '
	classVariableNames: 'DefaultStopConditions StopConditionsWithPaddedSpace StopConditionsWithSpace '
	poolDictionaries: ''
	category: 'Graphics-Text'!

!Integer methodsFor: 'benchmarks' stamp: 'jmv 6/15/2020 20:07:36'!
tinyBenchmarks
	"Report the results of running the two tiny Squeak benchmarks.
	ar 9/10/1999: Adjusted to run at least 1 sec to get more stable results


	Performance of early 80's experimental Smalltalk systems.
	Estimations from
		http://wiki.c2.com/?GreenBook
		http://www.wirfs-brock.com/allen/things/smalltalk-things/tektronix-smalltalk-document-archive/1982-qtr4-magnolia-perf-graph.pdf
		http://www.wirfs-brock.com/allen/things/smalltalk-things/tektronix-smalltalk-document-archive/1983-Magnolia-st-perf.pdf

		DEC PDP-11/23									5,000 bytecodes/sec (Green Book, p.128)
		Apple 5MHz 68000									9,000 to 11,000 bytecodes/sec (Green Book, p.187, awb)												5000 clocks/bytecode
		VAX-11/780 5MHz C HP Smalltalk									5,000 to 25,000 bytecodes/sec (Green Book, p.235)												  330 clocks/bytecode
		VAX-11/780 5MHz C Berkeley Smalltalk									12,000 to 23000 bytecodes/sec (Green Book, p.203, awb)												  300 clocks/bytecode
		DEC VAX-11/780 5MHz assembly									20,000 to 25,000 bytecodes/sec (Green Book, p.149, awb)												  200 clocks/bytecode
		Xerox Dolphin  µcode									20,000 to 40,000 bytecodes/sec (Green Book, p.44, p.203, awb)
		TEK Magnolia 10MHz 68000									50,000 bytecodes/sec (awb)												  200 clocks/bytecode
		Xerox Dorado 14MHz µcode									400,000 to 500,000 bytecodes/sec (G.B., p.44, p.203, awb)												    28 clocks/bytecode


	0 tinyBenchmarks
		
		292 MHz G3 Mac:  									     22,727,272 bytecodes/sec;  							       984,169 sends/sec					12.8 clocks/bytecode
		400 MHz PII/Win98:  									     18,028,169 bytecodes/sec;  							    1,081,272 sends/sec					22.2 clocks/bytecode

		900MHz RasPi2 - StackVM:  									     37,758,112 bytecodes/sec;  							    2,412,667 sends/sec					16.2 clocks/bytecode
		900MHz RasPi2- CogSSpur:  									   157,441,574 bytecodes/sec;  							  10,946,039 sends/sec					  5.7 clocks/bytecode
		
		1GHz C.H.I.P. (*1) - StackVM:  									     55,315,471 bytecodes/sec;  							    3,348,667 sends/sec					18.1 clocks/bytecode
		1GHz C.H.I.P. (*1) - CogSpur:  									   253,716,551 bytecodes/sec;  							  16,853,816 sends/sec					  3.9 clocks/bytecode
		
		1.2GHz RasPi3B - StackSpur:  									     44,107,512 bytecodes/sec;  							    2,767,863 sends/sec					27.2 clocks/bytecode
		1.2GHz RasPi3B - CogSpur:  									   281,783,159 bytecodes/sec;  							  16,404,381 sends/sec					  6.6 clocks/bytecode

		1.66GHz Atom N450 - Cog:  									   244,274,809 bytecodes/sec;  							  28,795,277 sends/sec					  6.8 clocks/bytecode
		1.66GHz Atom N450 - CogSpur:  									   469,724,770 bytecodes/sec;  							  30,754,699 sends/sec					  3.5 clocks/bytecode
 
		1.33GHz Atom 3735G - Cog:  									   326,114,649 bytecodes/sec;  							  34,985,976 sends/sec					  4.1 clocks/bytecode
		1.33GHz Atom 3735G - CogSpur:  									   632,098,765 bytecodes/sec;  							  33,692,910 sends/sec					  2.1 clocks/bytecode

		1.5GHz AMD A4-5000 APU - Cog:									   390,243,902 bytecodes/sec;  							  47,507,997 sends/sec					  3.8 clocks/bytecode
		1.5GHz AMD A4-5000 APU - CogSpur:									   675,907,590 bytecodes/sec;  							  40,669,724 sends/sec					  2.2 clocks/bytecode
		1.5GHz AMD A4-5000 APU - CogSpur64:									   659,368,963 bytecodes/sec;  							  50,338,916 sends/sec					  2.2 clocks/bytecode

		2.3GHz Tegra (*2) - StackVM:  									   258,847,320 bytecodes/sec;  							  13,014,759 sends/sec					  8.9 clocks/bytecode
		2.3GHz Tegra (*2) - CogSpur:  									1,083,024,854 bytecodes/sec;  							  64,289,750 sends/sec					  2.1 clocks/bytecode

		3.1GHz Core i3-2100 - Cog:  									1,203,290,246 bytecodes/sec;  							165,723,327 sends/sec					  2.6 clocks/bytecode
		3.1GHz Core i3-2100 - CogSpur:  									2,042,892,768 bytecodes/sec;  							127,837,794 sends/sec					  1.5 clocks/bytecode

		2.70GHz Core i5-6400 -CogSpur64:  									3,162,934,362 bytecodes/sec;  							243,321,293 sends/sec					  0.85 clocks/bytecode
	
		(*1) C.H.I.P. $9 Computer [Next Thing Co], Allwinner R8 Single-Core ARM Cortex-A8 - 1 GHz
		(*2) ACER Chromebook CB5-311 - NVIDIA Tegra K1 Quad-Core ARM Cortex-A15 'r3'  - 2.3 GHz
		
	It is interesting to note that Spur for the first time brings ARM hardware (RasPi and C.H.I.P.) into a 'Morphic is confortable and nice to use' level of performance.
	"
	| t1 t2 r n1 n2 |
	n1 _ 1.
	[
		t1 _ Time millisecondsToRun: [n1 benchmark].
		t1 < 1000] 
			whileTrue: [n1 _ n1 * 2]. "Note: #benchmark's runtime is about O(n)"

	n2 _ 28.
	[
		t2 _ Time millisecondsToRun: [r _ n2 benchFib].
		t2 < 1000] 
			whileTrue: [n2 _ n2 + 1]. 
	"Note: #benchFib's runtime is about O(k^n),
		where k is the golden number = (1 + 5 sqrt) / 2 = 1.618...."

	^String streamContents: [ :strm |
		(n1 * 500000 * 1000) // t1 withDecimalUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName |
			value printOn: strm fractionDigits: 2.
			strm
				space;
				nextPutAll: unitPrefixName;
				nextPutAll: 'Bytecodes/second; ' ].
		(r * 1000) // t2 withDecimalUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName |
			value printOn: strm fractionDigits: 2.
			strm
				space;
				nextPutAll: unitPrefixName;
				nextPutAll: 'Sends/second' ]]! !


!CharacterScanner methodsFor: 'private' stamp: 'jmv 6/15/2020 19:49:07'!
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
	tabWidth _ (font widthOf: $0) * 4.
	xTable _ font xTable! !

!CharacterScanner methodsFor: 'stop conditions' stamp: 'jmv 6/15/2020 19:56:51'!
tabDestX
	"This is the basic method of adjusting destX for a tab."

	^paragraphStyle
		ifNotNil: [
			paragraphStyle
				nextTabXFrom: destX
				leftMargin: leftMargin
				rightMargin: rightMargin ]
		ifNil: [
			(tabCount+1 * tabWidth max: spaceWidth//3 + destX) min: rightMargin ].! !

!CharacterScanner methodsFor: 'initialization' stamp: 'jmv 6/15/2020 19:32:08'!
initialize
	tabCount _ destX _ destY _ leftMargin _ rightMargin _ 0.! !


!CharacterBlockScanner methodsFor: 'stop conditions' stamp: 'jmv 6/15/2020 19:31:56'!
tab
	| currentX |
	currentX _ self tabDestX.
	lastSpaceOrTabWidth _ currentX - destX max: 0.
	currentX >= characterPoint x
		ifTrue: [
			lastCharacterWidth _ lastSpaceOrTabWidth.
			^ self crossedX ].
	destX _ currentX.
	lastIndex _ lastIndex + 1.
	tabCount _ tabCount + 1.
	^false! !

!CharacterBlockScanner methodsFor: 'scanning' stamp: 'jmv 6/15/2020 19:31:39'!
characterBlockAtPoint: aPoint index: index in: textLine
	"This method is the Morphic characterBlock finder."
	| runLength lineStop stopCondition |
	line _ textLine.
	rightMargin _ line rightMargin.
	lastIndex _ line first.
	tabCount _ 0.
	self setFont.
	self setStopConditions.
	characterIndex _ index.  "nil means scanning for point"
	characterPoint _ aPoint.
	(characterPoint isNil or: [ characterPoint y > line bottom ])
		ifTrue: [ characterPoint _ line bottomRight ].
	(text isEmpty or: [( characterPoint y < line top or: [ characterPoint x < line left ])
				or: [ characterIndex notNil and: [ characterIndex < line first ]]])
		ifTrue:	[^ CharacterBlock
					stringIndex: line first
					text: text
					topLeft: line leftMargin@line top
					extent: 0 @ line lineHeight
					textLine: line].
	 destX _ leftMargin _ line leftMarginForAlignment: alignment.
	destY _ line top.
	runLength _ text runLengthFor: line first.
	lineStop _ characterIndex	"scanning for index"
		ifNil: [ line last ].			"scanning for point"
	runStopIndex _ lastIndex + (runLength - 1) min: lineStop.
	lastCharacterWidth _ 0.
	spaceCount _ 0.

	self placeEmbeddedObject.
	[
		stopCondition _ self scanCharactersFrom: lastIndex to: runStopIndex
			in: text string rightX: characterPoint x
			stopConditions: stopConditions kern: font baseKern.
		"see setStopConditions for stopping conditions for character block operations."
		lastCharacterWidth _ specialWidth ifNil: [ font widthOf: (text at: lastIndex) ].
		(self perform: stopCondition) ifTrue: [
			^characterIndex
				ifNil: [	"Result for characterBlockAtPoint: "
					CharacterBlock
						stringIndex: lastIndex
						text: text
						topLeft: characterPoint x@line top
						extent: lastCharacterWidth @ line lineHeight
						textLine: line ]
				ifNotNil: [	"Result for characterBlockForIndex: "
					CharacterBlock
						stringIndex: characterIndex
						text: text
						topLeft: characterPoint x@line top
						extent: lastCharacterWidth @ line lineHeight
						textLine: line ]]
		] repeat! !


!CompositionScanner methodsFor: 'scanning' stamp: 'jmv 6/15/2020 19:33:05'!
composeFrom: startIndex inRectangle: lineRectangle firstLine: firstLine leftSide: leftSide rightSide: rightSide

	"Answer an instance of TextLineInterval that represents the next line in the paragraph."
	| runLength stopCondition xtraSpaceBefore spaceAfterParagraph |
	
	lastIndex _ startIndex.	"scanning sets last index"
	tabCount _ 0.
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
	destX _ spaceX _ leftMargin.

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

!CompositionScanner methodsFor: 'stop conditions' stamp: 'jmv 6/15/2020 19:33:16'!
tab
	"Advance destination x according to tab settings in the current
	ParagraphStyle. Answer whether the character has crossed the right edge of 
	the composition rectangle of the TextComposition."

	destX _ self tabDestX.
	destX > rightMargin ifTrue: [^self crossedX].
	lastIndex _ lastIndex + 1.
	tabCount _ tabCount + 1.
	^false
! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 6/15/2020 19:34:49'!
displayLine: textLine textTopLeft: textTopLeft leftInRun: leftInRun
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."

	"textTopLeft is relative to the morph currently being drawn"
	| stopCondition nowLeftInRun startIndex string lastPos x1 |

	topLeft _ textTopLeft.
	line _ textLine.
	lineY _ line top + textTopLeft y.
	rightMargin _ line rightMargin + textTopLeft x.
	lastIndex _ line first.
	tabCount _ 0.
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
				xTable _ font xTable.
				self setStopConditions.
				text ifNotNil: [ destY _ lineY + line baseline - font ascent ]]].
		self displayBulletIfAppropriateFor: textLine textLeft: textTopLeft x.
		^leftInRun ].

	self displayBulletIfAppropriateFor: textLine textLeft: textTopLeft x.

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
			x1 _ destX.
			(Preferences backgroundColorFillsAllBackground and: [startIndex > line last]) ifTrue: [
				x1 _ rightMargin ].
			canvas
				fillRectangle: (lastPos corner: x1 @ (line bottom + textTopLeft y))
				color: backgroundColor.
			(Preferences backgroundColorFillsAllBackground and: [stopCondition = #tab]) ifTrue: [
				canvas
					fillRectangle: (destX @ lastPos y corner: self tabDestX @ (line bottom + textTopLeft y))
					color: backgroundColor ]].
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

!MorphicScanner methodsFor: 'stop conditions' stamp: 'jmv 6/15/2020 19:33:48'!
tab
	destX _ self tabDestX.
	lastIndex _ lastIndex + 1.
	tabCount _ tabCount + 1.
	^ false! !

!classDefinition: #CharacterScanner category: #'Graphics-Text'!
Object subclass: #CharacterScanner
	instanceVariableNames: 'destX lastIndex xTable destY stopConditions text alignment leftMargin rightMargin font line runStopIndex spaceCount spaceWidth tabWidth kern paragraphStyle defaultFont tabCount'
	classVariableNames: 'DefaultStopConditions StopConditionsWithPaddedSpace StopConditionsWithSpace'
	poolDictionaries: ''
	category: 'Graphics-Text'!

!CharacterScanner reorganize!
('private' backgroundColor: setActualFont: setFont text: textColor:)
('scanning' placeEmbeddedObject placeEmbeddedObject: scanCharactersFrom:to:in:rightX:stopConditions:kern:)
('stop conditions' doNewLine tab tabDestX)
('initialization' defaultFont: initialize)
!


!CharacterBlockScanner reorganize!
('stop conditions' crossedX doNewLine endOfRun paddedSpace setFont setStopConditions tab)
('scanning' characterBlockAtPoint:index:in: placeEmbeddedObject:)
!


!CompositionScanner reorganize!
('scanning' composeFrom:inRectangle:firstLine:leftSide:rightSide: setActualFont:)
('stop conditions' crossedX doNewLine endOfRun placeEmbeddedObject: setStopConditions space tab)
('accessing' lastLineBreakingSpace rightX)
!


!MorphicScanner reorganize!
('scanning' displayBulletIfAppropriateFor:textLeft: displayBulletTextLeft:number: displayLine:textTopLeft:leftInRun: placeEmbeddedObject:)
('private' backgroundColor: setFont text:foreground: textColor:)
('stop conditions' crossedX doNewLine endOfRun paddedSpace setStopConditions tab)
('accessing' canvas:)
!

