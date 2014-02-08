'From Cuis 4.2 of 25 July 2013 [latest update: #1940] on 25 January 2014 at 1:45:11.923944 pm'!

!TextKern commentStamp: 'jmv 1/25/2014 00:14' prior: 0!
A TextKern encodes a kerning change applicable over a given range of text.  Positive values of kern spread letters out, negative kern will cause them to overlap more.!


!StrikeFont methodsFor: 'accessing' stamp: 'jmv 1/25/2014 13:34'!
baseKern
	"Return the base kern value to be used for all characters.
	What follows is some 'random' text used to visually adjust this method.
	HaHbHcHdHeHfHgHhHiHjHkHlHmHnHoHpHqHrHsHtHuHvHwHxHyHzH
	HAHBHCHDHEHFHGHHHIHJHKHLHMHNHOHPHQHRHSHTHUHVHWHXHYHXZH
	wok yuyo	wuwu	vuvu	rucu	tucu	WUWU	VUVU	huevo	HUEVO	to
											k y mate	runico ridiculo	ARABICO	AAAAA	TOMATE
	TUTU
	tatadalajafua
	abacadafagahaqawaearatayauaiaoapasadafagahajakalazaxacavabanama
	kUxUxa
	q?d?h?l?t?f?j?"
	
	| italic baseKern |
	italic _ emphasis allMask: 2.
	
	"Assume synthetic will not affect kerning (i.e. synthetic italics are not used)"
	"After all, DejaVu are the only StrikeFonts used in Cuis..."
"	self familyName = 'DejaVu'
		ifTrue: ["
			baseKern _ (italic or: [ pointSize < 9 ])
				ifTrue: [ -1 ]
				ifFalse: [ 0 ].
			(italic not and: [pointSize = 12]) ifTrue: [
				baseKern _ baseKern +1 ].
			pointSize >= 13 ifTrue: [
				baseKern _ baseKern +1 ].
			pointSize >= 20 ifTrue: [
				baseKern _ baseKern +1 ]"]
		ifFalse: [
			baseKern _ pointSize < 12
				ifTrue: [ -1 ]
				ifFalse: [ 0 ].
			italic ifTrue: [
				baseKern _ baseKern - 1]]".
	
	"If synthetic italic"
	"See makeItalicGlyphs"
	(self isSynthetic and: [ emphasis = 3 ]) ifTrue: [
		baseKern _ baseKern - ((self height-1-self ascent+4)//4 max: 0)  
		- (((self ascent-5+4)//4 max: 0)) ].
	^baseKern! !

!StrikeFont methodsFor: 'accessing' stamp: 'jmv 1/25/2014 00:25'!
widthOf: aCharacter 
	"Answer the width of the argument as a character in the receiver."

	| ascii |
	ascii _ characterToGlyphMap
		ifNil: [ aCharacter asciiValue ]
		ifNotNil: [ characterToGlyphMap at: aCharacter asciiValue + 1 ].
	(ascii >= minAscii and: [ ascii <= maxAscii ]) ifFalse: [ ascii _ maxAscii + 1 ].
	^ (xTable at: ascii + 2) - (xTable at: ascii + 1) + self baseKern! !


!CharacterScanner methodsFor: 'private' stamp: 'jmv 1/25/2014 00:15'!
setFont
	| priorFont attributes |
	"Set the font and other emphasis.
	In fact set actual ParagraphStyle (or nil), alignment, font and emphasis"
	
	priorFont _ font.
	self setActualFont: (text fontAt: lastIndex).
	attributes _ text attributesAt: lastIndex.
	text
		withFormattingAttributeValues: attributes
		do: [ :familyNameInText :pointSizeInText :emphasisInText :kernInText :colorInText :alignmentInText :characterStyleInText :paragraphStyleInText |
			colorInText ifNotNil: [ self textColor: colorInText ].
			alignment _ alignmentInText.
			kern _ kernInText.
			paragraphStyle _ paragraphStyleInText ].
	
	"Hardcoded color for TextAction"
	attributes do: [ :attribute |
		attribute forTextActionInfoDo: [ :info | self textColor: TextAction textActionColor ]].
	
	priorFont ifNotNil: [ destX _ destX + priorFont descentKern ].
	destX _ destX - font descentKern.
	kern _ kern + font baseKern.

	"Install various parameters from the font."
	spaceWidth _ font widthOf: Character space.
	xTable _ font xTable.
	map _ font characterToGlyphMap! !

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


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 1/25/2014 00:16'!
displayLine: textLine textTopLeft: textTopLeft leftInRun: leftInRun
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."

	"textTopLeft is relative to the morph currently being drawn"
	| stopCondition nowLeftInRun startIndex string lastPos priorFont |

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
				priorFont _ font.
				self setActualFont: ps font.
				ps color ifNotNil: [ :color | self textColor: color ].
				alignment _ ps alignment.
				paragraphStyle _ ps.
				priorFont ifNotNil: [ destX _ destX + priorFont descentKern ].
				destX _ destX - font descentKern.
				kern _ font baseKern.
				spaceWidth _ font widthOf: Character space.
				xTable _ font xTable.
				map _ font characterToGlyphMap.
				self setStopConditions.
				text ifNotNil: [ destY _ lineY + line baseline - font ascent ]
			]
		].
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


!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 1/25/2014 00:15'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor kern: kernOrNil

	| p1 font kern insideColor |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^self ].
	p1 _ currentTransformation intTransform: aPoint.
	port colorMap: nil.
	font _ fontOrNil ifNil: [ StrikeFont default ].
	kern _ kernOrNil ifNil: [ font baseKern ].
	insideColor _ shadowColor ifNil: [ aColor ].
	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		strikeFont: font
		color: insideColor
		kern: kern! !


!Character methodsFor: 'accessing' stamp: 'jmv 1/25/2014 13:44'!
iso8859s15Code
	"Answer the value of the receiver that represents its ISO 8859-15 (Latin-9) encoding.
	Any implementation of Character like object beyond ISO-8859-15 should answer nil.
	Senders, please consider that this method might answer nil.
	See #iso8859s15CodeForUnicodeCodePoint:"

	^ self value! !


!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 1/25/2014 13:36'!
inverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint.
	if DoIntegerTransformations, answer rounded integer values."

	^ DoIntegerTransformations
		ifTrue: [ self intInverseTransform: aPoint ]
		ifFalse: [ self floatInverseTransform: aPoint ]! !

