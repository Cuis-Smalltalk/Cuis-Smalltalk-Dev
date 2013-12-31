'From Cuis 4.0 of 3 April 2012 [latest update: #4209] on 11 April 2012 at 10:01:23 pm'!

!CodeProvider methodsFor: 'commands' stamp: 'jmv 4/11/2012 21:54'!
hierarchyBrowser
	"Create and schedule a new hierarchy browser on the currently selected class or meta."

	| newBrowser aSymbol aBehavior messageCatIndex selectedClassOrMetaClass |
	(selectedClassOrMetaClass _ self selectedClassOrMetaClass)
		ifNil: [^ nil].
	newBrowser _ HierarchyBrowser new initHierarchyForClass: selectedClassOrMetaClass.
	((aSymbol _ self selectedMessageName) notNil and: [(MessageSet isPseudoSelector: aSymbol) not])
		ifTrue: [
			aBehavior _ selectedClassOrMetaClass.
			messageCatIndex _ aBehavior organization numberOfCategoryOfElement: aSymbol.
			messageCatIndex = 0 ifFalse: [
				newBrowser messageCategoryListIndex: messageCatIndex + 1.
				newBrowser messageListIndex:
					((aBehavior organization listAtCategoryNumber: messageCatIndex) indexOf: aSymbol) ]].
	^newBrowser! !


!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 4/11/2012 21:56'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: c
	| font |
	port colorMap: nil.
	font _ fontOrNil ifNil: [StrikeFont default].
	"Slight optimization when there's nothing to do."
	port clipHeight = 0 ifTrue: [^self].
	port clipWidth = 0 ifTrue: [^self].
	port installStrikeFont: font foregroundColor: (shadowColor ifNil: [ c ]).
	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: (origin + aPoint)
		strikeFont: font
		kern: font baseKern negated! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 4/11/2012 21:56'!
drawString: aString from: firstIndex to: lastIndex in: bounds font: fontOrNil color: c
	| font portRect |
	port colorMap: nil.
	portRect _ port clipRect.
	port clipByX1: bounds left + origin x 
		y1: bounds top + origin y 
		x2: bounds right + origin x 
		y2: bounds bottom + origin y.
	"Slight optimization when there's nothing to do."
	port clipHeight = 0 ifTrue: [port clipRect: portRect. ^self].
	port clipWidth = 0 ifTrue: [port clipRect: portRect. ^self].
	font _ fontOrNil ifNil: [StrikeFont default].
	port installStrikeFont: font foregroundColor: (shadowColor ifNil: [ c ]).
	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: (bounds topLeft + origin)
		strikeFont: font
		kern: font baseKern negated.
	port clipRect: portRect! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 4/11/2012 21:58'!
drawStringEmbossed: aString from: firstIndex to: lastIndex in: bounds font: fontOrNil color: aColor
	| font portRect insideColor |
	port colorMap: nil.
	portRect _ port clipRect.
	port clipByX1: bounds left + origin x 
		y1: bounds top + origin y 
		x2: bounds right + origin x 
		y2: bounds bottom + origin y.
	"Slight optimization when there's nothing to do."
	port clipHeight = 0 ifTrue: [^self].
	port clipWidth = 0 ifTrue: [^self].
	font _ fontOrNil ifNil: [StrikeFont default].
	insideColor _ shadowColor ifNil: [ aColor ].
	insideColor = Color black ifFalse: [ | topColor |
		topColor _ insideColor alphaMixed: 0.25 with: Color black.
		port installStrikeFont: font foregroundColor: topColor.
		port
			displayString: aString asString
			from: firstIndex
			to: lastIndex
			at: (bounds topLeft + origin)
			strikeFont: font
			kern: font baseKern negated ].
	insideColor = Color white ifFalse: [ | bottomColor |
		bottomColor _ insideColor alphaMixed: 0.22 with: Color white.
		port installStrikeFont: font foregroundColor: bottomColor.
		port
			displayString: aString asString
			from: firstIndex
			to: lastIndex
			at: (bounds topLeft + origin + (0@2))
			strikeFont: font
			kern: font baseKern negated ].
	port installStrikeFont: font foregroundColor: insideColor.
	port
		displayString: aString asString
		from: firstIndex
		to: lastIndex
		at: (bounds topLeft + origin + (0@1))
		strikeFont: font
		kern: font baseKern negated.
	port clipRect: portRect! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 4/11/2012 21:51'!
displayBulletParagraphTopLeft: paragraphTopLeft number: bulletNumber
	| pattern i c j s bullet bulletPos bulletSize prefix |
	pattern _ paragraphStyle listBulletPattern.
	bullet _ pattern.
	(i _ pattern indexOf: $%) > 0
		ifTrue: [ bullet _ bulletNumber asString]
		ifFalse: [
			(i _ pattern indexOf: $z) > 0
				ifTrue: [ bullet _ (Character value: 96 + bulletNumber) asString ]
				ifFalse: [
					(i _ pattern indexOf: $Z) > 0
						ifTrue: [ bullet _ (Character value: 64 + bulletNumber) asString ]]].
	prefix _ 0.
	i > 0 ifTrue: [
		c _ pattern at: i.
		j _ i.
		s _ pattern size.
		[ j <= s and: [ (pattern at: j) = c ] ] whileTrue: [ j _ j + 1 ].
		j _ j - 1.
		bulletSize _ j-i+1.
		prefix _ bulletSize - bullet size max: 0.
		bullet size > bulletSize ifTrue: [
			bullet _ bullet copyFrom: bullet size - bulletSize + 1 to: bullet size ].
		bullet _ (pattern copyFrom: 1 to: i-1), bullet, (pattern copyFrom: j+1 to: pattern size) ].
	bulletPos _ paragraphStyle firstIndent + paragraphTopLeft x + ((font widthOf: $9) * prefix)@destY.
	canvas grafPort displayString: bullet from: 1 to: bullet size at: bulletPos strikeFont: font kern: kern! !

!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 4/11/2012 21:56'!
displayLine: textLine paragraphTopLeft: paragraphTopLeft leftInRun: leftInRun
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."
	| done stopCondition nowLeftInRun startIndex string lastPos priorFont |

	line _ textLine.
	paraTopLeft _ paragraphTopLeft + canvas origin.
	lineY _ line top + paraTopLeft y.
	lineHeight _ line lineHeight.
	rightMargin _ line rightMargin + paraTopLeft x.
	lastIndex _ line first.
	leftInRun <= 0 ifTrue: [self setStopConditions].
	leftMargin _ (line leftMarginForAlignment: alignment) + paraTopLeft x.
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
				canvas grafPort installStrikeFont: font foregroundColor: foregroundColor.
				text ifNotNil: [ destY _ lineY + line baseline - font ascent ]
				""
			]
		].
		self displayBulletIfAppropriateFor: textLine paragraphTopLeft: paraTopLeft.
		^leftInRun ].

	self displayBulletIfAppropriateFor: textLine paragraphTopLeft: paraTopLeft.

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
			canvas grafPort 
				displayString: string
				from: startIndex
				to: lastIndex
				at: lastPos
				strikeFont: font
				kern: kern ].
		"see setStopConditions for stopping conditions for displaying."
		done _ self perform: stopCondition ].
	^ runStopIndex - lastIndex   "Number of characters remaining in the current run"! !

!MorphicScanner methodsFor: 'private' stamp: 'jmv 4/11/2012 21:57'!
setFont 
	foregroundColor _ paragraphColor.
	super setFont.  "Sets font and emphasis bits, and maybe foregroundColor"
	canvas grafPort installStrikeFont: font foregroundColor: foregroundColor.
	text ifNotNil: [ destY _ lineY + line baseline - font ascent ]! !


!Transcript class methodsFor: 'displaying' stamp: 'jmv 4/11/2012 21:57'!
displayOn: aForm
	"
	Transcript displayOn: Display
	"
	| font port count i string x y fh f bw r |
	bw _ self borderWidth  .
	r _ innerRectangle outsetBy: bw + self padding.
	aForm border: r width: bw. 
	aForm fill: r fillColor: Color white.
	port _ BitBlt toForm: aForm.
	port clipWidth: innerRectangle right.
	font _ StrikeFont default.
	port installStrikeFont: font foregroundColor: Color veryDarkGray.
	
	fh _ font height.
	count _ innerRectangle height // fh-1.
	x _ innerRectangle left.
	y _ innerRectangle top.
	f _ firstIndex-1.
	firstIndex > lastIndex ifTrue: [ f _ f - self maxEntries ].
	i _ (lastIndex - count max: f) \\ self maxEntries + 1.
	[
		string _ entries at: i.	
		port displayString: string from: 1 to: string size at: x@y strikeFont: font kern: font baseKern negated.
		y _ y + fh.
		i = lastIndex
	] whileFalse: [ i _ i \\ self maxEntries + 1 ].

	string _ unfinishedEntry contents.	
	port displayString: string from: 1 to: string size at: x@y strikeFont: font kern: font baseKern negated.! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 4/11/2012 21:57'!
displayUnfinishedEntryOn: aForm

	| font port count string x y fh |
	port _ BitBlt toForm: aForm.
	port clipWidth: innerRectangle right.
	font _ StrikeFont default.
	port installStrikeFont: font foregroundColor: Color black.
	
	fh _ font height.
	count _ innerRectangle height // fh-1.
	x _ innerRectangle left.

	string _ unfinishedEntry contents.
	y _ ((lastIndex - firstIndex \\ self maxEntries) min: count-1) + 1 * font height + innerRectangle top.
	port displayString: string from: 1 to: string size at: x@y strikeFont: font kern: font baseKern negated.! !

!methodRemoval: StrikeFont #characters:in:displayAt:clippedBy:rule:fillColor:!
StrikeFont removeSelector: #characters:in:displayAt:clippedBy:rule:fillColor:!
!methodRemoval: StrikeFont #displayLine:at:!
StrikeFont removeSelector: #displayLine:at:!
!methodRemoval: StrikeFont #displayString:on:from:to:at:kern:!
StrikeFont removeSelector: #displayString:on:from:to:at:kern:!
!methodRemoval: StrikeFont #installOn:foregroundColor:!
StrikeFont removeSelector: #installOn:foregroundColor:!
!methodRemoval: AbstractFont #displayString:on:from:to:at:kern:!
AbstractFont removeSelector: #displayString:on:from:to:at:kern:!
!methodRemoval: AbstractFont #installOn:foregroundColor:!
AbstractFont removeSelector: #installOn:foregroundColor:!

!AbstractFont reorganize!
('accessing' baseKern characterToGlyphMap derivativeFonts height pointSize xTable)
('measuring' approxWidthOfText: widthOf: widthOfString: widthOfString:from:to: widthOfStringOrText:)
('caching' releaseCachedState)
!

