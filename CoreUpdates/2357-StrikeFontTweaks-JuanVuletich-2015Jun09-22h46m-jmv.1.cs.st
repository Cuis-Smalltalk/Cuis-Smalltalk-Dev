'From Cuis 4.2 of 25 July 2013 [latest update: #2356] on 9 June 2015 at 10:52:48.42063 pm'!

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 6/9/2015 22:49'!
glyphAt: character 
	"Answer a Form copied out of the glyphs for the argument, character."
	| ascii leftX rightX |
	ascii _ character asciiValue.
	(ascii between: minAscii and: maxAscii) ifFalse: [ascii _ maxAscii + 1].
	leftX _ xTable at: ascii + 1.
	rightX _ xTable at: ascii + 2.
	^ glyphs copy: (leftX @ 0 corner: rightX @ self height)! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 6/9/2015 22:52'!
glyphAt: character put: characterForm
	"Copy characterForm over the glyph for the argument, character."
	| ascii leftX rightX widthDif newGlyphs |
	ascii _ character asciiValue.
	ascii < minAscii ifTrue: [^ self error: 'Cant store characters below min ascii'].
	ascii > maxAscii ifTrue:
		[(self confirm:
'This font does not accomodate ascii values higher than ' , maxAscii printString , '.
Do you wish to extend it permanently to handle values up to ' , ascii printString)
			ifTrue: [self extendMaxAsciiTo: ascii]
			ifFalse: [^ self error: 'No change made']].
	leftX _ xTable at: ascii + 1.
	rightX _ xTable at: ascii + 2.
	widthDif _ characterForm width - (rightX - leftX).
	widthDif ~= 0 ifTrue:
		["Make new glyphs with more or less space for this char"
		newGlyphs _ Form extent: (glyphs width + widthDif) @ glyphs height depth: glyphs depth.
		newGlyphs copy: (0@0 corner: leftX@glyphs height)
			from: 0@0 in: glyphs rule: Form over.
		newGlyphs copy: ((rightX+widthDif)@0 corner: newGlyphs width@glyphs height)
			from: rightX@0 in: glyphs rule: Form over.
		glyphs _ newGlyphs.
		"adjust further entries on xTable"
		xTable _ xTable copy.
		ascii+2 to: xTable size
			do: [:i | xTable at: i put: (xTable at: i) + widthDif]].
	glyphs copy: (leftX @ 0 extent: characterForm extent)
		from: 0@0 in: characterForm rule: Form over
"
| f |  f _ AbstractFont default.
f glyphAt: $  put: (Form extent: (f widthOf: $ )+10@f height)
"! !


!StrikeFont methodsFor: 'accessing' stamp: 'jmv 6/9/2015 16:07'!
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

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 6/9/2015 22:50'!
alter: char formBlock: formBlock
	self
		glyphAt: char 
		put: (formBlock value: (self glyphAt: char))! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 6/9/2015 22:50'!
ensureCleanBold 
	"This ensures that all character glyphs have at least one pixel of white space on the right
	so as not to cause artifacts in neighboring characters in bold or italic."

	| wider glyph |
	emphasis = 0 ifFalse: [^ self].
	minAscii to: maxAscii do:
		[:i | glyph _ self glyphAt: (Character value: i).
		(glyph copy: (glyph boundingBox topRight - (1@0)
					corner: glyph boundingBox bottomRight)) isAllWhite ifFalse: [
			wider _ Form extent: (glyph width + 1)@glyph height depth: glyph depth.
			glyph depth > 1 ifTrue: [wider fillWhite].
			glyph displayOn: wider.
			self glyphAt: (Character value: i) put: wider]].
"
StrikeFont allInstancesDo: [:f | f ensureCleanBold].
(StrikeFont familyName: 'NewYork' size: 21) ensureCleanBold.
StrikeFont shutDown.  'Flush synthetic fonts'.
"
! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 6/9/2015 22:51'!
fillZeroWidthSlots
	| nullGlyph |
	"Note: this is slow because it copies the font once for every replacement."

	nullGlyph _ (Form extent: 1@glyphs height) fillGray.
	"Now fill the empty slots with narrow box characters."
	minAscii to: maxAscii do:
		[:i | (self widthOf: (Character value: i)) = 0 ifTrue:
			[self glyphAt: (Character value: i) put: nullGlyph]].
! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 6/9/2015 22:50'!
makeControlCharsVisible
	| glyph |
	self characterToGlyphMap.
	glyph _ self glyphAt: (Character space).
	glyph border: glyph boundingBox width: 1 fillColor: Color blue.
	self glyphAt: (Character value: 134) put: glyph.
	
	"Keep tab(9), lf(10), cr(13) and space(32) transparent or whatever the user chose"
	#(0 1 2 3 4 5 6 7 8 11 12 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31)
		do: [ :ascii |
			characterToGlyphMap at: ascii + 1 put: 134 ]! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 6/9/2015 22:50'!
makeCrVisible
	| glyph |
	self characterToGlyphMap.
	glyph _ self glyphAt: (Character value: 182).
	glyph border: glyph boundingBox width: 1 fillColor: Color blue.
"	glyph _ glyph reverse."
	self glyphAt: (Character value: 133) put: glyph.
	characterToGlyphMap at: 14 put: 133! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 6/9/2015 22:50'!
makeLfVisible
	| glyph |
	self characterToGlyphMap.
	glyph _ self glyphAt: (Character value: 163).
	glyph border: glyph boundingBox width: 1 fillColor: Color blue.
"	glyph _ glyph reverse."
	self glyphAt: (Character value: 132) put: glyph.
	characterToGlyphMap at: 11 put: 132! !

!methodRemoval: StrikeFont #characterForm:pixelValueAt:put:!
StrikeFont removeSelector: #characterForm:pixelValueAt:put:!
!methodRemoval: StrikeFont #characterFormAt:!
StrikeFont removeSelector: #characterFormAt:!
!methodRemoval: StrikeFont #characterFormAt:put:!
StrikeFont removeSelector: #characterFormAt:put:!
!methodRemoval: StrikeFont #fixOneWideChars!
StrikeFont removeSelector: #fixOneWideChars!

!StrikeFont reorganize!
('accessing' ascent baseFont baseFont: baseKern characterToGlyphMap characterToGlyphMap: derivativeFonts descent familyName glyphs maxAscii maxWidth minAscii name name: pointSize pointSize: setGlyphs: subscript superscript widthOf: xTable)
('testing' checkCharacter: isBaseFont isBold isItalic isStruckThrough isUnderlined)
('displaying')
('emphasis' bonk:with: derivativeFont:at: emphasis emphasis: emphasized: isSynthetic isSynthetic: makeBoldGlyphs makeItalicGlyphs makeStruckThroughGlyphs makeUnderlinedGlyphs releaseCachedState reset)
('Mac reader')
('file in/out' printOn:)
('glyphs' alter:formBlock: ensureCleanBold extendMaxAsciiTo: fillZeroWidthSlots glyphAt: glyphAt:put: makeControlCharsVisible makeCrInvisible makeCrVisible makeLfInvisible makeLfVisible makeTabInvisible makeTabVisible useLeftArrow useUnderscore widen:by:)
('private' createCharacterToGlyphMap fixDerivatives)
('building' buildFromForm:data:name: setGlyphsDepthAtMost: stripHighGlyphs)
('derivatives - emphasis')
('objects from disk' objectForDataStream:)
('as yet unclassified' on:displayString:from:to:at:color:)
!

