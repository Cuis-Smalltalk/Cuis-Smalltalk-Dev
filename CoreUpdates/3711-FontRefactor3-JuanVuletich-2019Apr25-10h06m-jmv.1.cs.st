'From Cuis 5.0 of 7 November 2016 [latest update: #3710] on 25 April 2019 at 10:12:27 am'!

!StrikeFont methodsFor: 'accessing' stamp: 'jmv 4/25/2019 10:10:55'!
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
	italic _ self isItalic.
	
	"Assume synthetic will not affect kerning (i.e. synthetic italics are not used)"
	"After all, DejaVu Sans are the only StrikeFonts used in Cuis..."
"	self familyName = 'DejaVu Sans'
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
	(self isSynthetic and: [ italic and: [ self isBold ]]) ifTrue: [
		baseKern _ baseKern - ((self lineSpacing-1-self ascent+4)//4 max: 0)  
		- (((self ascent-5+4)//4 max: 0)) ].
	^baseKern! !

!StrikeFont methodsFor: 'emphasis' stamp: 'jmv 4/25/2019 10:11:20'!
makeItalicGlyphs
	"Make an italic set of glyphs with same widths by skewing left and right.
	In the process, characters would overlap, so we widen them all first.
	"
	| extraWidth newGlyphs newXTable x newX w extraOnLeft |  
	extraOnLeft _ (self lineSpacing-1-self ascent+4)//4 max: 0.
	extraWidth _ ((self ascent-5+4)//4 max: 0) + extraOnLeft.
	newGlyphs _ Form extent: (glyphs width + (maxAscii + 1 - minAscii*extraWidth)) @ glyphs height depth: glyphs depth.
	newGlyphs fillWhite.
	newXTable _ xTable copy.

	"Copy glyphs into newGlyphs with room on left and right for overlap."
	minAscii to: maxAscii+1 do:
		[:ascii | x _ xTable at: ascii+1.  w _ (xTable at: ascii+2) - x.
		newX _ newXTable at: ascii+1.
		newGlyphs copy: ((newX + extraOnLeft) @ 0 extent: w @ glyphs height)
			from: x @ 0 in: glyphs rule: Form over.
		newXTable at: ascii+2 put: newX + w + extraWidth].		
	glyphs _ newGlyphs. 
	xTable _ newXTable.
	"Slide the bitmaps left and right for synthetic italic effect."
	4 to: self ascent-1 by: 4 do:
		[:y | 		"Slide ascenders right..."
		glyphs copy: (1@0 extent: glyphs width @ (self ascent - y))
			from: 0@0 in: glyphs rule: Form over].
	self ascent to: self height-1 by: 4 do:
		[:y | 		"Slide descenders left..."
		glyphs copy: (0@y extent: glyphs width @ glyphs height)
			from: 1@y in: glyphs rule: Form over].
	self isSynthetic: true! !

!StrikeFont methodsFor: 'file in/out' stamp: 'jmv 4/25/2019 10:11:24'!
printOn: aStream
	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: self name;
		space;
		print: self lineSpacing;
		nextPut: $)! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 4/25/2019 10:10:59'!
glyphAt: character 
	"Answer a Form copied out of the glyphs for the argument, character."
	| ascii leftX rightX |
	ascii _ character numericValue.
	(ascii between: minAscii and: maxAscii) ifFalse: [ascii _ maxAscii + 1].
	leftX _ xTable at: ascii + 1.
	rightX _ xTable at: ascii + 2.
	^ glyphs copy: (leftX @ 0 corner: rightX @ self lineSpacing)! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 4/25/2019 10:11:09'!
glyphAt: character put: characterForm
	"Copy characterForm over the glyph for the argument, character."
	| ascii leftX rightX widthDif newGlyphs |
	ascii _ character numericValue.
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
f glyphAt: $  put: (Form extent: (f widthOf: $ )+10@f lineSpacing)
"! !

