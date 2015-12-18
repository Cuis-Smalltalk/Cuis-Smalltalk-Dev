'From Cuis 4.2 of 25 July 2013 [latest update: #2632] on 10 December 2015 at 11:19:15.852803 am'!

!StrikeFont methodsFor: 'accessing' stamp: 'jmv 12/10/2015 11:18'!
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
	(self isSynthetic and: [ italic and: [ self isBold ]]) ifTrue: [
		baseKern _ baseKern - ((self height-1-self ascent+4)//4 max: 0)  
		- (((self ascent-5+4)//4 max: 0)) ].
	^baseKern! !

!StrikeFont methodsFor: 'accessing' stamp: 'jmv 12/10/2015 11:11'!
descent
	"Answer the receiver's maximum extent of characters below the baseline."

	| answer |
	answer _ descent.
	self isSubscript ifTrue: [ answer _ answer * 2 ].
	^ answer! !


!TextComposition methodsFor: 'display' stamp: 'jmv 12/10/2015 11:16'!
displayTextCursorAtX: x top: top bottom: bottom emphasis: emphasis on: aCanvas textLeft: textLeft
	"x, top, bottom, textLeft are relative to the morph currently being drawn."

	| textCursorColor x1 isBold isItalic x0 h w halfW r d extraW corner origin |
	isBold _ emphasis allMask: 1.
	isItalic _ emphasis allMask: 2.
	textCursorColor _ Theme current textCursor.
	h _ bottom - top.
	w _ isBold
		ifTrue: [ h // 25 + 2 ]
		ifFalse: [ h // 30 + 1 ].
	halfW _ w // 2.
	isItalic
		ifTrue: [	
			"Keep tweaking if needed!! For italics with descenders (i.e. p), cursor shows a bit to the left..."
			d _ isBold ifTrue: [ h // 8 ] ifFalse: [ h // 9].
			x0 _ x- (h*5//24) + d.
			x1 _ x + d ]
		ifFalse: [
			x0 _ x - halfW.
			x1 _ x - halfW].
	x0-textLeft < -1 ifTrue: [
		x1 _ x1 - x0 + textLeft.
		x0 _ textLeft ].
	r _ extentForComposing x-halfW-1.
	r < (x1-textLeft) ifTrue: [
		x0 _ x0 + r - x1+textLeft.
		x1 _ r +textLeft].
	extraW _ isBold
		ifTrue: [ 3 ]
		ifFalse: [ 2 ].
	origin _ x0-halfW@ top.
	corner _ x1+halfW+extraW @ (bottom+2).
	lastTextCursorRect
		ifNil: [ lastTextCursorRect _ origin corner: corner ]
		ifNotNil: [lastTextCursorRect
			setOrigin: (lastTextCursorRect origin min: origin)
			corner: (lastTextCursorRect corner max: corner)].
	aCanvas
		line: x0+halfW@(bottom-w-w+halfW) to: x1+halfW@top
		width: w color: textCursorColor! !

