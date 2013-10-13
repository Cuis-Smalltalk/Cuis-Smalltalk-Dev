'From Cuis 4.2 of 25 July 2013 [latest update: #1849] on 12 October 2013 at 9:35:25.504478 pm'!

!StrikeFont methodsFor: 'accessing' stamp: 'jmv 10/12/2013 21:33'!
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
				ifTrue: [ 1 ]
				ifFalse: [ 0 ].
			(italic not and: [pointSize = 12]) ifTrue: [
				baseKern _ baseKern -1 ].
			pointSize >= 13 ifTrue: [
				baseKern _ baseKern -1 ].
			pointSize >= 20 ifTrue: [
				baseKern _ baseKern -1 ]"]
		ifFalse: [
			baseKern _ pointSize < 12
				ifTrue: [ 1 ]
				ifFalse: [ 0 ].
			italic ifTrue: [
				baseKern _ baseKern + 1]]".
	
	"If synthetic italic"
	"See makeItalicGlyphs"
	(self isSynthetic and: [ emphasis = 3 ]) ifTrue: [
		baseKern _ baseKern + ((self height-1-self ascent+4)//4 max: 0)  
		+ (((self ascent-5+4)//4 max: 0)) ].
	^baseKern! !

