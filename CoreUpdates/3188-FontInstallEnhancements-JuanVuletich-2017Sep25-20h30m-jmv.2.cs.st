'From Cuis 5.0 of 7 November 2016 [latest update: #3187] on 25 September 2017 at 10:56:05 pm'!

!Preferences class methodsFor: 'fonts' stamp: 'jmv 9/25/2017 20:23:26'!
defaultFontFamily
	"Answer the default font family name"

	^self parameters at: #defaultFontFamily ifAbsentPut: [ AbstractFont familyNames first ]! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 9/25/2017 20:33:41'!
setDefaultFont: fontFamilyName spec: defaultFontsSpec
		
	| font |
	defaultFontsSpec do: [ :triplet |
		font _ AbstractFont familyName: fontFamilyName pointSize: triplet second.
		font ifNil: [ font _ AbstractFont default ]. 
		triplet size > 2 ifTrue: [
			font _ font emphasized: triplet third ].
		self
			perform: triplet first
			with: font]! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 9/25/2017 20:22:40'!
setDefaultFontFamilyTo: aString

	self parameters at: #defaultFontFamily put: aString! !


!Preferences class methodsFor: 'fonts' stamp: 'jmv 9/25/2017 20:40:23'!
restoreDefaultFonts
	"Since this is called from menus, we can take the opportunity to prompt for missing font styles.
	Preferences restoreDefaultFonts
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 9)
			(setListFontTo: 9)
			(setMenuFontTo: 10)
			(setWindowTitleFontTo: 12)
			(setCodeFontTo: 9)
			(setButtonFontTo: 9))! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/25/2017 20:40:14'!
bigFonts
	"Sets not only fonts but other GUI elements
	to fit high resolution or large screens
	Preferences bigFonts
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 11)
			(setListFontTo: 11)
			(setMenuFontTo: 12)
			(setWindowTitleFontTo: 14)
			(setCodeFontTo: 11)
			(setButtonFontTo: 11)).
	Preferences enable: #biggerCursors! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/25/2017 20:40:20'!
hugeFonts
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences hugeFonts
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 17)
			(setListFontTo: 17)
			(setMenuFontTo: 17)
			(setWindowTitleFontTo: 22)
			(setCodeFontTo: 17)
			(setButtonFontTo: 17)).
	Preferences enable: #biggerCursors! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/25/2017 20:40:26'!
smallFonts
	"Sets not only fonts but other GUI elements
	to fit low resolution or small screens
	Preferences smallFonts
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 8)
			(setListFontTo: 8)
			(setMenuFontTo: 8)
			(setWindowTitleFontTo: 11)
			(setCodeFontTo: 8)
			(setButtonFontTo: 8)).
	Preferences disable: #biggerCursors! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/25/2017 20:40:30'!
standardFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences standardFonts
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 9)
			(setListFontTo: 9)
			(setMenuFontTo: 10)
			(setWindowTitleFontTo: 12)
			(setCodeFontTo: 9)
			(setButtonFontTo: 9)).
	Preferences disable: #biggerCursors! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/25/2017 20:40:32'!
tinyFonts
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences tinyFonts
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 7)
			(setListFontTo: 5)
			(setMenuFontTo: 5)
			(setWindowTitleFontTo: 7)
			(setCodeFontTo: 5)
			(setButtonFontTo: 5)).
	Preferences disable: #biggerCursors! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/25/2017 20:40:36'!
veryBigFonts
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences veryBigFonts
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 14)
			(setListFontTo: 14)
			(setMenuFontTo: 14)
			(setWindowTitleFontTo: 17)
			(setCodeFontTo: 14)
			(setButtonFontTo: 14)).
	Preferences enable: #biggerCursors! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/25/2017 20:40:39'!
verySmallFonts
	"Sets not only fonts but other GUI elements
	to fit low resolution or small screens
	Preferences verySmallFonts
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 8)
			(setListFontTo: 7)
			(setMenuFontTo: 7)
			(setWindowTitleFontTo: 9)
			(setCodeFontTo: 7)
			(setButtonFontTo: 7)).
	Preferences disable: #biggerCursors! !


!AbstractFont methodsFor: 'displaying' stamp: 'jmv 9/25/2017 20:24:32'!
on: aGrafPort displayString: aString from: firstIndex to: lastIndex at: p color: color
	"Answer last affected pixel position
	Answer nil if nothing was done"

	| char fallbackFont x y answer |
	fallbackFont _ (AbstractFont
		familyName: Preferences defaultFontFamily
		aroundPointSize: self pointSize*0.9)
			emphasized: self emphasis.
	x _ p x.
	y _ p y.
	firstIndex to: lastIndex do: [ :i |
		char _ aString at: i.
		answer _ aGrafPort displayString: char asString from: 1 to: 1 at: x@y strikeFont: fallbackFont color: color.
		x _ x + (self widthOf: char) ].
	^answer! !


!AbstractFont class methodsFor: 'instance accessing' stamp: 'jmv 9/25/2017 20:25:27'!
familyName: aString aroundPointSize: aNumber
	"
	AbstractFont familyName: Preferences defaultFontFamily aroundPointSize: 120
	"
	| familyDictionary found |
	familyDictionary _ AvailableFonts at: aString ifAbsent: [^nil].
	^familyDictionary at: aNumber ifAbsent: [
		familyDictionary do: [ :font |
			(found isNil or: [ (found pointSize - aNumber) abs > (font pointSize - aNumber) abs ])
				ifTrue: [ found _ font ]].
			found
		]! !

!AbstractFont class methodsFor: 'instance accessing' stamp: 'jmv 9/25/2017 20:25:34'!
familyName: aString pointSize: aNumber
	"
	AbstractFont familyName: Preferences defaultFontFamily pointSize: 12
	"
	| familyDictionary |
	familyDictionary _ AvailableFonts at: aString ifAbsent: [^nil].
	^familyDictionary at: aNumber ifAbsent: nil! !

!AbstractFont class methodsFor: 'instance accessing' stamp: 'jmv 9/25/2017 20:25:58'!
pointSizesFor: aString
	"
	AbstractFont pointSizesFor: Preferences defaultFontFamily
	"
	| familyDictionary |
	familyDictionary _ AvailableFonts at: aString ifAbsent: [^#()].
	^familyDictionary keys sort! !

!AbstractFont class methodsFor: 'class initialization' stamp: 'jmv 9/25/2017 20:25:47'!
initialize
	"AvailableFonts is a dictionary whose keys are family names, such as 'DejaVu Sans' and values are family dictionaries
	family dictionaries have keys that are integers (point sizes such as 10 or 12) and values instances of the Font hierarcy
	
	Fonts with emphasis (such as bold or italic) are derivative fonts of the one found in the family dictionary"
	
	AvailableFonts _ Dictionary new! !


!StrikeFont methodsFor: 'accessing' stamp: 'jmv 9/25/2017 20:45:55'!
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
		baseKern _ baseKern - ((self height-1-self ascent+4)//4 max: 0)  
		- (((self ascent-5+4)//4 max: 0)) ].
	^baseKern! !

!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 9/25/2017 20:44:21'!
syntheticSubscript
	"Build and answer a derivative that is Subscript."

	| derivative |
	derivative _ ((AbstractFont familyName: Preferences defaultFontFamily aroundPointSize: pointSize * 0.58)
		emphasized: emphasis)
			copy.
	derivative useShortUnderscore.
	^ derivative

"
StrikeFont allInstances do: [ :a | a reset ].
('Hi ', (Text string: 'there' attribute: TextEmphasis superscript), ' how ', (Text string: 'are' attribute: TextEmphasis subscript), ' you?') edit.

que todos, menos estos, tengan superscript y subscript en cero. Y en estos, apropiado. y en 'aca' usarlo. y listo
"! !

!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 9/25/2017 20:44:26'!
syntheticSuperscript
	"Build and answer a derivative that is Superscript."

	| derivative |
	derivative _ ((AbstractFont familyName: Preferences defaultFontFamily aroundPointSize: pointSize * 0.58)
		emphasized: emphasis)
			copy.
	derivative name: self name , 'Sup'.
	^ derivative

"
StrikeFont allInstances do: [ :a | a reset ].
('Hi ', (Text string: 'there' attribute: TextEmphasis superscript), ' how ', (Text string: 'are' attribute: TextEmphasis subscript), ' you?') edit.
"! !


!StrikeFont class methodsFor: 'instance creation' stamp: 'jmv 9/25/2017 20:17:15'!
create: fontName size: pointSize bold: includeBold italic: includeItalic boldItalic: includeBoldItalic
	"
	self create: 'DejaVu Sans Mono' size: 12 bold: true italic: true boldItalic: true
	"
	| folder base bold oblique boldOblique point |
	folder _ DirectoryEntry smalltalkImageDirectory / 'AdditionalFontData'.
	point _ pointSize asString.
	base _ [ (StrikeFont new
		buildFromForm: ((Form fromFileEntry: folder // (fontName, '-0-', point, '.bmp')) asFormOfDepth: 16)
		data: (folder // (fontName, '-0-', point, '.txt')) fileContents substrings
		name: fontName, ' ', point)
			pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | ex print. nil ].
	includeBold ifTrue: [
		bold _ [ (StrikeFont new
			buildFromForm: ((Form fromFileEntry: folder // (fontName, '-1-', point, '.bmp')) asFormOfDepth: 16)
			data: (folder // (fontName, '-1-', point, '.txt')) fileContents substrings
			name: fontName, ' ', point, 'B')
				emphasis: 1;
				pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | ex print. nil ]].
	includeItalic ifTrue: [
		oblique _ [ (StrikeFont new
			buildFromForm: ((Form fromFileEntry: folder // (fontName, '-2-', point, '.bmp')) asFormOfDepth: 16)
			data: (folder // (fontName, '-2-', point, '.txt')) fileContents substrings
			name: fontName, ' ', point, 'I')
				emphasis: 2;
				pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | ex print. nil ]].
	includeBoldItalic ifTrue: [
		boldOblique _ [ (StrikeFont new
			buildFromForm: ((Form fromFileEntry: folder // (fontName, '-3-', point, '.bmp')) asFormOfDepth: 16)
			data: (folder // (fontName, '-3-', point, '.txt')) fileContents substrings
			name: fontName, ' ', point, 'BI')
				emphasis: 3;
				pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | ex print. nil ]].
	"We have a regular, base font. Make others derivatives of it"
	base ifNotNil: [
		bold ifNotNil: [
			base derivativeFont: bold at: 1 ].
		oblique ifNotNil: [
			base derivativeFont: oblique at: 2].
		boldOblique ifNotNil: [
			base derivativeFont: boldOblique at: 3 ].
		^base ].
	"We don't have a base, regular font."
	oblique ifNotNil: [
		oblique emphasis: 0.	"Hacky. Non regular fonts can not have derivatives. Should change this?"
		bold ifNotNil: [
			oblique derivativeFont: bold at: 1 ].
		boldOblique ifNotNil: [
			oblique derivativeFont: boldOblique at: 3 ].
		^oblique ].
	bold ifNotNil: [
		bold emphasis: 0.	"Hacky. Non regular fonts can not have derivatives. Should change this?"
		boldOblique ifNotNil: [
			bold derivativeFont: boldOblique at: 3 ].
		^bold ].
	boldOblique ifNotNil: [
		^boldOblique ].
	^nil! !

!StrikeFont class methodsFor: 'instance creation' stamp: 'jmv 9/25/2017 20:48:55'!
install: aString
"
StrikeFont install: 'DejaVu Sans'.
StrikeFont buildLargerPunctuation: 'DejaVu Sans'.
Preferences setDefaultFontFamilyTo: 'DejaVu Sans'.
Preferences standardFonts.
Character initialize.
"
"
StrikeFont install: 'DejaVu Sans Mono'.
Preferences setDefaultFontFamilyTo: 'DejaVu Sans Mono'.
Preferences bigFonts.
Character initialize.
"
"
StrikeFont install: 'Inconsolata'
StrikeFont install: '#PilGi'
StrikeFont install: 'Optima'
StrikeFont install: 'Herculanum'
StrikeFont install: 'Papyrus'
StrikeFont install: 'Handwriting - Dakota'
StrikeFont install: 'Times New Roman'
StrikeFont install: 'Apple Chancery'
StrikeFont install: 'Cochin'
StrikeFont install: 'Cracked'
StrikeFont install: 'Zapfino'
StrikeFont install: 'Brush Script MT'
StrikeFont install: 'Chalkboard'
"
	| fontDict |
	fontDict _ Dictionary new.
	#(5 6 7 8 9 10 11 12 14 17 22) do: [ :s |
		(self create: aString size: s bold: true italic: true boldItalic: false) ifNotNil: [ :font |
			fontDict
				at: s
				put: font ]].
	fontDict notEmpty ifTrue: [
		AvailableFonts at: aString put: fontDict ].
	Preferences restoreDefaultFonts! !

!StrikeFont class methodsFor: 'removing' stamp: 'jmv 9/25/2017 20:41:27'!
removeForPDA
"
StrikeFont removeForPDA
"
	| familyDict |
	familyDict _ AvailableFonts at: Preferences defaultFontFamily.
	familyDict keys do: [ :k |
		(#(5 6 7 8 9) includes: k) 
			ifTrue: [
				(familyDict at: k) derivativeFont: nil at: 0 ]
			ifFalse: [
				familyDict removeKey: k ]].
	
	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 8)
			(setListFontTo: 6)
			(setMenuFontTo: 7)
			(setWindowTitleFontTo: 9)
			(setCodeFontTo: 7)
			(setButtonFontTo: 7))! !

!StrikeFont class methodsFor: 'removing' stamp: 'jmv 9/25/2017 20:42:25'!
removeMostFonts
"
StrikeFont removeMostFonts
"
	| familyDict |
	Preferences disable: #italicsInShout.
	SHTextStylerST80 initialize.
	familyDict _ AvailableFonts at: Preferences defaultFontFamily.
	familyDict keys do: [ :k |
		(#(8 10 12) includes: k) 
			ifTrue: [
				(familyDict at: k) derivativeFont: nil at: 0 ]
			ifFalse: [
				familyDict removeKey: k ]].

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 10)
			(setListFontTo: 10)
			(setMenuFontTo: 10)
			(setWindowTitleFontTo: 12)
			(setCodeFontTo: 10)
			(setButtonFontTo: 10))! !

!StrikeFont class methodsFor: 'removing' stamp: 'jmv 9/25/2017 20:42:49'!
removeSomeFonts
"
StrikeFont removeSomeFonts
"
	| familyDict |
	familyDict _ AvailableFonts at: Preferences defaultFontFamily.
	familyDict keys do: [ :k |
		"No boldItalic for the followint"
		(#(5 6 7 8 9 10 11 12 14 17 22) includes: k)
			ifTrue: [ (familyDict at: k) derivativeFont: nil at: 3 ].
		"No derivatives at all for the following"
		(#() includes: k)
			ifTrue: [ (familyDict at: k) derivativeFont: nil at: 0 ].
		"Sizes to keep"
		(#(5 6 7 8 9 10 11 12 14 17 22) includes: k) 
			ifFalse: [ familyDict removeKey: k ]].

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 9)
			(setListFontTo: 9)
			(setMenuFontTo: 10)
			(setWindowTitleFontTo: 12)
			(setCodeFontTo: 9)
			(setButtonFontTo: 9))! !

!StrikeFont class methodsFor: 'character shapes' stamp: 'jmv 9/25/2017 20:45:01'!
buildLargerPunctuation: familyName
	"
	StrikeFont buildLargerPunctuation: 'DejaVu Sans'
	"
	| form form2 f10 f11 f12 f9 |

	f9 _ AbstractFont familyName: familyName pointSize: 9.
	f10 _ AbstractFont familyName: familyName pointSize: 10.
	f11 _ AbstractFont familyName: familyName pointSize: 11.
	f12 _ AbstractFont familyName: familyName pointSize: 12.


	f9 takeGlyphFor: $. from: $. in: f12.
	f9 takeGlyphFor: $, from: $, in: f12.
	
	form _ f9 glyphAt: $..
	form copy: (0@9 extent: 3@14) from: form to: 0@4 rule: Form and.
	f9 glyphAt: $: put: form.

	form _ f9 glyphAt: $,.
	form2 _ f9 glyphAt: $..
	form copy: (0@9 extent: 3@14) from: form2 to: 1@4 rule: Form and.
	f9 glyphAt: $; put: form.



	f10 takeGlyphFor: $. from: $. in: f12.
	f10 takeGlyphFor: $, from: $, in: f12.

	form _ f10 glyphAt: $. .
	form copy: (0@9 extent: 3@14) from: form to: 0@4 rule: Form and.
	f10 glyphAt: $: put: form.

	form _ f10 glyphAt: $,.
	form2 _ f10 glyphAt: $..
	form copy: (0@9 extent: 3@14) from: form2 to: 1@4 rule: Form and.
	f10 glyphAt: $; put: form.



	f11 takeGlyphFor: $. from: $. in: f12.
	f11 takeGlyphFor: $, from: $, in: f12.
	f11 takeGlyphFor: $: from: $: in: f12.
	f11 takeGlyphFor: $; from: $; in: f12! !


!UpdatingStringMorph class methodsFor: 'new-morph participation' stamp: 'jmv 9/25/2017 20:44:33'!
initializedInstance
	"Answer a digital clock"

	| newInst |
	newInst := self 
		contents: '' 
		font: (AbstractFont familyName: Preferences defaultFontFamily pointSize: 22) 
		emphasis: AbstractFont  boldCode.
	newInst 
		stepTime: 500; "half a second"
		target: [String streamContents: [ :strm | DateAndTime now printHMSOn: strm]] ;
		getSelector: #value.
		
	^ newInst! !

