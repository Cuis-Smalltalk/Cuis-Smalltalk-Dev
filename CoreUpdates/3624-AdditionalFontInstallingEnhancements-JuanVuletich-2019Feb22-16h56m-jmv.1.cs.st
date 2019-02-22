'From Cuis 5.0 of 7 November 2016 [latest update: #3623] on 22 February 2019 at 5:03:32 pm'!

!Preferences class methodsFor: 'fonts' stamp: 'jmv 2/22/2019 17:03:19'!
setDefaultFont: fontFamilyName spec: defaultFontsSpec
		
	| font |
	defaultFontsSpec do: [ :triplet |
		font _ AbstractFont familyName: fontFamilyName pointSize: triplet second.
		font ifNil: [
			StrikeFont install: Preferences defaultFontFamily.
			font _ AbstractFont familyName: fontFamilyName pointSize: triplet second ].
		font ifNil: [ font _ AbstractFont default ]. 
		triplet size > 2 ifTrue: [
			font _ font emphasized: triplet third ].
		self
			perform: triplet first
			with: font]! !


!StrikeFont class methodsFor: 'instance creation' stamp: 'jmv 2/22/2019 16:57:45'!
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
			pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | nil ].
	includeBold ifTrue: [
		bold _ [ (StrikeFont new
			buildFromForm: ((Form fromFileEntry: folder // (fontName, '-1-', point, '.bmp')) asFormOfDepth: 16)
			data: (folder // (fontName, '-1-', point, '.txt')) fileContents substrings
			name: fontName, ' ', point, 'B')
				emphasis: 1;
				pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | nil ]].
	includeItalic ifTrue: [
		oblique _ [ (StrikeFont new
			buildFromForm: ((Form fromFileEntry: folder // (fontName, '-2-', point, '.bmp')) asFormOfDepth: 16)
			data: (folder // (fontName, '-2-', point, '.txt')) fileContents substrings
			name: fontName, ' ', point, 'I')
				emphasis: 2;
				pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | nil ]].
	includeBoldItalic ifTrue: [
		boldOblique _ [ (StrikeFont new
			buildFromForm: ((Form fromFileEntry: folder // (fontName, '-3-', point, '.bmp')) asFormOfDepth: 16)
			data: (folder // (fontName, '-3-', point, '.txt')) fileContents substrings
			name: fontName, ' ', point, 'BI')
				emphasis: 3;
				pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | nil ]].
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

!StrikeFont class methodsFor: 'instance creation' stamp: 'jmv 2/22/2019 16:59:13'!
install: aString
"
StrikeFont install: 'DejaVu'.
"
"
StrikeFont install: 'DejaVu Sans Mono'.
Preferences setDefaultFontFamilyTo: 'DejaVu Sans Mono'.
Preferences bigFonts.
Character initialize.
"
"
StrikeFont install: 'DejaVu Sans'.
StrikeFont buildLargerPunctuation: 'DejaVu Sans'.
Preferences setDefaultFontFamilyTo: 'DejaVu Sans'.
Preferences standardFonts.
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
	fontDict _ nil.
	1 to: 200 do: [ :s |
		(self create: aString size: s bold: true italic: true boldItalic: false) ifNotNil: [ :font |
			font print.
			fontDict ifNil: [ fontDict _ AvailableFonts at: aString ifAbsentPut: Dictionary new ].
			fontDict
				at: s
				put: font ]].
	Preferences restoreDefaultFonts! !


!Theme class methodsFor: 'user interface' stamp: 'jmv 2/22/2019 17:02:20'!
changeFontSizes

	(MenuMorph new defaultTarget: Preferences)
		addTitle: 'Choose a size';
		addStayUpIcons;
		add: 'Very High Resolution / Big Fonts (80pix)' action: #defaultFont80;
		add: 'Very High Resolution / Big Fonts (60pix)' action: #defaultFont60;
		add: 'Very High Resolution / Reg Fonts (46pix)' action: #defaultFont46;
		add: 'Very High Resolution / Reg Fonts (36pix)' action: #defaultFont36;
		add: 'High Resolution / Big Fonts (28pix)' action: #defaultFont28;
		add: 'High Resolution / Big Fonts (22pix)' action: #defaultFont22;
		add: 'High Resolution / Reg Fonts (17pix)' action: #defaultFont17;
		add: 'High Resolution / Reg Fonts  (14pix)' action: #defaultFont14;
		add: 'Standard Resolution (11pix)' action: #defaultFont11;
		add: 'Standard Resolution (9pix)' action: #defaultFont9;
		add: 'Small Fonts (8pix)' action: #defaultFont8;
		add: 'Very small Fonts'action: #verySmallFonts;
		add: 'Tiny Fonts'action: #tinyFonts;
		popUpInWorld: self runningWorld! !

