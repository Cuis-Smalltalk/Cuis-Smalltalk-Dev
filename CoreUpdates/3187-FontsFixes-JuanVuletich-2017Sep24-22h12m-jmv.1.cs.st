'From Cuis 5.0 of 7 November 2016 [latest update: #3186] on 24 September 2017 at 10:13:16 pm'!

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/24/2017 20:34:05'!
shortUnderscore
	"Answer the Character representing very short (or invisible) underscore.
	Used to optionally mark subscript in code."

	^ Character numericValue: 127! !


!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/24/2017 20:13:24'!
infinity
	"
	Character infinity
	"
	^ $…! !


!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 9/24/2017 22:08:45'!
makeCrInvisible
	| glyph |
	glyph _ self glyphAt: Character cr.
	glyph fillWhite.
	self glyphAt: Character cr put: glyph! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 9/24/2017 22:07:50'!
makeCrVisible
	| glyph |
	glyph _ self glyphAt: (Character numericValue: 182).
	glyph border: glyph boundingBox width: 1 fillColor: Color blue.
"	glyph _ glyph reverse."
	self glyphAt: Character cr put: glyph! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 9/24/2017 22:09:18'!
makeLfInvisible
	| glyph |
	glyph _ self glyphAt: Character lf.
	glyph fillWhite.
	self glyphAt: Character lf put: glyph! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 9/24/2017 22:07:44'!
makeLfVisible
	| glyph |
	glyph _ self glyphAt: (Character numericValue: 163).
	glyph border: glyph boundingBox width: 1 fillColor: Color blue.
"	glyph _ glyph reverse."
	self glyphAt: Character lf put: glyph! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 9/24/2017 20:24:03'!
makeTabInvisible
	self characterToGlyphMap.
	characterToGlyphMap at: 10 put: (10 < minAscii ifFalse: [10] ifTrue: [maxAscii+1])! !

!StrikeFont methodsFor: 'building' stamp: 'jmv 9/24/2017 20:49:47'!
buildFromForm: allGlyphs data: data name: aString

	| x shortUnderscore firstGlyphInFiles |
	pointSize _ data first asNumber.
	ascent _ data second asNumber.
	descent _ data third asNumber.

	firstGlyphInFiles _ 257-(data size-3).
	minAscii _ 1.
	maxAscii _ 255.
	name _ aString.
	type _ 0.  "ignored for now"
	superscript _ ascent - descent // 3.	
	subscript _ descent - ascent // 3.	
	emphasis _ 0.

	xTable _ (Array new: 258) atAllPut: 0.
	maxWidth _ 0.
	glyphs _ allGlyphs depth > 16 ifTrue: [ allGlyphs asFormOfDepth: 16 ] ifFalse: [ allGlyphs ].
	x _ 0.
	4 to: data size do: [ :i |
		x _ (data at: i) asNumber.
		xTable at: i+firstGlyphInFiles+1-4 put: x].
	xTable at: 258 put: x.
	self reset.
	derivativeFonts _ nil.

	self makeCrVisible.

	"Replace glyph for 127 (a box) with a short version of the underscore (used to optionally mark subscript in code)"
	shortUnderscore _ self glyphAt: $_.
	shortUnderscore _ shortUnderscore copy: (0@0 extent: 1@shortUnderscore height).
	self glyphAt: (Character numericValue: 127) put: shortUnderscore! !


!StrikeFont class methodsFor: 'instance creation' stamp: 'jmv 9/24/2017 20:08:30'!
create: fontName size: pointSize bold: includeBold italic: includeItalic boldItalic: includeBoldItalic
	"
	self create: 'DejaVu Sans Mono' size: 12 bold: true italic: true boldItalic: true
	"
	| folder basename base bold oblique boldOblique point |
	folder _ DirectoryEntry smalltalkImageDirectory / 'AdditionalFontData'.
	basename _ fontName = 'DejaVu' ifTrue: ['DejaVu Sans'] ifFalse: [fontName].
	point _ pointSize asString.
	base _ [ (StrikeFont new
		buildFromForm: ((Form fromFileEntry: folder // (basename, '-0-', point, '.bmp')) asFormOfDepth: 16)
		data: (folder // (basename, '-0-', point, '.txt')) fileContents substrings
		name: fontName, ' ', point)
			pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | ex print. nil ].
	includeBold ifTrue: [
		bold _ [ (StrikeFont new
			buildFromForm: ((Form fromFileEntry: folder // (basename, '-1-', point, '.bmp')) asFormOfDepth: 16)
			data: (folder // (basename, '-1-', point, '.txt')) fileContents substrings
			name: fontName, ' ', point, 'B')
				emphasis: 1;
				pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | ex print. nil ]].
	includeItalic ifTrue: [
		oblique _ [ (StrikeFont new
			buildFromForm: ((Form fromFileEntry: folder // (basename, '-2-', point, '.bmp')) asFormOfDepth: 16)
			data: (folder // (basename, '-2-', point, '.txt')) fileContents substrings
			name: fontName, ' ', point, 'I')
				emphasis: 2;
				pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | ex print. nil ]].
	includeBoldItalic ifTrue: [
		boldOblique _ [ (StrikeFont new
			buildFromForm: ((Form fromFileEntry: folder // (basename, '-3-', point, '.bmp')) asFormOfDepth: 16)
			data: (folder // (basename, '-3-', point, '.txt')) fileContents substrings
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

!StrikeFont class methodsFor: 'instance creation' stamp: 'jmv 9/24/2017 20:15:42'!
install: aString
"
StrikeFont install: 'DejaVu'.
StrikeFont buildLargerPunctuation: 'DejaVu'.
Character initialize

StrikeFont install: 'DejaVu Sans Mono'.
StrikeFont buildLargerPunctuation: 'DejaVu Sans Mono'.
Character initialize
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

