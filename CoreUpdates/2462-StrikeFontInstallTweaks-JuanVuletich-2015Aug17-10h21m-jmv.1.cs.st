'From Cuis 4.2 of 25 July 2013 [latest update: #2461] on 17 August 2015 at 10:21:59.485282 am'!

!StrikeFont methodsFor: 'building' stamp: 'jmv 8/17/2015 10:16'!
buildFromForm: allGlyphs data: data name: aString

	| x glyph |
	pointSize _ data first asNumber.
	ascent _ data second asNumber.
	descent _ data third asNumber.
	
	minAscii _ 32.
	maxAscii _ 255.
	name _ aString.
	type _ 0.  "ignored for now"
	superscript _ ascent - descent // 3.	
	subscript _ descent - ascent // 3.	
	emphasis _ 0.

	xTable _ (Array new: 258) atAllPut: 0.
	maxWidth _ 0.
	glyphs _ allGlyphs.
	x _ 0.
	minAscii to: maxAscii+1 do: [ :i |
		x _ (data at: i-minAscii+4) asNumber.
		xTable at: i+1 put: x].
	xTable at: 258 put: x.
	self reset.
	derivativeFonts _ nil.
	
	"Set left arrow and up arrow as default glyphs"
	glyph _ self glyphAt: (Character value: 130).
	self glyphAt: (Character value: 94) put: glyph.
	glyph _ self glyphAt: (Character value: 131).
	self glyphAt: (Character value: 95) put: glyph.! !


!StrikeFont class methodsFor: 'instance creation' stamp: 'jmv 8/17/2015 10:20'!
create: fontName size: pointSize bold: includeBold italic: includeItalic boldItalic: includeBoldItalic
	"
	self create: 'DejaVu Sans Mono' size: 12 bold: true italic: true boldItalic: true
	"
	| base bold oblique boldOblique point prefix |
	prefix _ 'AdditionalFontData', FileDirectory slash, fontName.
	point _ pointSize asString.
	base _ [ (StrikeFont new
		buildFromForm: (Form fromFileNamed: prefix, '-0-', point, '.bmp')
		data: (FileStream oldFileNamed: prefix, '-0-', point, '.txt') contentsOfEntireFile substrings
		name: fontName, ' ', point)
			pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | ex print. nil ].
	includeBold ifTrue: [
		bold _ [ (StrikeFont new
			buildFromForm: (Form fromFileNamed: prefix, '-1-', point, '.bmp')
			data: (FileStream oldFileNamed: prefix, '-1-', point, '.txt') contentsOfEntireFile substrings
			name: fontName, ' ', point, 'B')
				emphasis: 1;
				pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | ex print. nil ]].
	includeItalic ifTrue: [
		oblique _ [ (StrikeFont new
			buildFromForm: (Form fromFileNamed: prefix, '-2-', point, '.bmp')
			data: (FileStream oldFileNamed: prefix, '-2-', point, '.txt') contentsOfEntireFile substrings
			name: fontName, ' ', point, 'I')
				emphasis: 2;
				pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | ex print. nil ]].
	includeBoldItalic ifTrue: [
		boldOblique _ [ (StrikeFont new
			buildFromForm: (Form fromFileNamed: prefix, '-3-', point, '.bmp')
			data: (FileStream oldFileNamed: prefix, '-3-', point, '.txt') contentsOfEntireFile substrings
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

!StrikeFont class methodsFor: 'instance creation' stamp: 'jmv 8/17/2015 10:17'!
install: aString
"
StrikeFont install: 'DejaVu Sans Mono'
"
	"
StrikeFont install: 'DejaVu Sans'
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
	"Just try a lot of sizes. Will ignore missing files."
	1 to: 200 do: [ :s |
		(self create: aString size: s bold: true italic: true boldItalic: true) ifNotNil: [ :font |
			fontDict
				at: s
				put: font ]].
	fontDict notEmpty ifTrue: [
		AvailableFonts at: aString put: fontDict ].
	Preferences restoreDefaultFonts! !

