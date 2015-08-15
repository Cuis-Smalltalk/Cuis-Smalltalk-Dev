'From Cuis 4.2 of 25 July 2013 [latest update: #2449] on 4 August 2015 at 11:39:36.252341 pm'!
!classDefinition: #CharacterScanner category: #'Graphics-Text'!
Object subclass: #CharacterScanner
	instanceVariableNames: 'destX lastIndex xTable map destY stopConditions text alignment leftMargin rightMargin font line runStopIndex spaceCount spaceWidth kern paragraphStyle tabWidth '
	classVariableNames: 'DefaultStopConditions StopConditionsWithPaddedSpace StopConditionsWithSpace '
	poolDictionaries: ''
	category: 'Graphics-Text'!

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 8/4/2015 23:28'!
useLeftArrow
	self characterToGlyphMap.
	characterToGlyphMap at: 96 put: 131.
	characterToGlyphMap at: 95 put: 130! !


!StrikeFont class methodsFor: 'instance creation' stamp: 'jmv 8/4/2015 21:18'!
create: fontName size: pointSize bold: includeBold italic: includeItalic boldItalic: includeBoldItalic
	"
	self create: 'popo' size: 12 bold: true italic: true boldItalic: true
	"
	| base bold oblique boldOblique point prefix |
	prefix _ 'AAFonts', FileDirectory slash, fontName.
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

!StrikeFont class methodsFor: 'instance creation' stamp: 'jmv 8/4/2015 23:25'!
install: aString
	"
StrikeFont install: 'DejaVu Sans'
StrikeFont install: 'DejaVu Sans Mono'
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
				put: font useLeftArrow ]].
	fontDict notEmpty ifTrue: [
		AvailableFonts at: aString put: fontDict ].
	Preferences restoreDefaultFonts! !


!CharacterScanner methodsFor: 'private' stamp: 'jmv 8/4/2015 23:36'!
setFont
	| attributes |
	"Set the font and other emphasis.
	In fact set actual ParagraphStyle (or nil), alignment, font and emphasis"

	self setActualFont: (text fontAt: lastIndex).
	attributes _ text attributesAt: lastIndex.
	text
		withAttributeValues: attributes
		do: [ :familyNameInText :pointSizeInText :emphasisInText :colorInText :alignmentInText :characterStyleInText :paragraphStyleInText |
			colorInText ifNotNil: [ self textColor: colorInText ].
			alignment _ alignmentInText.
			paragraphStyle _ paragraphStyleInText ].
	
	"Hardcoded color for TextAction"
	attributes do: [ :attribute |
		attribute forTextActionInfoDo: [ :info |
			self textColor: TextAction textActionColor ]].

	"Install various parameters from the font."
	spaceWidth _ font widthOf: Character space.
	tabWidth _ (font widthOf: $a) * 3.
	xTable _ font xTable.
	map _ font characterToGlyphMap! !

!CharacterScanner methodsFor: 'stop conditions' stamp: 'jmv 8/4/2015 23:33'!
tabDestX
	"This is the basic method of adjusting destX for a tab."
	^paragraphStyle
		ifNotNil: [
			paragraphStyle
				nextTabXFrom: destX
				leftMargin: leftMargin
				rightMargin: rightMargin ]
		ifNil: [
			destX - leftMargin // tabWidth + 1 * tabWidth + leftMargin min: rightMargin ]! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 8/4/2015 23:36'!
displayLine: textLine textTopLeft: textTopLeft leftInRun: leftInRun
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."

	"textTopLeft is relative to the morph currently being drawn"
	| stopCondition nowLeftInRun startIndex string lastPos |

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
				self setActualFont: ps font.
				ps color ifNotNil: [ :color | self textColor: color ].
				alignment _ ps alignment.
				paragraphStyle _ ps.
				spaceWidth _ font widthOf: Character space.
				tabWidth _ (font widthOf: $a) * 3.
				xTable _ font xTable.
				map _ font characterToGlyphMap.
				self setStopConditions.
				text ifNotNil: [ destY _ lineY + line baseline - font ascent ]]].
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
			kern: font baseKern.
		lastIndex >= startIndex ifTrue: [
			canvas  
				drawString: string
				from: startIndex
				to: lastIndex
				at: lastPos
				font: font
				color: foregroundColor ].
		"see setStopConditions for stopping conditions for displaying."
		(self perform: stopCondition) ifTrue: [
			"Number of characters remaining in the current run"
			^ runStopIndex - lastIndex ]
	] repeat! !

!classDefinition: #CharacterScanner category: #'Graphics-Text'!
Object subclass: #CharacterScanner
	instanceVariableNames: 'destX lastIndex xTable map destY stopConditions text alignment leftMargin rightMargin font line runStopIndex spaceCount spaceWidth kern paragraphStyle tabWidth'
	classVariableNames: 'DefaultStopConditions StopConditionsWithPaddedSpace StopConditionsWithSpace'
	poolDictionaries: ''
	category: 'Graphics-Text'!
!methodRemoval: StrikeFont class #createDejaVu:!
StrikeFont class removeSelector: #createDejaVu:!
!methodRemoval: StrikeFont class #installBrushScriptMT!
StrikeFont class removeSelector: #installBrushScriptMT!
!methodRemoval: StrikeFont class #installDejaVu!
StrikeFont class removeSelector: #installDejaVu!
!methodRemoval: StrikeFont class #installDejaVu2!
StrikeFont class removeSelector: #installDejaVu2!
