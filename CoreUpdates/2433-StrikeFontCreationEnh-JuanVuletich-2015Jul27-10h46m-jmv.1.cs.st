'From Cuis 4.2 of 25 July 2013 [latest update: #2431] on 27 July 2015 at 10:47:11.214526 am'!

!StrikeFont class methodsFor: 'instance creation' stamp: 'jmv 7/25/2015 17:57'!
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
			pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | ex print ].
	includeBold ifTrue: [
		bold _ [ (StrikeFont new
			buildFromForm: (Form fromFileNamed: prefix, '-1-', point, '.bmp')
			data: (FileStream oldFileNamed: prefix, '-1-', point, '.txt') contentsOfEntireFile substrings
			name: fontName, ' ', point, 'B')
				emphasis: 1;
				pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | ex print ]].
	includeItalic ifTrue: [
		oblique _ [ (StrikeFont new
			buildFromForm: (Form fromFileNamed: prefix, '-2-', point, '.bmp')
			data: (FileStream oldFileNamed: prefix, '-2-', point, '.txt') contentsOfEntireFile substrings
			name: fontName, ' ', point, 'I')
				emphasis: 2;
				pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | ex print ]].
	includeBoldItalic ifTrue: [
		boldOblique _ [ (StrikeFont new
			buildFromForm: (Form fromFileNamed: prefix, '-3-', point, '.bmp')
			data: (FileStream oldFileNamed: prefix, '-3-', point, '.txt') contentsOfEntireFile substrings
			name: fontName, ' ', point, 'BI')
				emphasis: 3;
				pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | ex print ]].
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

