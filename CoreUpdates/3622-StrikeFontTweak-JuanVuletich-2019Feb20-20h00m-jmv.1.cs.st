'From Cuis 5.0 of 7 November 2016 [latest update: #3621] on 20 February 2019 at 8:00:52 pm'!

!StrikeFont class methodsFor: 'instance creation' stamp: 'jmv 2/20/2019 19:58:58'!
install: aString
"
StrikeFont install: 'DejaVu'.

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
	fontDict _ nil.
	#(5 6 7 8 9 10 11 12 14 17 22 28 36 46 60 80) do: [ :s |
		(self create: aString size: s bold: true italic: true boldItalic: false) ifNotNil: [ :font |
			fontDict ifNil: [ fontDict _ AvailableFonts at: aString ifAbsentPut: Dictionary new ].
			fontDict
				at: s
				put: font ]].
	Preferences restoreDefaultFonts! !

