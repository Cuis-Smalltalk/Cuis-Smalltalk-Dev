'From Cuis 4.2 of 25 July 2013 [latest update: #2610] on 1 December 2015 at 9:51:32.106509 am'!

!StrikeFont class methodsFor: 'instance creation' stamp: 'jmv 12/1/2015 09:51'!
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
	"Just try a lot of sizes. Will ignore missing files."
	1 to: 200 do: [ :s |
		(self create: aString size: s bold: true italic: true boldItalic: false) ifNotNil: [ :font |
			fontDict
				at: s
				put: font ]].
	fontDict notEmpty ifTrue: [
		AvailableFonts at: aString put: fontDict ].
	Preferences restoreDefaultFonts! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
(self confirm: 'If you have updated and unzipped AdditionalFontData/DejaVuSans.zip,
please allow for StrikeFont glyph re-creation, to add methematical symbols.

It not, it is advisable to answer "No", exit this image, prepare font data files, and restart the update process.

Otherwise check this Changeset Postscript and do it when appropriate.') ifTrue: [
	StrikeFont install: 'DejaVu'.
	StrikeFont buildLargerPunctuation: 'DejaVu'.
	Character initialize ]!

