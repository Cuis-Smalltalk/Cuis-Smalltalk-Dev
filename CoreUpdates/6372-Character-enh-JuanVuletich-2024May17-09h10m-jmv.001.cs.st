'From Cuis7.1 [latest update: #6367] on 17 May 2024 at 9:18:14 am'!

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 5/17/2024 09:12:17'!
backspace
	"Answer the Character representing a backspace."

	^`Character codePoint: 8`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 5/17/2024 09:11:54'!
cr
	"Answer the Character representing a carriage return."

	^`Character codePoint: 13`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 5/17/2024 09:12:34'!
delete

	^ `Character codePoint: 127`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 5/17/2024 09:12:49'!
end

	^ `Character codePoint: 4`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 5/17/2024 09:13:10'!
enter
	"Answer the Character representing enter."

	^ `Character codePoint: 3`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 5/17/2024 09:13:24'!
escape
	"Answer the ASCII ESC character"

	^ `Character codePoint: 27`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 5/17/2024 09:13:34'!
home

	^ `Character codePoint: 1`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 5/17/2024 09:13:43'!
insert

	^ `Character codePoint: 5`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 5/17/2024 09:11:50'!
lf
	"Answer the Character representing a linefeed."

	^`Character codePoint: 10`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 5/17/2024 09:14:06'!
newPage
	"Answer the Character representing a form feed."

	^ `Character codePoint: 12`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 5/17/2024 09:14:16'!
null
	"Answer the null Character."

	^ `Character codePoint: 0`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 5/17/2024 09:14:30'!
pageDown

	^ `Character codePoint: 12`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 5/17/2024 09:14:40'!
pageUp

	^ `Character codePoint: 11`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 5/17/2024 09:14:52'!
shortUnderscore
	"Answer the Character representing very short (or invisible) underscore.
	Used to optionally mark subscript in code."

	^ `Character codePoint: 127`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 5/17/2024 09:15:02'!
smalltalkDownArrow
	"
	The use of code 31 for a down arrow is not part of the ASCII or Unicode standards.
	This code is generally not used in text.
	Cuis holds a down arrow glyph at this code. This is for possible future use.

	The actual glyph used is decided by the TrueType font. See #mapAndStoreGlyphsUsing:
		and references to FallbackArrowGlyphs.

	Character smalltalkDownArrow
	"
	^ `Character codePoint: 31`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 5/17/2024 09:15:08'!
smalltalkLeftArrow
	"
	The use of code 28 for a left arrow is not part of the ASCII or Unicode standards.
	This code is generally not used in text files.

	Cuis holds a left arrow glyph at this code, that is only used to show $_ as a left arrow,
	when used for assignment of a Smalltalk variable.
	Note that it will be replaced by $_ when saving code files or exporting to OS clipboard.
	Browse senders to see how this works.

	The actual glyph used is decided by the TrueType font. See #mapAndStoreGlyphsUsing:
		and references to FallbackArrowGlyphs.

	Character smalltalkLeftArrow
	"
	^ `Character codePoint: 28`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 5/17/2024 09:15:13'!
smalltalkRightArrow
	"
	The use of code 30 for a right arrow is not part of the ASCII or Unicode standards.
	This code is generally not used in text.
	Cuis holds a right arrow glyph at this code. This is for possible future use.

	The actual glyph used is decided by the TrueType font. See #mapAndStoreGlyphsUsing:
		and references to FallbackArrowGlyphs.

	Character smalltalkRightArrow
	"
	^ `Character codePoint: 30`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 5/17/2024 09:15:19'!
smalltalkUpArrow
	"
	The use of code 29 for an up arrow is not part of the ASCII or Unicode standards.
	This code is generally not used in text files.

	Cuis holds an up arrow glyph at this code, that is only used to show $^ as an up arrow,
	when used for return from a Smalltalk method.
	Note that it will be replaced by $^ when saving code files or exporting to OS clipboard.
	Browse senders to see how this works.

	The actual glyph used is decided by the TrueType font. See #mapAndStoreGlyphsUsing:
		and references to FallbackArrowGlyphs.

	Character smalltalkUpArrow
	"
	^ `Character codePoint: 29`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 5/17/2024 09:15:46'!
space
	"Answer the Character representing a space."

	^ `Character codePoint: 32`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 5/17/2024 09:16:04'!
tab
	"Answer the Character representing a tab."

	^ `Character codePoint: 9`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 5/17/2024 09:16:19'!
zeroWidthSpace

	^ `Character codePoint: 16r200B`! !

