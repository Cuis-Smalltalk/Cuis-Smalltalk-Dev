'From Cuis7.5 [latest update: #7591] on 19 September 2025 at 3:33:06 pm'!

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/19/2025 15:01:42'!
backspace
	"Answer the Character representing a backspace.
	This character is generally not used in text editors and files, and doesn't have a visible glyph.
	This is the code for the [<-] backspace key. See #kbdTest See #primGetNextEvent:"

	^`Character codePoint: 8`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/19/2025 15:02:59'!
cr
	"Answer the Character representing a carriage return.
	This character is generally not used in text editors and files, because Cuis uses the LF line ending convention.
	It has a visible glyph, to make it easier to spot text that doesn't conform to the Cuis line ending convention.
	This is the code for the [ENTER] or [RETURN] key. See #kbdTest See #primGetNextEvent:"

	^`Character codePoint: 13`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/19/2025 15:06:45'!
end
	"Answer the Character for the [END] key. See #kbdTest See #primGetNextEvent:
	This character is generally not used in text editors and files, and doesn't have a visible glyph."

	^ `Character codePoint: 4`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/19/2025 15:03:33'!
escape
	"Answer the ASCII ESC character
	This character is generally not used in text editors and files, and doesn't have a visible glyph.
	This is the code for the [ESC] key. See #kbdTest See #primGetNextEvent:"

	^ `Character codePoint: 27`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/19/2025 15:07:18'!
home
	"Answer the Character for the [HOME] key. See #kbdTest See #primGetNextEvent:
	This character is generally not used in text editors and files, and doesn't have a visible glyph."

	^ `Character codePoint: 1`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/19/2025 15:12:40'!
insert
	"Answer the Character for the [INSERT] key. See #kbdTest See #primGetNextEvent:
	This character is generally not used in text editors and files, and doesn't have a visible glyph."

	^ `Character codePoint: 5`! !

!Character class methodsFor: 'accessing untypeable characters' library: #MinimalKernel01 stamp: 'jmv 9/19/2025 15:14:20'!
lf
	"Answer the Character representing a linefeed.
	It is the standard line terminator character for systems following the nearly universal LF line ending convention.
	It doesn't have a visible glyph."

	^`Character codePoint: 10`! !

!Character class methodsFor: 'accessing untypeable characters' library: #MinimalKernel01 stamp: 'jmv 9/19/2025 15:14:49'!
newLineCharacter
	"Answer the Character representing a line terminator or newLine, that is, a linefeed.
	This should be the only method in the system that knows about this detail.
	Everybody else should eventually ask us."

	^self lf! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/19/2025 15:11:08'!
newPage
	"Answer the Character representing a form feed.
	This character is also returned for the [Page Down] key. See #pageDown
	This character doesn't have a visible glyph."

	^ `Character codePoint: 12`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/19/2025 15:16:00'!
null
	"Answer the null Character.
	Sometimes it is used as a String terminator. It is not used in files often, but some files may be 'null terminated', as some API calls."

	^ `Character codePoint: 0`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/19/2025 15:10:14'!
pageDown
	"Answer the Character for the [Page Down] key. See #kbdTest See #primGetNextEvent:
	This character may be used in text files. The ASCII control character name for it is 'Form Feed'. See #newPage
	This character doesn't have a visible glyph."

	^ `Character codePoint: 12`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/19/2025 15:08:39'!
pageUp
	"Answer the Character for the [Page Up] key. See #kbdTest See #primGetNextEvent:
	This character is generally not used in text editors and files, and doesn't have a visible glyph."

	^ `Character codePoint: 11`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/19/2025 15:03:08'!
smalltalkDownArrow
	"
	The use of code 31 for a down arrow is not part of the ASCII or Unicode standards.
	This character is generally not used in text.
	Cuis holds a down arrow glyph at this code. This is for possible future use.

	The actual glyph used is decided by the TrueType font. See #mapAndStoreGlyphsUsing:
		and references to FallbackArrowGlyphs.

	Character smalltalkDownArrow
	"
	^ `Character codePoint: 31`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/19/2025 15:03:13'!
smalltalkLeftArrow
	"
	The use of code 28 for a left arrow is not part of the ASCII or Unicode standards.
	This character is generally not used in text files.

	Cuis holds a left arrow glyph at this code, that is only used to show $_ as a left arrow,
	when used for assignment of a Smalltalk variable.
	Note that it will be replaced by $_ when saving code files or exporting to OS clipboard.
	Browse senders to see how this works.

	The actual glyph used is decided by the TrueType font. See #mapAndStoreGlyphsUsing:
		and references to FallbackArrowGlyphs.

	Character smalltalkLeftArrow
	"
	^ `Character codePoint: 28`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/19/2025 15:03:16'!
smalltalkRightArrow
	"
	The use of code 29 for a right arrow is not part of the ASCII or Unicode standards.
	This character is generally not used in text.
	Cuis holds a right arrow glyph at this code. This is for possible future use.

	The actual glyph used is decided by the TrueType font. See #mapAndStoreGlyphsUsing:
		and references to FallbackArrowGlyphs.

	Character smalltalkRightArrow
	"
	^ `Character codePoint: 29`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/19/2025 15:03:19'!
smalltalkUpArrow
	"
	The use of code 30 for an up arrow is not part of the ASCII or Unicode standards.
	This character is generally not used in text files.

	Cuis holds an up arrow glyph at this code, that is only used to show $^ as an up arrow,
	when used for return from a Smalltalk method.
	Note that it will be replaced by $^ when saving code files or exporting to OS clipboard.
	Browse senders to see how this works.

	The actual glyph used is decided by the TrueType font. See #mapAndStoreGlyphsUsing:
		and references to FallbackArrowGlyphs.

	Character smalltalkUpArrow
	"
	^ `Character codePoint: 30`! !

!Character class methodsFor: 'private' stamp: 'jmv 9/19/2025 15:30:46'!
constantNames
	^ #(backQuote backspace cr delete doubleQuote end escape home insert lf newPage noBreakSpace null pageUp singleQuote smalltalkDownArrow smalltalkLeftArrow smalltalkRightArrow smalltalkUpArrow space tab zeroWidthSpace)! !

