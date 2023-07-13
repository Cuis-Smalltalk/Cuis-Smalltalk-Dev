'From Cuis 6.0 [latest update: #5916] on 12 July 2023 at 8:25:10 pm'!

!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 7/12/2023 20:23:39'!
fileNameFormattedFrom: entry namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad
	"entry is a 5-element array of the form:
		(name creationTime modificationTime dirFlag fileSize)"
	| sizeStr nameStr paddedNameStr dateStr someSpaces sizeDigits sizeDigitsAndCommas spacesToAdd font spaceWidth |
	font := Preferences at: #standardListFont.
	spaceWidth := font widthOf: $ .
	nameStr := entry isDirectory
		ifTrue: [ entry name , self folderString ]
		ifFalse: [ entry name ].
	spacesToAdd := namePad - (font widthOfString: nameStr) // spaceWidth.
	paddedNameStr := nameStr ,
		(String
			new: spacesToAdd
			withAll: $ ).
	dateStr := (entry modificationTime date printFormat: #(3 2 1 $/ 1 1 2 )) , '  ' ,
		(String streamContents: [ :s |
			entry modificationTime time
				print24: true
				showSeconds: true
				on: s ]).
	sizeDigits := entry fileSize printString size.
	sizeStr := entry fileSize printStringWithCommas.
	sizeDigitsAndCommas := sizeStr size.
	spacesToAdd := sizeWithCommasPad - sizeDigitsAndCommas.
	"Usually a space takes the same space as a comma, and half the space of a digit.
	Pad with 2 spaces for each missing digit and 1 space for each missing comma"
	(font widthOf: Character space) ~= (font widthOf: $, )
		ifTrue: [spacesToAdd := spacesToAdd + sizePad - sizeDigits max: 0].
	sizeStr := (String new: spacesToAdd withAll: $ ) , sizeStr.
	someSpaces := String new: 6 withAll: $ .
	"
	sortMode = #name ifTrue: [ ^ paddedNameStr , someSpaces , '( ' , dateStr , someSpaces , sizeStr , ' )' ].
	sortMode = #date ifTrue: [ ^ '( ' , dateStr , someSpaces , sizeStr , ' )' , someSpaces , nameStr ].
	sortMode = #size ifTrue: [ ^ '( ' , sizeStr , someSpaces , dateStr , ' )' , someSpaces , nameStr ].
	"
	^ paddedNameStr , someSpaces , '( ' , dateStr , someSpaces , sizeStr , ' )' .! !


!FileIOAccessor methodsFor: 'primitives' stamp: 'jmv 7/12/2023 20:20:16'!
primFixedNameLookupEntryIn: fullPathAsUtf8Bytes index: index
	"Fix the malformed String answered by the primitive. See comment at #primLookupEntryIn:index:"

	| answer |
	answer := self primLookupEntryIn: fullPathAsUtf8Bytes index: index.
	answer isArray ifTrue: [
		answer at: 1 put: (UnicodeString fromUtf8Bytes: answer first asByteArray) asByteStringIfAscii ].
	^answer! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
DirectoryEntry releaseClassCachedState!

