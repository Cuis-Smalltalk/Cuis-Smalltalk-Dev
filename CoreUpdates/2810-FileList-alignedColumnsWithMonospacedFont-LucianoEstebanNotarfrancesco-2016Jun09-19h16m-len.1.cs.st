'From Cuis 4.2 of 25 July 2013 [latest update: #2795] on 9 June 2016 at 7:18:10.687491 pm'!

!FileList methodsFor: 'volume list and pattern' stamp: 'len 6/9/2016 18:22'!
fileNameFormattedFrom: entry namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad
	"entry is a 5-element array of the form:
		(name creationTime modificationTime dirFlag fileSize)"
	| sizeStr nameStr paddedNameStr dateStr someSpaces sizeDigits sizeDigitsAndCommas spacesToAdd font spaceWidth |
	font _ Preferences standardListFont.
	spaceWidth _ font widthOf: $ .
	nameStr _ entry isDirectory
		ifTrue: [ entry name , self folderString ]
		ifFalse: [ entry name ].
	spacesToAdd _ namePad - (font widthOfString: nameStr) // spaceWidth.
	paddedNameStr _ nameStr ,
		(String
			new: spacesToAdd
			withAll: $ ).
	dateStr _ ((Date fromSeconds: entry modificationTime) printFormat: #(3 2 1 $/ 1 1 2 )) , '  ' ,
		(String streamContents: [ :s |
			(Time fromSeconds: entry modificationTime \\ Time secondsInDay)
				print24: true
				on: s ]).
	sizeDigits _ entry fileSize printString size.
	sizeStr _ entry fileSize printStringWithCommas.
	sizeDigitsAndCommas _ sizeStr size.
	spacesToAdd _ sizeWithCommasPad - sizeDigitsAndCommas.
	"Usually a space takes the same space as a comma, and half the space of a digit.
	Pad with 2 spaces for each missing digit and 1 space for each missing comma"
	(font widthOf: Character space) ~= (font widthOf: $, )
		ifTrue: [spacesToAdd _ spacesToAdd + sizePad - sizeDigits max: 0].
	sizeStr _ (String new: spacesToAdd withAll: $ ) , sizeStr.
	someSpaces _ String new: 6 withAll: $ .
	sortMode = #name ifTrue: [ ^ paddedNameStr , someSpaces , '( ' , dateStr , someSpaces , sizeStr , ' )' ].
	sortMode = #date ifTrue: [ ^ '( ' , dateStr , someSpaces , sizeStr , ' )' , someSpaces , nameStr ].
	sortMode = #size ifTrue: [ ^ '( ' , sizeStr , someSpaces , dateStr , ' )' , someSpaces , nameStr ].! !

