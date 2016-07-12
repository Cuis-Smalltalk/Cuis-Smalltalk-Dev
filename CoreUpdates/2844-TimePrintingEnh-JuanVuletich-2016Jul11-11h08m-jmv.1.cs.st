'From Cuis 4.2 of 25 July 2013 [latest update: #2843] on 11 July 2016 at 11:11:17.577207 am'!

!Time methodsFor: 'printing' stamp: 'jmv 7/11/2016 11:10:15'!
print24: hr24 showSeconds: showSeconds showSecondsFraction: showSecondsFraction on: aStream
	"Format is 'hh:mm:ss' or 'h:mm:ss am'  or, if showSeconds is false, 'hh:mm' or 'h:mm am'"

	| h m s |
	h _ self hour.
	m _ self minute.
	s _ self second.
	hr24
		ifTrue: [
			h < 10 ifTrue: [ aStream nextPutAll: '0' ].
			h printOn: aStream ]
		ifFalse: [
			h > 12
				ifTrue: [h - 12 printOn: aStream]
				ifFalse: 
					[h < 1
						ifTrue: [ 12 printOn: aStream ]
						ifFalse: [ h printOn: aStream ]]].

	aStream nextPutAll: (m < 10 ifTrue: [':0'] ifFalse: [':']).
	m printOn: aStream.

	showSeconds ifTrue: [
		aStream nextPutAll: (s < 10 ifTrue: [':0'] ifFalse: [':']).
		(self nanoSecond = 0 or: [ showSecondsFraction not ])
			ifTrue: [s asInteger printOn: aStream]
			ifFalse: [(s + (1.0 * self nanoSecond / Time nanosInSecond)) printOn: aStream]].

	hr24 ifFalse: [
		aStream nextPutAll: (h < 12 ifTrue: [' am'] ifFalse: [' pm']) ]! !

!Time methodsFor: 'printing' stamp: 'jmv 7/11/2016 11:10:21'!
print24: hr24 showSecondsFraction: showSecondsFraction on: aStream
	"Format is 'hh:mm:ss.ssssss' or 'h:mm:ss.ssssss am'  or, if showSecondsFraction is false, 'hh:mm:ss' or 'h:mm:ss am'"

	^self print24: hr24 showSeconds: true showSecondsFraction: showSecondsFraction on: aStream! !


!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 7/11/2016 11:10:29'!
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
				showSeconds: true
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
	sortMode = #size ifTrue: [ ^ '( ' , sizeStr , someSpaces , dateStr , ' )' , someSpaces , nameStr ]! !


!Time methodsFor: 'printing' stamp: 'jmv 7/11/2016 11:10:36'!
print24
	"Return as 8-digit string 'hh:mm:ss', with leading zeros if needed"

	^String streamContents: [ :aStream |
		self print24: true showSeconds: true on: aStream ]! !

!Time methodsFor: 'printing' stamp: 'jmv 7/11/2016 11:10:50'!
print24: hr24 showSeconds: showSeconds on: aStream
	"Format is 'hh:mm:ss' or 'h:mm:ss am'  or, if showSeconds is false, 'hh:mm' or 'h:mm am'"

	^self print24: hr24 showSeconds: showSeconds showSecondsFraction: false on: aStream! !

!Time methodsFor: 'printing' stamp: 'jmv 7/11/2016 11:10:56'!
printOn: aStream

	self print24: false
		showSeconds: (seconds ~= 0
				or: [ nanos ~= 0])
		showSecondsFraction: nanos ~= 0
		on: aStream! !


!Utilities class methodsFor: 'identification' stamp: 'jmv 7/11/2016 11:11:01'!
changeStamp
	"Answer a string to be pasted into source code to mark who changed it and when."
	^ self authorInitials , ' ' , Date today mmddyyyy, ' ',
		(String streamContents: [:s | Time now print24: true showSeconds: true on: s])! !

