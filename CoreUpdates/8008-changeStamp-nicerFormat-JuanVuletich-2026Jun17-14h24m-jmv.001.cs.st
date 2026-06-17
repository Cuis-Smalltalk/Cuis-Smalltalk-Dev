'From Cuis7.9 [latest update: #8004] on 17 June 2026 at 2:32:00 pm'!

!Utilities class methodsFor: 'identification' stamp: 'jmv 17/Jun/2026 00:17:59'!
changeStamp
	"Answer a string to be pasted into source code to mark who changed it and when.
	Utilities changeStamp
	'jmv 16/Jun/2026, 9:01:18 pm (UTC)' .
	'jmv 16/Jun/2026 17:20:43' .
	"
	"^ self authorInitials , ' ' , (Date today printFormat: #(1 2 3 $/ 2 1)), ' ',
		(String streamContents: [:s | Time now print24: true showSeconds: true on: s]).
		"
	^String streamContents: [ :strm |
		strm nextPutAll: self authorInitials.
		strm space.
		DateAndTime nowUTC printNiceOn: strm ]! !

