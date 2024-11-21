'From Cuis7.1 [latest update: #6834] on 21 November 2024 at 4:06:22 pm'!

!ChangeRecord methodsFor: 'accessing' stamp: 'jmv 11/21/2024 16:02:19'!
string
	"The file is usually closed. But if it happens to be open, leave it like that."
	| string mustOpenAndClose |
	mustOpenAndClose := file closed.
	mustOpenAndClose ifTrue: [
		file openReadOnly ].
	file position: position.
	string := file nextChunk withCuisLineEndings.
	mustOpenAndClose ifTrue: [
		file close].
	^ string! !

