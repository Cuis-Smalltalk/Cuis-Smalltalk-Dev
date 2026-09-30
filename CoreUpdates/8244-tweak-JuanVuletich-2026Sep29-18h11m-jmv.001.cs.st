'From Cuis7.9 [latest update: #8244] on 29 September 2026 at 6:13:06 pm'!

!ClassCommentChange methodsFor: 'accessing' stamp: 'jmv 29/Sep/2026, 9:11:52 pm (UTC)'!
string
	"The file is usually closed. But if it happens to be open, leave it like that."
	| s mustOpenAndClose |
	mustOpenAndClose := file closed.
	mustOpenAndClose ifTrue: [
		file openReadOnly ].
	file position: position.
	s := file nextChunk withCuisLineEndings.
	mustOpenAndClose ifTrue: [
		file close].
	^ s! !


!ClassDefinitionChange methodsFor: 'accessing' stamp: 'jmv 29/Sep/2026, 9:11:58 pm (UTC)'!
string
	"The file is usually closed. But if it happens to be open, leave it like that."
	| s mustOpenAndClose |
	mustOpenAndClose := file closed.
	mustOpenAndClose ifTrue: [
		file openReadOnly ].
	file position: position.
	s := file nextChunk withCuisLineEndings.
	mustOpenAndClose ifTrue: [
		file close].
	^ s! !


!MethodDefinitionChange methodsFor: 'accessing' stamp: 'jmv 29/Sep/2026, 9:12:15 pm (UTC)'!
string
	"The file is usually closed. But if it happens to be open, leave it like that."
	| s mustOpenAndClose |
	mustOpenAndClose := file closed.
	mustOpenAndClose ifTrue: [
		file openReadOnly ].
	file position: position.
	s := file nextChunk withCuisLineEndings.
	mustOpenAndClose ifTrue: [
		file close].
	^ s! !


!PreambleRecord methodsFor: 'accessing' stamp: 'jmv 29/Sep/2026, 9:12:23 pm (UTC)'!
string
	"The file is usually closed. But if it happens to be open, leave it like that."
	| s mustOpenAndClose |
	mustOpenAndClose := file closed.
	mustOpenAndClose ifTrue: [
		file openReadOnly ].
	file position: position.
	s := file nextChunk withCuisLineEndings.
	mustOpenAndClose ifTrue: [
		file close].
	^ s! !


!UnspecificRecord methodsFor: 'accessing' stamp: 'jmv 29/Sep/2026, 9:12:30 pm (UTC)'!
string
	"The file is usually closed. But if it happens to be open, leave it like that."
	| s mustOpenAndClose |
	mustOpenAndClose := file closed.
	mustOpenAndClose ifTrue: [
		file openReadOnly ].
	file position: position.
	s := file nextChunk withCuisLineEndings.
	mustOpenAndClose ifTrue: [
		file close].
	^ s! !

