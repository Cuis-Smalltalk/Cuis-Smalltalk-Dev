'From Cuis 5.0 [latest update: #4812] on 28 August 2021 at 7:17:31 pm'!

!ContextPart methodsFor: 'debugger access' stamp: 'jmv 8/28/2021 19:05:56'!
tempsAndValuesLimitedTo: sizeLimit indent: indent
	"Return a string of the temporary variabls and their current values"

	| aStream tempNames title |
	aStream _ WriteStream on: (String new: 100).
	tempNames _ self tempNames.
	1 to: self size do: [ :index |
		title _ tempNames size = self size ifTrue: [tempNames at: index]  ifFalse: [ 'argOrTemp', index printString ].
		indent timesRepeat: [aStream tab].
		aStream nextPutAll: title; nextPut: $:; space; tab.
		aStream nextPutAll: 
			((self tempAt: index) printStringLimitedTo: (sizeLimit -3 -title size max: 1)).
		aStream newLine].
	^aStream contents! !

