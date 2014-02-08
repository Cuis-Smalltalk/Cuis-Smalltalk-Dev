'From Cuis 4.2 of 25 July 2013 [latest update: #1960] on 6 February 2014 at 11:10:06.267431 pm'!

!TextModel methodsFor: 'initialization' stamp: 'jmv 2/6/2014 22:47'!
flushUndoRedoCommands

	undoRedoCommands _ ReadWriteStream on: Array new! !

!TextModel methodsFor: 'initialization' stamp: 'jmv 2/6/2014 22:47'!
initialize
	"Initialize the state of the receiver with its default contents."

	actualContents _ '' asText.
	undoRedoCommands _ ReadWriteStream on: Array new! !

