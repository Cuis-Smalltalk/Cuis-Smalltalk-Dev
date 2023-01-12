'From Cuis 6.0 [latest update: #5627] on 12 January 2023 at 12:43:44 pm'!

!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'jmv 1/12/2023 12:41:54'!
show: anObject
	"Old TranscriptStream protocol."
	"
	Transcript
	show: 'Hello

		World';
	show: 'Hello
           
		World';
	newLine;
	show: 'stuff';
	newLine;
	newLine;
	show: 'done '; show: 'yeah'.
	"
	(anObject asString findTokens: `{Character newLineCharacter}` keep: `{Character newLineCharacter}`)
		do: [ :line |
			line = Character newLineCharacter asString
				ifFalse: [ self nextPutAll: line ]
				ifTrue: [ self newLine ]]! !

!Transcript class methodsFor: 'private' stamp: 'jmv 1/12/2023 12:42:43'!
finishEntry
	| newEntry |
	newEntry _ unfinishedEntry contents.
	unfinishedEntry reset.
	lastDisplayPosition _ 0.
	self addEntry: newEntry.
	self display.! !

