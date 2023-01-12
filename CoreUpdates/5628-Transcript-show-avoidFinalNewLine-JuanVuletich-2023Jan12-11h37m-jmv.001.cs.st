'From Cuis 6.0 [latest update: #5627] on 12 January 2023 at 12:14:30 pm'!

!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'jmv 1/12/2023 12:13:33'!
show: anObject
	"Old TranscriptStream protocol."
	(anObject asString findTokens: Character newLineCharacter)
		do: [ :line | self nextPutAll: line ]
		separatedBy: [ self newLine ].! !

