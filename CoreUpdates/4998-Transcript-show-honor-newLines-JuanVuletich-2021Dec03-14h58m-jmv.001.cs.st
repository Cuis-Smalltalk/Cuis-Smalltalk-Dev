'From Cuis 5.0 [latest update: #4997] on 3 December 2021 at 3:00:53 pm'!

!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'jmv 12/3/2021 15:00:28'!
show: anObject
	"Old TranscriptStream protocol."
	(anObject asString findTokens: Character newLineCharacter)
		do: [ :line | self nextPutAll: line; newLine ]! !

