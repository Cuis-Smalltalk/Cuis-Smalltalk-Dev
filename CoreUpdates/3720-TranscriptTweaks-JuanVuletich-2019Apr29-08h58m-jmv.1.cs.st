'From Cuis 5.0 of 7 November 2016 [latest update: #3719] on 29 April 2019 at 8:59:46 am'!

!Transcript class methodsFor: 'private' stamp: 'jmv 4/29/2019 08:58:28'!
addEntry: aString log: otherString
	"Add a new entrie to the entries circular list. If full, a new entry will replace the oldest one."

	accessSemaphore critical: [
		
		"Internal circular collection"
		lastIndex _ lastIndex \\ self maxEntries + 1.
		firstIndex = lastIndex ifTrue: [ firstIndex _ firstIndex \\ self maxEntries + 1 ].
		entries at: lastIndex put: aString.
		
		otherString ifNotNil: [
			self writeToFile: otherString.
			self writeToStdout: otherString
		]
	]! !


!Transcript class methodsFor: 'private' stamp: 'jmv 4/29/2019 08:58:40'!
addEntry: aString
	"Add a new entrie to the entries circular list. If full, a new entry will replace the oldest one."
	| msg now |
	logToFile | logToStdout ifTrue: [
		now _ DateAndTime now.
		msg _ String streamContents: [ :strm |
			now printWithMsOn: strm.
			strm
				nextPutAll: ' process:';
				nextPutAll: Processor activeProcess priority printString;
				nextPut: $ ;
				nextPutAll: Processor activeProcess hash printString;
				nextPut: $ ;
				nextPutAll: aString;
				newLine ]].

	self addEntry: (aString copyReplaceAll: String newLineString with: ' ') log: msg! !

!methodRemoval: Transcript class #addEntry:logToFile:!
Transcript class removeSelector: #addEntry:logToFile:!
!methodRemoval: Transcript class #addEntry:logToFileAndStdout:!
Transcript class removeSelector: #addEntry:logToFileAndStdout:!
