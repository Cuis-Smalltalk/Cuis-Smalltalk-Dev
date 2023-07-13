'From Cuis 6.0 [latest update: #5907] on 12 July 2023 at 5:32:02 pm'!

!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 7/12/2023 17:30:24'!
clear

	accessSemaphore critical: [
		"Having at least one entry simplifies handling of the entries circular collection"
		firstIndex := 1.
		lastIndex := 1.
		entries atAllPut: nil.
		entries at: 1 put: 'Transcript'.
		unfinishedEntry reset.
		lastDisplayPosition := 0.
		
		logToFile ifTrue: [
			self filename asFileEntry forceWriteStreamDo: [ :stream |
				stream nextPutAll: 'Transcript log started: '.
				DateAndTime now printOn: stream.
				stream
					newLine;
					nextPutAll: '------------------------------------------------------------------------';
					newLine ]]].
	self display! !

!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 7/12/2023 17:30:20'!
clearInternal

	accessSemaphore critical: [
		"Having at least one entry simplifies handling of the entries circular collection"
		firstIndex := 1.
		lastIndex := 1.
		entries atAllPut: nil.
		entries at: 1 put: 'Transcript'.
		unfinishedEntry reset.
		lastDisplayPosition := 0 ].
	self display! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Transcript clear!

