'From Cuis6.3 [latest update: #6217] on 7 February 2024 at 3:53:32 pm'!

!TaskbarMorph methodsFor: 'geometry' stamp: 'jmv 2/7/2024 15:41:51'!
basicDisplayBounds

	self visible ifFalse: [ ^nil ].
	^self morphPosition extent: extent! !


!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 2/7/2024 15:45:52'!
aboutThisSystem 
	"Identify software version"

	Utilities defaultTextEditorContents edit.
	^ self inform: self systemInformationString! !


!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 2/7/2024 15:50:51'!
clear

	accessSemaphore critical: [
		"Having at least one entry simplifies handling of the entries circular collection"
		firstIndex := 1.
		lastIndex := 0.
		entries atAllPut: nil.
		Smalltalk systemInformationString linesDo: [ :l |
			lastIndex := lastIndex + 1.
			entries at: lastIndex put: l ].
		unfinishedEntry reset.
		lastDisplayPosition := 0.
		
		logToFile ifTrue: [
			DirectoryEntry logsDirectory // self filename forceWriteStreamDo: [ :stream |
				stream nextPutAll: 'Transcript log started: '.
				DateAndTime now printOn: stream.
				stream
					newLine;
					nextPutAll: '------------------------------------------------------------------------';
					newLine ]]].
	self display! !


!WorldMorph methodsFor: 'defaul desktop' stamp: 'jmv 2/7/2024 15:43:18'!
recreateDefaultDesktop
	"
	self runningWorld recreateDefaultDesktop
	"
	self whenUIinSafeState: [
		self hideTaskbar.
		(submorphs
			select: [ :ea | ea class == SystemWindow or: [ea class == TranscriptWindow]])
				do: [ :ea | ea delete ].
		Theme current useTaskbar ifTrue: [self showTaskbar].
		Transcript clear.
		TranscriptWindow openTranscript
			resizeBottomLeft
	].! !

