'From Cuis 6.0 [latest update: #5722] on 26 March 2023 at 1:11:52 pm'!

!WorldMorph methodsFor: 'defaul desktop' stamp: 'jmv 3/26/2023 13:11:31'!
recreateDefaultDesktop
	"
	Display extent
	For 1920@1076
	self runningWorld recreateDefaultDesktop
	"
	| editor |
	self whenUIinSafeState: [
		self hideTaskbar.
		(submorphs
			select: [ :ea | ea class == SystemWindow or: [ea class == TranscriptWindow]])
				do: [ :ea | ea delete ].
		TranscriptWindow openTranscript
			morphPosition: 4 @ 440;
			morphExtent: 1200 @ 556.
		editor := TextEditor openTextEditor
			morphPosition: 516 @ 4;
			morphExtent: 1400 @ 992.
		editor setLabel: 'About Cuis'.
		editor model actualContents: Utilities defaultTextEditorContents.
		Theme current useTaskbar ifTrue: [self showTaskbar].
	].! !

