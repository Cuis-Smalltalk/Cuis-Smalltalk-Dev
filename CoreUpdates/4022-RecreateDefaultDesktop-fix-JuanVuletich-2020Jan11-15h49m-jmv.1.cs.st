'From Cuis 5.0 [latest update: #4021] on 11 January 2020 at 3:49:54 pm'!

!PasteUpMorph methodsFor: 'defaul desktop' stamp: 'jmv 1/11/2020 15:47:27'!
recreateDefaultDesktop
	| editor |
	self whenUIinSafeState: [
		self hideTaskbar.
		(submorphs
			select: [ :ea | ea class == SystemWindow or: [ea class == TranscriptWindow]])
				do: [ :ea | ea delete ].
		TranscriptWindow openTranscript
			morphPosition: 5 @ 290;
			morphExtent: 990 @ 400.
		editor _ TextEditor openTextEditor
			morphPosition: 456 @ 10;
			morphExtent: 900 @ 680.
		editor setLabel: 'About Cuis'.
		editor model actualContents: Utilities defaultTextEditorContents.
		self showTaskbar.
	].! !

