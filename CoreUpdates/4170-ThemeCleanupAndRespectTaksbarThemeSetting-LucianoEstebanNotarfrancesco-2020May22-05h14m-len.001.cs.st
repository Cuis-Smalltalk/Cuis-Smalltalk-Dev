'From Cuis 5.0 [latest update: #4163] on 22 May 2020 at 7:13:13 am'!

!PasteUpMorph methodsFor: 'defaul desktop' stamp: 'len 5/22/2020 05:21:04'!
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
		Theme current useTaskbar ifTrue: [self showTaskbar].
	].! !


!SystemWindow methodsFor: 'initialization' stamp: 'len 5/22/2020 05:29:29'!
openInWorld
	"Ensure all widgets have proper colors before opening"
	self widgetsColor: self windowColor.
	super openInWorld! !

!SystemWindow methodsFor: 'open/close' stamp: 'len 5/22/2020 05:30:12'!
delete
	| thisWorld |
	self okToChange ifFalse: [^self].
	thisWorld _ self world.
	SystemWindow noteTopWindowIn: thisWorld but: self.
	self sendToBack.
	self removeHalo.
	super delete.
	self model: nil! !

!methodRemoval: Theme #windowClosed: stamp: 'len 5/22/2020 07:02:37'!
Theme removeSelector: #windowClosed:!
!methodRemoval: Theme #windowOpen: stamp: 'len 5/22/2020 07:02:20'!
Theme removeSelector: #windowOpen:!
