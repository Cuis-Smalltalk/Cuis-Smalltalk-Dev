'From Haver 6.0 [latest update: #5734] on 31 March 2023 at 3:24:02 pm'!

!TextModel methodsFor: 'commands' stamp: 'KLG 3/31/2023 15:23:20'!
acceptContentsFrom: aTextModelMorph
	"Tell the owning window to save the contents if it supports that action.
	
	Also clear the user edits."
	
	aTextModelMorph owningWindow ifNotNil: [ :ow |
		ow hasSaveAs ifTrue: [ 
			ow forceSaveContents.
			self changed: #clearUserEdits ] ]! !

