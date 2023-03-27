'From Cuis 6.0 [latest update: #5695] on 16 March 2023 at 12:34:54 pm'!

!SystemWindow methodsFor: 'user interface' stamp: 'KLG 3/16/2023 12:24:32'!
forceSaveContents
	"Saves the contents to the file name in the #lastFilenameUsedToSave property.
	
	If none exists ask the user for a filename."
	
	self hasSaveAs ifFalse: [ ^ self ].
	self valueOfProperty: #lastFilenameUsedToSave
		ifPresentDo: [ :fileName | self forceSaveContentsTo: fileName ]
		ifAbsent: [
			self
				request: 'Enter file name'
				initialAnswer: ''
				verifying: [ :aString | aString notEmpty ]
				do: [ :fileName | self saveContentsTo: fileName ] ]! !

!SystemWindow methodsFor: 'user interface' stamp: 'KLG 3/16/2023 12:32:50'!
forceSaveContentsTo: fileName
	"Saves the contents to the given filename.
	
	Force overwrting the file if it exists."

	self hasSaveAs ifFalse: [^self ].
	fileName asFileEntry forceWriteStreamDo: [ :stream |
		model saveOn: stream ].
	self 
		setProperty: #lastFilenameUsedToSave toValue: fileName! !


!TextModel methodsFor: 'commands' stamp: 'KLG 3/16/2023 12:18:55'!
acceptContentsFrom: aTextModelMorph
	"Tell the owning window to save the contents if it supports that action."
	
	aTextModelMorph owningWindow ifNotNil: [ :ow |
		ow hasSaveAs ifTrue: [ ow forceSaveContents ] ]! !

