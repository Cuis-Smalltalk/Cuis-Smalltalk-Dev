'From Cuis 6.0 [latest update: #5366] on 7 July 2022 at 2:32:07 pm'!

!UniFileStream class methodsFor: 'error handling' stamp: 'jmv 7/7/2022 14:31:18'!
fileDoesNotExistUserHandling: fullFileName

	| selection newName |
	selection _ (PopUpMenu labels:
'create a new file
choose another name
cancel')
			startUpWithCaption: fullFileName asFileEntry name, '
does not exist.'.
	selection = 1 ifTrue:
		[^ self new open: fullFileName forWrite: true].
	selection = 2 ifTrue:
		[ newName _ self request: 'Enter a new file name'
						initialAnswer:  fullFileName.
		^ FileIOAccessor default privateWriteableFile: newName asFileEntry ].
	self halt! !

