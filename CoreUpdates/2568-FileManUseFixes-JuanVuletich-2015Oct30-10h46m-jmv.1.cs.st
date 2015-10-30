'From Cuis 4.2 of 25 July 2013 [latest update: #2566] on 30 October 2015 at 10:49:59.369668 am'!

!FmFileIOAccessor methodsFor: 'file stream creation' stamp: 'jmv 10/30/2015 10:49'!
privateFile: aFileEntry
	"Open the file with the given name in this directory for writing."

	^ self concreteStreamClass new open: aFileEntry pathName forWrite: true! !


!FmFileEntry methodsFor: 'private' stamp: 'jmv 10/30/2015 10:49'!
privateAppendStream
	"Note: You need to eventually close the stream.
	Usually prefer #appendStream: that closes the file for you."

	self exists ifFalse: [
		^ self privateWriteStream ].
	^ (self fileAccessor privateFile: self) setToEnd! !


!StandardFileStream class methodsFor: 'error handling' stamp: 'jmv 10/30/2015 10:49'!
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
		[ newName _ FillInTheBlankMorph request: 'Enter a new file name'
						initialAnswer:  fullFileName.
		^ FmFileIOAccessor default privateFile: newName asFileEntry ].
	self halt! !

!StandardFileStream class methodsFor: 'error handling' stamp: 'jmv 10/30/2015 10:48'!
fileExistsUserHandling: fullFileName
	| dir localName choice newName entry |
	entry _ fullFileName asFileEntry.
	dir _ entry parent.
	localName _ entry name.
	choice _ (PopUpMenu
		labels:
'overwrite that file\choose another name\cancel' withNewLines)
		startUpWithCaption: localName, '
already exists.'.

	choice = 1 ifTrue: [
		dir removeKey: localName
			ifAbsent: [self error: 'Could not delete the old version of that file'].
		^ self new open: fullFileName forWrite: true].

	choice = 2 ifTrue: [
		newName _ FillInTheBlankMorph request: 'Enter a new file name' initialAnswer: fullFileName.
		^ FmFileIOAccessor default privateNewFile: newName asFileEntry ].

	self error: 'Please close this to abort file opening'! !

!StandardFileStream class methodsFor: 'error handling' stamp: 'jmv 10/30/2015 10:49'!
readOnlyFileDoesNotExistUserHandling: fullFileName

	| dir files choices selection newName fileName |
	dir _ fullFileName asFileEntry parent.
	files _ dir fileNames.
	fileName _ fullFileName asFileEntry name.
	choices _ fileName correctAgainst: files.
	choices add: 'Choose another name'.
	choices add: 'Cancel'.
	selection _ (PopUpMenu labelArray: choices lines: (Array with: 5) )
		startUpWithCaption: fullFileName asFileEntry name, '
does not exist.'.
	selection = choices size ifTrue:["cancel" ^ nil "should we raise another exception here?"].
	selection < (choices size - 1) ifTrue: [
		newName _ (dir pathName , '/', (choices at: selection))].
	selection = (choices size - 1) ifTrue: [
		newName _ FillInTheBlankMorph 
							request: 'Enter a new file name' 
							initialAnswer: fileName].
	newName = '' ifFalse: [^ FmFileIOAccessor default privateReadOnlyFile: newName asFileEntry ].
	^ self error: 'Could not open a file'! !

!methodRemoval: FmFileIOAccessor #privateFileNamed:!
FmFileIOAccessor removeSelector: #privateFileNamed:!
