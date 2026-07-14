'From Cuis7.9 [latest update: #8062] on 14 July 2026 at 11:37:49 am'!

!UniFileStream class methodsFor: 'error handling' stamp: 'jmv 14/Jul/2026, 2:34:22 pm (UTC)'!
fileDoesNotExistUserHandling: fullFileName

	| selection |
	selection := (PopUpMenu labels:
'create a new file
choose another name
cancel')
			startUpWithCaption: fullFileName asFullFileEntry name, '
does not exist.'.

	selection = 1 ifTrue: [
		^ self new open: fullFileName forWrite: true].

	selection = 2 ifTrue: [
		(self request: 'Enter a new file name' initialAnswer: fullFileName orCancel: nil)
			ifNotNil: [ :newName |
				^ FileIOAccessor default privateWriteableFile: newName asFileEntry ]].

	"choice = 3: cancel in the menu, or cancel in the new file name request."
	^ self error: 'Could not open a file'! !

!UniFileStream class methodsFor: 'error handling' stamp: 'jmv 14/Jul/2026, 2:33:57 pm (UTC)'!
fileExistsUserHandling: fullFileName
	| dir localName choice entry |
	entry := fullFileName asFullFileEntry.
	dir := entry parent.
	localName := entry name.
	choice := (PopUpMenu
		labels:
'overwrite that file\choose another name\cancel' withNewLines)
		startUpWithCaption: localName, '
already exists.'.

	"overwrite that file"
	choice = 1 ifTrue: [
		dir removeKey: localName
			ifAbsent: [self error: 'Could not delete the old version of that file'].
		^ self new open: fullFileName forWrite: true].

	"choose another name"
	choice = 2 ifTrue: [
		(self request: 'Enter a new file name' initialAnswer: fullFileName orCancel: nil)
			ifNotNil: [ :newName | 
				^FileIOAccessor default privateNewFile: newName asFileEntry ]].

	"choice = 3: cancel in the menu, or cancel in the new file name request."
	"Answer nil. Let callers decide whether to continue or not"
	^nil.! !

!UniFileStream class methodsFor: 'error handling' stamp: 'jmv 14/Jul/2026, 2:37:00 pm (UTC)'!
readOnlyFileDoesNotExistUserHandling: fullFileName

	| dir files choices selection newName fileName |
	dir := fullFileName asFullFileEntry parent.
	files := dir fileNames.
	fileName := fullFileName asFullFileEntry name.
	choices := fileName correctAgainst: files.
	choices add: 'Choose another name'.
	choices add: 'Cancel'.
	selection := (PopUpMenu labelArray: choices lines: {5} )
		startUpWithCaption: fullFileName asFullFileEntry name, '
does not exist.'.

	selection = 0 ifTrue: ["esc, click outside" ^ nil ].

	selection = choices size ifTrue: ["cancel" ^ nil ].

	selection < (choices size - 1) ifTrue: [
		newName := (dir pathName , '/', (choices at: selection))].

	selection = (choices size - 1) ifTrue: [
		newName := self request: 'Enter a new file name' initialAnswer: fileName orCancel: [ ^nil ]].

	newName = '' ifFalse: [^ FileIOAccessor default privateReadOnlyFile: newName asFileEntry ].
	^ self error: 'Could not open a file'! !


!FileList methodsFor: 'private' stamp: 'jmv 14/Jul/2026, 2:28:49 pm (UTC)'!
put: aText
	"Private - put the supplied text onto the file"

	| type |
	"Ask for file name if needed."
	fileName ifNil: [
		fileName := self request: 'Enter a new file name'
						initialAnswer:  'NewFile.txt'
						orCancel: nil.
		fileName ifNil: [ ^false ].
		brevityState := #fullFile ].

	brevityState == #fullFile ifTrue: [ | deselect reselect |
		acceptedContentsCache := aText.
		directory // fileName writeStreamDo: [ :fileStream | | actuallySavedFile |
			fileStream ifNil: [ ^false ].
			fileStream nextPutAll: acceptedContentsCache asPlainString.
			"Saved file name or location may have been changed. See #fileExistsUserHandling:"
			deselect := false. reselect := nil.
			actuallySavedFile := fileStream fileEntry.
			actuallySavedFile parent = directory
				ifFalse: [deselect := true.]
				ifTrue: [
					fileName = actuallySavedFile name ifFalse: [
						reselect := actuallySavedFile name]]].
		directory invalidateChildren.
		self updateFileList.
		deselect ifTrue: [self fileListIndex: 0].
		reselect ifNotNil: [
			self fileListIndex: (list findFirst: [:item | item name = reselect])].
		^ true  "accepted"].

	"Can't save. inform user."
	type := 'These'.
	brevityState = #briefFile ifTrue: [type := 'Abbreviated'].
	brevityState = #briefHex ifTrue: [type := 'Abbreviated'].
	brevityState = #fullHex ifTrue: [type := 'Hexadecimal'].
	brevityState = #FileList ifTrue: [type := 'Directory'].
	self inform: ('{1} contents cannot
meaningfully be saved at present.' format:{type}).
	^ false  "failed"! !

