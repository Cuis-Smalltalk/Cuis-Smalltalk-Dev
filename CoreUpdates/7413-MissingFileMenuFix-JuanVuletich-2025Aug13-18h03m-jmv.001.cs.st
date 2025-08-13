'From Cuis7.5 [latest update: #7412] on 13 August 2025 at 6:03:46 pm'!

!UniFileStream class methodsFor: 'error handling' stamp: 'jmv 8/13/2025 16:40:08'!
readOnlyFileDoesNotExistUserHandling: fullFileName

	| dir files choices selection newName fileName |
	dir := fullFileName asFullFileEntry parent.
	files := dir fileNames.
	fileName := fullFileName asFullFileEntry name.
	choices := fileName correctAgainst: files.
	choices add: 'Choose another name'.
	choices add: 'Cancel'.
	selection := (PopUpMenu labelArray: choices lines: (Array with: 5) )
		startUpWithCaption: fullFileName asFullFileEntry name, '
does not exist.'.
	selection = 0 ifTrue: ["esc, click outside" ^ nil ].
	selection = choices size ifTrue: ["cancel" ^ nil ].
	selection < (choices size - 1) ifTrue: [
		newName := (dir pathName , '/', (choices at: selection))].
	selection = (choices size - 1) ifTrue: [
		newName := self request: 'Enter a new file name' initialAnswer: fileName.
		"If Cancel was pressed, no file should be opened - Hernan"
		newName isEmpty ifTrue: [ ^nil ]].
	newName = '' ifFalse: [^ FileIOAccessor default privateReadOnlyFile: newName asFileEntry ].
	^ self error: 'Could not open a file'! !

