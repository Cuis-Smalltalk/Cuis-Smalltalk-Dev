'From Cuis7.5 [latest update: #7641] on 27 November 2025 at 1:00:01 pm'!

!DirectoryEntry class methodsFor: 'user default directories' stamp: 'LR 11/27/2025 12:45:59'!
userDirectory: userDirectoryDefaultName queryFileName: suggestedFileName writeStreamDo: writeBlock
	"Query the user for a fully qualified file path name. 
	If the file already exists, ask for confirmation to overwrite.
	If the user modifies the suggested directory, keep it as a saved preference for next time.
	If user accepts, evaluate writeBlock on the file write stream.
	See #userDirectory:fileName:writeStreamDo:ifExists:
	"
	| initialDirectoryChoice file |
	initialDirectoryChoice := self userDirectory: userDirectoryDefaultName.
	file := initialDirectoryChoice // suggestedFileName.
	self
		request: 'Confirm or enter path and file name'
		initialAnswer: file pathName
		verifying: [ :userInput |
			(FileEntry withPathName: userInput) exists
				ifTrue: [ 
					(PopUpMenu confirm: 'File already exists. Do you want to overwrite it?' )
						ifTrue: [ true ]
						ifFalse: [ 'Please pick another file name' ] ]
				ifFalse: [ true "Validation passed" ]]
		do: [ :userInput |
			file := FileEntry withPathName: userInput.
			file parent = initialDirectoryChoice ifFalse: [
				self userDirectory: userDirectoryDefaultName put: file parent pathName ].
			file forceWriteStreamDo: [ :fileStream |
				fileStream ifNotNil: [
					writeBlock value: fileStream ]]	].! !

