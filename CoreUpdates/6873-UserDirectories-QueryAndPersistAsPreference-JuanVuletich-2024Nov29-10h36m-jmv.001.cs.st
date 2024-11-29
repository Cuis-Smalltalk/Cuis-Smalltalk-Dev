'From Cuis7.1 [latest update: #6872] on 29 November 2024 at 10:37:43 am'!

!DirectoryEntry class methodsFor: 'user default directories' stamp: 'jmv 11/28/2024 17:06:16'!
userDirectory: userDirectoryDefaultName
	| preferenceKey |
	preferenceKey := self userDirectoryKeyFor: userDirectoryDefaultName.
	^(Preferences includesKey: preferenceKey)
		ifTrue: [ DirectoryEntry withPathName: (Preferences at: preferenceKey) ]
		ifFalse: [ DirectoryEntry userBaseDirectory / userDirectoryDefaultName ].! !

!DirectoryEntry class methodsFor: 'user default directories' stamp: 'jmv 11/29/2024 10:28:02'!
userDirectory: userDirectoryDefaultName fileName: aFileName writeStreamDo: writeBlock ifExists: appendOverwriteSymbol
	"Don't query the user at all. Use the saved preference for the folder, if it exists.
	If file doesn't exist, just create it.
	If it exists, act on appendOverwriteSymbol
		#append: append new contents at the end
		#overwrite: just delete any previous contents of the file.
	See #userDirectory:queryFileName:writeStreamDo:
	"
	| file |
	file := (self userDirectory: userDirectoryDefaultName) // aFileName.
	file writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			writeBlock value: fileStream ]]	.
	
	appendOverwriteSymbol
		caseOf: {
			[#append] 		-> 	[ file appendStreamDo: [ :fileStream | writeBlock value: fileStream ]].
			[#overwrite] 	-> 	[ file forceWriteStreamDo: [ :fileStream | writeBlock value: fileStream ]] }
		otherwise: 				[ file writeStreamDo: [ :fileStream | fileStream ifNotNil: [ writeBlock value: fileStream ]]].! !

!DirectoryEntry class methodsFor: 'user default directories' stamp: 'jmv 11/28/2024 17:06:25'!
userDirectory: userDirectoryDefaultName put: pathName
	| preferenceKey |
	preferenceKey := self userDirectoryKeyFor: userDirectoryDefaultName.
	Preferences name: preferenceKey description: 'User Directory ', userDirectoryDefaultName
		category: #directories type: CharacterSequence value: pathName.
	Preferences saveToDisk: preferenceKey.! !

!DirectoryEntry class methodsFor: 'user default directories' stamp: 'jmv 11/29/2024 10:28:16'!
userDirectory: userDirectoryDefaultName queryFileName: suggestedFileName writeStreamDo: writeBlock
	"Query the user for a fully qualified file path name. File must not exist.
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
				ifTrue: [ 'File already exists. Please pick another file name.' ]
				ifFalse: [ true "Validation passed" ]]
		do: [ :userInput |
			file := FileEntry withPathName: userInput.
			file parent = initialDirectoryChoice ifFalse: [
				self userDirectory: userDirectoryDefaultName put: file parent pathName ].
			file writeStreamDo: [ :fileStream |
				fileStream ifNotNil: [
					writeBlock value: fileStream ]]	].! !

!DirectoryEntry class methodsFor: 'user default directories' stamp: 'jmv 11/28/2024 17:05:56'!
userDirectoryKeyFor: userDirectoryDefaultName

	^('user', userDirectoryDefaultName, 'Directory') asSymbol.! !

