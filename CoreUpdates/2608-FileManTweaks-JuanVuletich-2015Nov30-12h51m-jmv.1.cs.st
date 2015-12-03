'From Cuis 4.2 of 25 July 2013 [latest update: #2605] on 30 November 2015 at 1:14:18.876151 pm'!

!FmFileIOAccessor methodsFor: 'file stream creation' stamp: 'jmv 11/30/2015 13:04'!
privateWriteableFile: aFileEntry
	"Open the file with the given name in this directory for writing."

	| pathName |
	pathName _ aFileEntry pathName.
	^ (self concreteStreamClass new open: pathName forWrite: true)
		ifNil: [
			"Failed to open the file"
			(FileWriteError fileName: pathName)
				signal: ('File [', pathName, '] open for write failed' ) ]! !


!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 11/30/2015 13:13'!
openSourcesAndChanges
	"Open the changes and sources files and install them in SourceFiles. Inform the user of problems regarding write permissions or Lf/CrLf mixups."
	"Note: SourcesName and imageName are full paths; changesName is a  
	local name."
	| sources changes msg wmsg entry |
	msg _ 'Cuis cannot locate XfileRef
Please check that the file is named properly and is in the
same directory as this image.'.
	wmsg _ 'Cuis cannot write to XfileRef.

Please check that you have write permission for this file.

You won''t be able to save this image correctly until you fix this.'.

	"Do not open source files if internalized (i.e. notNil)"
	sources _ SourceFiles at: 1.
	sources ifNil: [
		entry _ Smalltalk defaultSourcesName asFileEntry.
		entry exists ifFalse: [
			entry _ Smalltalk alternativeSourcesName asFileEntry ].
		entry exists ifTrue: [
			sources _ [ entry privateReadStream ] on: FileDoesNotExistException do: [ nil ]]].
	(sources isNil and: [ Preferences valueOfFlag: #warnIfNoSourcesFile ])
		ifTrue: [
			Smalltalk platformName = 'Mac OS' ifTrue: [
				msg _ msg , String newLineString, 'Make sure the sources file is not an Alias.'].
			self inform: (msg copyReplaceAll: 'XfileRef' with: 'the sources file named ' , entry pathName) ].

	"Do not open source files if internalized (i.e. notNil)"
	changes _ (SourceFiles at: 2) ifNil: [ 
		entry _ Smalltalk defaultChangesName asFileEntry.
		[ entry privateAppendStream ] on: FileWriteError do: [ nil ] ].
	(changes isNil and: [ Preferences valueOfFlag: #warnIfNoChangesFile ])
		ifTrue: [self inform: (wmsg copyReplaceAll: 'XfileRef' with: 'the changes file named ' , entry pathName)].

	SourceFiles _ Array with: sources with: changes! !


!FmFileEntry methodsFor: 'private' stamp: 'jmv 11/30/2015 12:59'!
privateAppendStream
	"Note: You need to eventually close the stream.
	Usually prefer #appendStream: that closes the file for you."

	self exists ifFalse: [
		^ self privateWriteStream ].
	^ (self fileAccessor privateWriteableFile: self) setToEnd! !


!FmFileIOAccessor methodsFor: 'file stream creation' stamp: 'jmv 11/30/2015 13:05'!
privateForceNewFile: aFileEntry
	"Open the file with the given name in this directory for writing.  If it already exists, delete it first without asking."

	| pathName |
	pathName _ aFileEntry pathName.
	aFileEntry exists
		ifTrue: [
			self deleteFile: pathName ifAbsent: [
				(CannotDeleteFileException new
					messageText: 'Could not delete the old version of file ' , pathName) signal]].

	^ self privateWriteableFile: aFileEntry! !

!FmFileIOAccessor methodsFor: 'file stream creation' stamp: 'jmv 11/30/2015 13:06'!
privateNewFile: aFileEntry
	"Create a new file with the given full pathName."

	^aFileEntry exists
		ifTrue: [
			(FileExistsException fileName: aFileEntry pathName fileClass: self concreteStreamClass) signal]
		ifFalse: [
			self privateWriteableFile: aFileEntry ]! !

!FmFileIOAccessor methodsFor: 'file stream creation' stamp: 'jmv 11/30/2015 13:06'!
privateReadOnlyFile: fileEntry
	"Open the existing file with the given name in this directory for read-only access."

	| pathName |
	pathName _ fileEntry pathName.
	^(self concreteStreamClass new open: pathName forWrite: false)
		ifNil: [
			"File does not exist..."
			((FileDoesNotExistException fileName: pathName) readOnly: true) signal ]! !


!StandardFileStream class methodsFor: 'error handling' stamp: 'jmv 11/30/2015 12:59'!
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
		^ FmFileIOAccessor default privateWriteableFile: newName asFileEntry ].
	self halt! !

!methodRemoval: FmFileIOAccessor #privateFile:!
FmFileIOAccessor removeSelector: #privateFile:!
