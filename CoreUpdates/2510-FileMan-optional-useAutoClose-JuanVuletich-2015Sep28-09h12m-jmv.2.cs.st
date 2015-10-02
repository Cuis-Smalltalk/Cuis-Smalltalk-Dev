'From Cuis 4.2 of 25 July 2013 [latest update: #2509] on 28 September 2015 at 12:00:09.604199 pm'!

!FileList methodsFor: 'file list' stamp: 'jmv 9/28/2015 09:34'!
selectedFileEntry
"COMENTAR
reemplaza a #readOnlyStream
"
	"Answer a read-only stream on the selected file. For the various stream-reading services."
"Ojo. Esto es un agujero. Quien lo cierra?????"
"creo que es mejor que no exista. Entregar un FmFileEntry y que el otro use el protocolo correcto..."
	^self directory ifNotNil: [ :dir | (dir fullNameFor: fileName) asFileEntry ]! !


!ChangeList class methodsFor: 'public access' stamp: 'jmv 9/28/2015 09:36'!
browsePackageContents: aFileEntry
	"Opens a changeList on a fileStream"
	| changeList packageFile |
	aFileEntry readStream: [ :stream |
		changeList _ self new scanFile: stream from: 0 to: stream size.
		stream reset.
		packageFile _ CodePackageFile buildFileStream: stream.
	].
	"Add deletions of methods and classes that are in the CodePackage (i.e., active in the system)
	but are no longer in the CodePackageFile being viewed."
	packageFile methodsToRemove do: [ :methodReference |
		changeList
			addItem: (MethodDeletionChangeRecord new methodReference: methodReference)
			text: 'method no longer in package: ', methodReference stringVersion ].
	packageFile classesToRemove do: [ :clsName |
		changeList
			addItem: (ClassDeletionChangeRecord new clsName: clsName)
			text: 'class no longer in package: ', clsName ].
	changeList clearSelections.
	ChangeListWindow open: changeList label: aFileEntry name! !

!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 9/28/2015 09:34'!
servicePackageContents
	"Answer a service for opening a changelist browser on a file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'package file contents'
		selector: #browsePackageContents:
		description: 'open a code file contents tool on this package file'
		buttonLabel: 'pck contents')
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !


!FileList methodsFor: 'file list' stamp: 'jmv 9/28/2015 09:38'!
readOnlyStream
	"Answer a read-only stream on the selected file. For the various stream-reading services."
"Ojo. Esto es un agujero. Quien lo cierra?????"
"creo que es mejor que no exista. Entregar un FmFileEntry y que el otro use el protocolo correcto...
#readOnlyFileNamed: y similaria ademas estan mal.

Ah, y habria que ver en los paquetes...
"
	^self directory ifNotNil: [ :dir | dir readOnlyFileNamed: fileName ]! !


!ChangeSet class methodsFor: 'services' stamp: 'jmv 9/28/2015 09:13'!
fileIn: anFmFileEntry
	"File in the entire contents of the file specified by the name provided"

	anFmFileEntry ifNil: [^ Smalltalk beep ].
	"Argument could also be a String"
	anFmFileEntry asFileEntry readStream: [ :stream |
		stream fileIn ]! !


!CodePackageFile class methodsFor: 'instance creation' stamp: 'jmv 9/28/2015 09:31'!
buildFileStream: aFileStream packageName: pkName fullName: fullFileName
	| instance |
	instance _ self new.
	instance buildFileStream: aFileStream packageName: pkName fullName: fullFileName.
	^instance! !

!CodePackageFile class methodsFor: 'instance creation' stamp: 'jmv 9/28/2015 09:19'!
installFileStream: aFileStream packageName: pkName fullName: fullFileName
	| instance |
	instance _ self new.
	instance installFileStream: aFileStream packageName: pkName fullName: fullFileName.
	^instance! !


!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 9/28/2015 09:17'!
install
	"Preconditions have been satisfied.  Install the required package."
	pathName asFileEntry readStream: [ :stream |
		CodePackageFile basicInstallPackageStream: stream ].

	"No need to have a platform and machine specific path around anymore. It was just for installation. Clear it."
	pathName _ nil! !


!FmFileEntry methodsFor: 'actions-file' stamp: 'jmv 9/28/2015 11:52'!
binaryContents
	| answer |
	self readStream: [ :stream |
		answer _ stream binary contents ].
	^ answer! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'jmv 9/28/2015 09:45'!
binaryContents: aByteArray
	self writeStream: [ :stream |
		self setContentsOf: stream binary to: aByteArray ]! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'jmv 9/28/2015 11:52'!
textContents
	| answer |
	self readStream: [ :stream |
		answer _ stream contents ].
	^ answer! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'jmv 9/28/2015 09:45'!
textContents: aString
	self writeStream: [ :stream |
		self setContentsOf: stream to: aString ]! !

!FmFileEntry methodsFor: 'private' stamp: 'jmv 9/28/2015 11:54'!
setContentsOf: aStream to: aStringOrBytes

	aStringOrBytes isString
		ifFalse: [ aStream binary].
	aStream nextPutAll: aStringOrBytes.
	self refresh! !


!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'jmv 9/28/2015 11:57'!
at: localFileName

	^(self / localFileName) textContents! !

!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'jmv 9/28/2015 11:57'!
at: localFileName ifAbsent: block

	^ [self at: localFileName]
		on: FileDoesNotExistException
		do: [:ex | block value]! !

!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'jmv 9/28/2015 11:55'!
at: localFileName put: contents

	(self / localFileName) forceWriteStream: [ :stream |
		self setContentsOf: stream to: contents ].
	self refreshChildren.
	^contents! !

!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'jmv 9/28/2015 11:39'!
binaryAt: localFileName 

	^ (self / localFileName) binaryContents! !

!FmDirectoryEntry methodsFor: 'dictionary-like' stamp: 'jmv 9/28/2015 11:58'!
binaryAt: localFileName ifAbsent: block

	^ [self binaryAt: localFileName]
		on: FileDoesNotExistException
		do: [:ex | block value]! !


!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 9/28/2015 09:27'!
fileInAnnouncing: announcement
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimitors. The expressions are read and passed to the 
	Compiler. Answer the result of compilation.  Put up a progress report with
     the given announcement as the title."

	| val chunk |
	announcement 
		displayProgressAt: Sensor mousePoint
		from: 0
		to: self size
		during: [ :bar | 
			[ self atEnd ] whileFalse: [
					bar value: self position.
					self skipSeparators.
					
					[
						val := (self peekFor: $!!) 
								ifTrue: [
									chunk := self nextChunk.
									"These are the ones that should do nothing, 
									because next line is a doit that does the stuff
									(or because it is handled elsewhere)"
									(chunk beginsWith: 'description:  ')
									| (chunk beginsWith: 'provides: ')
									| (chunk beginsWith: 'requires: ')
									| (chunk beginsWith: 'classDefinition: ')
									| (chunk beginsWith: 'classRemoval: ')
									| (chunk beginsWith: 'methodRemoval: ')
									| (chunk beginsWith: 'classMoveToSomePackage: ')
									| (chunk beginsWith: 'methodMoveToSomePackage: ')
										ifFalse: [(Compiler evaluate: chunk logged: false) scanFrom: self]]
								ifFalse: [
									chunk := self nextChunk.
									self checkForPreamble: chunk.
									[ Compiler evaluate: chunk logged: true ]
										on: Error
										do: [ :ex |
											ex print.
											('while evaluating: ', chunk) print.
											ex resume: true ]
										]] 
							on: InMidstOfFileinNotification
							do: [ :ex | ex resume: true] ].
			].
	"Note:  The main purpose of this banner is to flush the changes file."
	Smalltalk logChange: '----End fileIn of ' , self name , '----'.
	^val! !

!methodRemoval: FmFileIOAccessor #encode:on:!
FmFileIOAccessor removeSelector: #encode:on:!
!methodRemoval: FmDirectoryEntry #at:by:!
FmDirectoryEntry removeSelector: #at:by:!
!methodRemoval: FmDirectoryEntry #at:put:by:!
FmDirectoryEntry removeSelector: #at:put:by:!
!methodRemoval: FmFileEntry #contentsOf:!
FmFileEntry removeSelector: #contentsOf:!
!methodRemoval: FmFileEntry #contentsOf:by:!
FmFileEntry removeSelector: #contentsOf:by:!
!methodRemoval: FmFileEntry #setContentsOf:to:by:!
FmFileEntry removeSelector: #setContentsOf:to:by:!
!methodRemoval: FileList #fileEntry!
FileList removeSelector: #fileEntry!
