'From Cuis 4.2 of 25 July 2013 [latest update: #2528] on 14 October 2015 at 5:50:14.153202 pm'!

!CodeFile methodsFor: 'accessing' stamp: 'jmv 10/14/2015 17:36'!
name

	^ self fullName asFileEntry name! !


!CodePackageFile methodsFor: 'services' stamp: 'jmv 10/14/2015 17:36'!
install: aFileStream
	"Installs the package. Replace all existing code in the PackageInfo, removing any code that is not included in us."
	| localName newCodePackage |

	"Give reasonable warnings if there is stuff that can't be properly cleaned. Give the user the chance to abort."
	'=============' print.
	classesToRemove notEmpty ifTrue: [
		('classesToRemove: ', classesToRemove printString) print.
		'=============' print ].
	methodsToRemove notEmpty ifTrue: [
		'methodsToRemove: ' print.
		methodsToRemove do: [ :methodReference | methodReference print ].
		'=============' print ].
	
	"Tirar warning si hay que borrar cosas que no se puede, si hay referencias, etc. Quizas si vamos a borrar el ultimo implementor de un mensaje enviado?"

	"Create, install and answer an instance of CodePackage"
	newCodePackage _ CodePackage
		named: packageName
		createIfAbsent: true
		registerIfNew: true.

	"Esto crea el change set y carga ahi. OJO. En ese CS, hay que borrar todo lo que hay que borrar"
	"These were created in #fromFileStream: ... reuse?"
	localName _ fullName asFileEntry name.
	ChangeSet installing: newCodePackage packageName do: [
		newCodePackage
			fullFileName: fullName;
			sourceSystem: sourceSystem;
			description: description;
			featureSpec: featureSpec.
		aFileStream fileInAnnouncing: 'Installing ', localName, '...'.
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ]].
	newCodePackage hasUnsavedChanges: false; triggerEvent: #dirtyFlagChanged.
	DataStream initialize. "Just in case"
	"If we are installing an already installed package, zap the change set with possible changes done, 
	as they are irrelevant now: we have the package from disk"
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: newCodePackage).
	Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine.
	
	"Tirar undeclared al transcript. warning si quedaron undeclared
	Es feo que tire an transcript undeclareds que despues no lo son..."
	Smalltalk cleanOutUndeclared.
	Undeclared notEmpty ifTrue: [
		('Undeclared: ', Undeclared printString) print ].

	"Descartar la instancia de CodePackageFile"
	^newCodePackage! !


!CodePackageFile class methodsFor: 'services' stamp: 'jmv 10/14/2015 17:37'!
packageNameFrom: fullName

	^fullName asFileEntry name sansPeriodSuffix! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 10/14/2015 17:43'!
save

	"If we can't save, find a new destination directory."
	fullFileName ifNotNil: [
		fullFileName asDirectoryEntry exists ifFalse: [
			fullFileName _ nil ]].

	"If we were never saved, or never saved since image was moved, or target directory disappeared, then save to image directory."
	fullFileName ifNil: [
		fullFileName _
			(FmDirectoryEntry smalltalkImageDirectory / self packageFileName) pathName ].

	fullFileName asFileEntry forceWriteStream: [ :stream |
		stream timeStamp.
		self writeOnStream: stream ].

	self hasUnsavedChanges: false.
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: self)! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 10/14/2015 17:40'!
logSapshot: save andQuit: quit
	"Log quitting to changes file"
	| msg |
	(SourceFiles at: 2) ifNotNil: [
		msg _ String streamContents: [ :s |
			s
				nextPutAll: '----';
				nextPutAll:
				(save
					ifTrue: [
						quit
							ifTrue: [ 'QUIT' ]
							ifFalse: [ 'SNAPSHOT' ]]
					ifFalse: [
						quit
							ifTrue: [ 'QUIT/NOSAVE' ]
							ifFalse: [ 'NOP' ]]);
				nextPutAll: '----';
				print: Date dateAndTimeNow;
				space;
				nextPutAll: self imageName asFileEntry name;
				nextPutAll: ' priorSource: ';
				print: LastQuitLogPosition ].
		self assureStartupStampLogged.
		save ifTrue: [
			LastQuitLogPosition _ (SourceFiles at: 2)
				 setToEnd;
				 position ].
		self logChange: msg.
		Transcript
			 newLine;
			 show: msg;
			 newLine ]! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 10/14/2015 17:40'!
saveAsEmbeddedImage
	"Save the current state of the system as an embedded image"

	| dir newName newImageName |
	dir _ FileDirectory smalltalkImageDirectory.
	newName _ FillInTheBlankMorph
		request: 'Select existing VM file'
		initialAnswer: ''.
	newName = '' ifTrue: [^ self].
	newName _ FileDirectory baseNameFor: newName asFileName.

	newImageName _ newName.
	(dir includesKey: newImageName) ifFalse:
		[^ self inform: 'Unable to find name ', newName, ' Please choose another name.'].

	self logChange: '----SAVEAS (EMBEDDED) ', newName, '----', Date dateAndTimeNow printString.
	self imageName: (dir fullNameFor: newImageName).
	LastImageName _ self imageName.
	self closeSourceFiles.
	self snapshot: true andQuit: true embedded: true clearAllClassState: false! !

!SystemDictionary methodsFor: 'toDeprecate' stamp: 'jmv 10/14/2015 17:39'!
getFileNameFromUser

	| newName |
	newName _ FillInTheBlankMorph
		request: 'New File Name?'
		initialAnswer: self imageName asFileEntry name.
	newName isEmpty ifTrue: [ ^nil ].
	((FileDirectory smalltalkImageDirectory fileOrDirectoryExists: (self fullNameForImageNamed: newName)) or:
	 [FileDirectory smalltalkImageDirectory fileOrDirectoryExists: (self fullNameForChangesNamed: newName)]) ifTrue: [
		(self confirm: ('{1} already exists. Overwrite?' format: {newName})) ifFalse: [^nil]].
	^newName
! !


!StandardFileMenu methodsFor: 'basic control sequences' stamp: 'jmv 10/14/2015 17:38'!
getTypedFileName: aResult

	| name |
	name _ FillInTheBlankMorph 
		request: 'Enter a new file name' 
		initialAnswer: ''.
	name = '' ifTrue: [^self startUpWithCaption: 'Select a File:' ].
	name _ aResult directory fullNameFor: name.
	^ StandardFileMenuResult
			directory: (FileDirectory forFullFileName: name)
			name: name asFileEntry name
! !


!FileStream methodsFor: 'file accessing' stamp: 'jmv 10/14/2015 17:38'!
localName
"mhhhhh. Eliminar, mejor?"
	^ self name asFileEntry name! !


!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 10/14/2015 17:39'!
forceNewFileNamed: fileName 
	"Create a new file with the given name, and answer a stream opened 
	for writing on that file. If the file already exists, delete it without 
	asking before creating the new file."
	| dir localName fullName f |
	fullName _ FileDirectory currentDirectory fullNameFor: fileName.
	(self isAFileFullNamed: fullName)
		ifFalse: [
			f _ self new open: fullName forWrite: true.
			^ f
				ifNil: ["Failed to open the file"
					(FileDoesNotExistException fileName: fullName) signal]].
	dir _ FileDirectory forFullFileName: fullName.
	localName _ fullName asFileEntry name.
	dir
		deleteFileNamed: localName
		ifAbsent: [(CannotDeleteFileException new
			messageText: 'Could not delete the old version of file ' , fullName) signal].
	f _ self new open: fullName forWrite: true.
	^ f
		ifNil: ["Failed to open the file"
			(FileDoesNotExistException fileName: fullName) signal]! !

!StandardFileStream class methodsFor: 'error handling' stamp: 'jmv 10/14/2015 17:38'!
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
		^ self oldFileNamed: newName ].
	self halt! !

!StandardFileStream class methodsFor: 'error handling' stamp: 'jmv 10/14/2015 17:38'!
fileExistsUserHandling: fullFileName
	| dir localName choice newName |
	dir _ FileDirectory forFullFileName: fullFileName.
	localName _ fullFileName asFileEntry name.
	choice _ (PopUpMenu
		labels:
'overwrite that file\choose another name\cancel' withNewLines)
		startUpWithCaption: localName, '
already exists.'.

	choice = 1 ifTrue: [
		dir deleteFileNamed: localName
			ifAbsent: [self error: 'Could not delete the old version of that file'].
		^ self new open: fullFileName forWrite: true].

	choice = 2 ifTrue: [
		newName _ FillInTheBlankMorph request: 'Enter a new file name' initialAnswer: fullFileName.
		^ self newFileNamed: newName].

	self error: 'Please close this to abort file opening'! !

!StandardFileStream class methodsFor: 'error handling' stamp: 'jmv 10/14/2015 17:39'!
readOnlyFileDoesNotExistUserHandling: fullFileName

	| dir files choices selection newName fileName |
	dir _ FileDirectory forFullFileName: fullFileName.
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
		newName _ (dir pathName , FileDirectory slash , (choices at: selection))].
	selection = (choices size - 1) ifTrue: [
		newName _ FillInTheBlankMorph 
							request: 'Enter a new file name' 
							initialAnswer: fileName].
	newName = '' ifFalse: [^ self readOnlyFileNamed: newName].
	^ self error: 'Could not open a file'! !

