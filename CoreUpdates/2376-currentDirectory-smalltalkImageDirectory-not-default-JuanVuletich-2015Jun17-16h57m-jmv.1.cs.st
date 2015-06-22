'From Cuis 4.2 of 25 July 2013 [latest update: #2375] on 17 June 2015 at 5:20:26.634572 pm'!
!classDefinition: #FileDirectory category: #'System-Files'!
Object subclass: #FileDirectory
	instanceVariableNames: 'pathName '
	classVariableNames: 'DefaultDirectory DirectoryClass CurrentDirectory VMDirectory ImageDirectory '
	poolDictionaries: ''
	category: 'System-Files'!

!StandardFileMenu commentStamp: '<historical>' prior: 0!
I represent a SelectionMenu which operates like a modal dialog for selecting files, somewhat similar to the StandardFile dialogs in MacOS and Java Swing.

Try for example, the following:

	(StandardFileMenu oldFileFrom: FileDirectory smalltalkImageDirectory) inspect!


!FeatureRequirement methodsFor: 'private' stamp: 'jmv 6/17/2015 12:17'!
inPackagesSubtreeOf: aFileDirectory do: aBlock

	| repoDir pckDir compatPckDir |

	"Look in the requested directory"
	aBlock value: aFileDirectory.

	"Look in the usual Packages subfolders"
	pckDir _ aFileDirectory directoryNamed: 'Packages'.
	pckDir exists ifTrue: [
		aBlock value: pckDir ].
	compatPckDir _ aFileDirectory directoryNamed: 'CompatibilityPackages'.
	compatPckDir exists ifTrue: [
		aBlock value: compatPckDir ].

	"Finally look in folders that follow the convention of naming package repositories
	with the 'Cuis-Smalltalk' prefix, and their possible 'Packages' subdir."
	aFileDirectory entries do: [ :entry |
		(entry isDirectory and: [ entry name beginsWith: 'Cuis-Smalltalk' ]) ifTrue: [
			repoDir _ entry asFileDirectory.
			aBlock value: repoDir.
			pckDir _ repoDir directoryNamed: 'Packages'.
			pckDir exists ifTrue: [
				aBlock value: pckDir ].
			compatPckDir _ repoDir directoryNamed: 'CompatibilityPackages'.
			compatPckDir exists ifTrue: [
				aBlock value: compatPckDir ]]]! !


!FileDirectory class methodsFor: 'instance creation' stamp: 'jmv 6/17/2015 11:09'!
currentDirectory
	"Answer the current directory.

	In Unix it is the current directory in the OS shell that started us.
	In Windows the same happens if the image file is in a subree of the Windows current directory.

	But it defaults to the directory in wich this Smalltalk image was started (or last saved) if this fails
	(this usually happens, for example, if the image is dropped on the VM in a Windows explorer).
	See #getCurrentWorkingDirectory
	"

	CurrentDirectory ifNil: [
		CurrentDirectory _ self on: (Smalltalk getCurrentWorkingDirectory ifNil: [ Smalltalk imagePath ]) ].
	^ CurrentDirectory
! !

!FileDirectory class methodsFor: 'instance creation' stamp: 'jmv 6/17/2015 13:44'!
forFullFileName: fullFileName

	| path |
	path _ self dirPathFor: fullFileName.
	^ self on: path
! !

!FileDirectory class methodsFor: 'instance creation' stamp: 'jmv 6/17/2015 11:17'!
smalltalkImageDirectory
	"Answer the directory on which this Smalltalk image was started (or last saved)"

	ImageDirectory ifNil: [
		ImageDirectory _ self on: Smalltalk imagePath ].
	^ ImageDirectory
! !

!FileDirectory class methodsFor: 'instance creation' stamp: 'jmv 6/17/2015 11:06'!
vmDirectory
	"Answer the directory containing the VM that runs us."

	VMDirectory ifNil: [
		VMDirectory _ self on: Smalltalk vmPath ].
	^ VMDirectory
! !


!ChangeList methodsFor: 'menu actions' stamp: 'jmv 6/17/2015 14:07'!
fileOutSelections 
	| f |
	f _ FileDirectory smalltalkImageDirectory newFileNamed: (FillInTheBlankMorph request: 'Enter file name' initialAnswer: 'Filename.st').
	f ifNil: [^ self].
	f timeStamp.
	listSelections with: changeList do: 
		[:selected :item | selected ifTrue: [item fileOutOn: f]].
	f close! !


!AsyncFile methodsFor: 'as yet unclassified' stamp: 'jmv 6/17/2015 11:19'!
test: byteCount fileName: fileName
	"AsyncFile new test: 10000 fileName: 'testData'"

	| buf1 buf2 bytesWritten bytesRead |
	buf1 _ String new: byteCount withAll: $x.
	buf2 _ String new: byteCount.
	self open: ( FileDirectory smalltalkImageDirectory fullNameFor: fileName) forWrite: true.
	self primWriteStart: fileHandle
		fPosition: 0
		fromBuffer: buf1
		at: 1
		count: byteCount.
	semaphore wait.
	bytesWritten _ self primWriteResult: fileHandle.
	self close.
	
	self open: ( FileDirectory smalltalkImageDirectory fullNameFor: fileName) forWrite: false.
	self primReadStart: fileHandle fPosition: 0 count: byteCount.
	semaphore wait.
	bytesRead _
		self primReadResult: fileHandle
			intoBuffer: buf2
			at: 1
			count: byteCount.
	self close.

	buf1 = buf2 ifFalse: [self error: 'buffers do not match'].
	^ 'wrote ', bytesWritten printString, ' bytes; ',
	   'read ', bytesRead printString, ' bytes'
! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 6/17/2015 14:08'!
fileOutCategory: catName
	"FileOut the named category"
	| fileStream |
	fileStream _ FileDirectory smalltalkImageDirectory newFileNamed: self name , '-' , catName , '.st'.
	fileStream timeStamp.
	self fileOutCategory: catName on: fileStream moveSource: false toFile: 0.
	fileStream close! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 6/17/2015 14:08'!
fileOutMethod: selector
	"Write source code of a single method on a file.  Make up a name for the file."

	| fileStream nameBody |
	(selector == #Comment) ifTrue: [^ self inform: 'Sorry, cannot file out class comment in isolation.'].
	(self includesSelector: selector) ifFalse: [^ self error: 'Selector ', selector asString, ' not found'].
	nameBody _ self name , '-' , (selector copyReplaceAll: ':' with: '').
	fileStream _ FileDirectory smalltalkImageDirectory newFileNamed: nameBody , '.st'.
	fileStream timeStamp.
	self printMethodChunk: selector withPreamble: true
		on: fileStream moveSource: false toFile: 0.
	fileStream close! !


!Class methodsFor: 'fileIn/Out' stamp: 'jmv 6/17/2015 14:08'!
fileOut
	"File a description of the receiver onto a new file whose base name is the name of the receiver."

	| fileStream |
	fileStream _ FileDirectory smalltalkImageDirectory newFileNamed: self name, FileDirectory dot, 'st'.
	fileStream timeStamp.
	self sharedPools size > 0 ifTrue: [
		self shouldFileOutPools
			ifTrue: [self fileOutSharedPoolsOn: fileStream]].
	self fileOutOn: fileStream moveSource: false toFile: 0.
	fileStream close.

	"DeepCopier new checkVariables."
! !


!Class class methodsFor: 'fileIn/Out' stamp: 'jmv 6/17/2015 14:08'!
fileOutPool: aString
	"file out the global pool named aString"
	| f |
	f _ FileDirectory smalltalkImageDirectory newFileNamed: aString, '.st'.
	self new fileOutPool: (Smalltalk at: aString asSymbol) onFileStream: f. 	f close.
	! !


!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 6/17/2015 14:13'!
fileOut  "SystemOrganization fileOut"

	(FileDirectory smalltalkImageDirectory newFileNamed:
		(FileDirectory smalltalkImageDirectory nextNameFor: 'SystemOrganization' extension: 'st'))
			nextPutAll: 'SystemOrganization changeFromCategorySpecs: #(';
			newLine;
			print: SystemOrganization;  "ends with a cr"
			nextPutAll: ')!!';
			newLine;
			close.! !

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 6/17/2015 14:13'!
fileOutAllCategories
	"
	Cursor write showWhile: [
		SystemOrganization fileOutAllCategories ]
	"
	| fileStream imName |
	imName _ FileDirectory baseNameFor:
		(FileDirectory localNameFor: Smalltalk imageName).
	fileStream _ FileDirectory smalltalkImageDirectory newFileNamed: imName, '-AllCode', FileDirectory dot, 'st'.
	[
		self categories do: [ :category |
			self fileOutCategoryNoPoolsNoInit: category on: fileStream]
	] ensure: [ fileStream close ]! !

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 6/17/2015 14:13'!
fileOutCategory: category
	"FileOut all the classes in the named system category."
	| fileStream |
	fileStream _ FileDirectory smalltalkImageDirectory newFileNamed: category asFileName , '.st'.
	self fileOutCategory: category on: fileStream initializing: true.
	fileStream close! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 6/17/2015 11:33'!
fileOut
	"File out the receiver, to a file whose name is a function of the  
	change-set name and either of the date & time or chosen to have a  
	unique numeric tag, depending on the preference  
	'changeSetVersionNumbers'"
	| slips nameToUse file |
	nameToUse _ self name.
	nameToUse _ nameToUse copyReplaceAll: 'AuthorName' with: Utilities authorName asCamelCase. 
	nameToUse _ Preferences changeSetVersionNumbers
				ifTrue: [ FileDirectory smalltalkImageDirectory nextNameFor:nameToUse coda: '-', Utilities authorInitials extension: 'cs.st' ]
				ifFalse: [ (nameToUse , FileDirectory dot , Utilities dateTimeSuffix , FileDirectory dot , 'cs.st') asFileName ].
	file _ FileDirectory smalltalkImageDirectory newFileNamed: nameToUse.
	[
		file timeStamp.
		self fileOutPreambleOn: file.
		self fileOutOn: file.
		self fileOutPostscriptOn: file]
			ensure: [ file close ].
	
	self hasUnsavedChanges: false.
	Preferences checkForSlips
		ifFalse: [^ self].
	slips := self checkForSlips.
	(slips size > 0
			and: [(PopUpMenu withCaption: 'Methods in this fileOut have halts
or references to the Transcript
or other ''slips'' in them.
Would you like to browse them?' chooseFrom: 'Ignore\Browse slips')
					= 2])
		ifTrue: [Smalltalk browseMessageList: slips name: 'Possible slips in ' , name]! !


!CodeFile methodsFor: 'fileIn/fileOut' stamp: 'jmv 6/17/2015 14:08'!
fileOut
	| fileName stream |
	fileName := FillInTheBlankMorph request: 'Enter the file name' initialAnswer:''.
	stream := FileDirectory smalltalkImageDirectory newFileNamed: fileName.
	sourceSystem isEmpty ifFalse:[
		stream nextChunkPut: sourceSystem printString; newLine ].
	self fileOutOn: stream.
	stream newLine; newLine.
	classes do: [ :cls |
		cls needsInitialize ifTrue: [
			stream newLine; nextChunkPut: cls name,' initialize']].
	stream newLine.
	stream close! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 6/17/2015 14:32'!
save
	| file |
	"If we can't save, find a new destination directory."
	fullFileName ifNotNil: [
		(FileDirectory forFullFileName: fullFileName) exists ifFalse: [
			fullFileName _ nil ]].

	"If we were never saved, or never saved since image was moved, or target directory disappeared, then save to image directory."
	fullFileName ifNil: [
		fullFileName _
			FileDirectory smalltalkImageDirectory fullNameFor: self packageFileName].
		
	file _ FileStream forceNewFileNamed: fullFileName.
	[
		file timeStamp.
		self writeOnStream: file ]
			ensure: [ file close ].

	self hasUnsavedChanges: false.
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: self)! !


!ContentPack methodsFor: 'importing' stamp: 'jmv 6/17/2015 11:20'!
path: aString 
	
	| directory contentPacks |

	self flag: #todo. "Consider renaming this method. --cbr"

	directory _ FileDirectory on: 
		(FileDirectory smalltalkImageDirectory fullNameFor: aString).

	(self supportedFilesIn: directory) do: [ :i |
		| filename | filename _ directory 
			pathName , 
			directory pathNameDelimiter asString , 
			(i at: 1).
			
		self flag: #todo. "Add hook for other media types here. Also consider renaming this method. --cbr"
		self at: i name 
			put: (self import: [ Form fromFileNamed: filename ]) "This may yet be a cross-cutting concern, and need to be refactored when other media types become present. --cbr"
	].

	contentPacks _ directory directoryNames collect: [ :i |	
		i ->  (ContentPack new path: (directory fullNameFor: i))
	].
	
	^ self union: (contentPacks as: Dictionary)! !

!ContentPack methodsFor: 'private' stamp: 'jmv 6/17/2015 11:19'!
exportDirectory
	
	^ (FileDirectory smalltalkImageDirectory directoryNamed: self class defaultContentDirectory)
		directoryNamed: 'Exported'! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 6/17/2015 14:17'!
condenseChanges	
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseChanges"

	| f oldChanges classCount |
	f _ FileStream fileNamed: 'ST80.temp'.
	f timeStamp.
'Condensing Changes File...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: Smalltalk classNames size
	during:
		[:bar | classCount _ 0.
		Smalltalk allClassesDo:
			[:class | bar value: (classCount _ classCount + 1).
			class moveChangesTo: f.
			class putClassCommentToCondensedChangesFile: f.
			class class moveChangesTo: f]].
	LastQuitLogPosition _ f position.
	f close.

	CompiledMethod allInstancesDo:
		[ : e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	oldChanges _ SourceFiles at: 2.
	oldChanges close.
	FileDirectory smalltalkImageDirectory 
		deleteFileNamed: oldChanges name , '.old';
		rename: oldChanges name toBe: oldChanges name , '.old';
		rename: f name toBe: oldChanges name.
	SourceFiles at: 2
			put: (FileDirectory smalltalkImageDirectory oldFileNamed: oldChanges name).

	self inform: 'Changes file has been rewritten!!

Check that all is well, and then save/quit.
 
Otherwise, remove new changes,
replace it with the former one, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 6/17/2015 14:13'!
condenseSources	
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseSources"

	| f classCount dir newVersionString oldChangesName newChangesName newSourcesName |
	newVersionString _ FillInTheBlankMorph request: 'Please name the new sources file' initialAnswer: SourceFileVersionString.
	newVersionString ifNil: [^ self].
	newVersionString = SourceFileVersionString ifTrue: [
		^ self error: 'The new source file must not be the same as the old.'].
	SourceFileVersionString _ newVersionString.

	"Write all sources with fileIndex 1"
	newSourcesName _ self defaultSourcesName.
	f _ FileDirectory smalltalkImageDirectory newFileNamed: newSourcesName.
	f timeStamp.
	'Condensing Sources File...'
		displayProgressAt: Sensor mousePoint
		from: 0 to: Smalltalk classNames size
		during: [ :bar |
			classCount _ 0.
			Smalltalk allClassesDo: [ :class |
				bar value: (classCount _ classCount + 1).
				class fileOutOn: f moveSource: true toFile: 1]].
	f close.

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	"Make a new empty changes file"
	oldChangesName _ self currentChangesName.
	self closeSourceFiles.
	oldChangesName ifNotNil: [
		dir _ FileDirectory smalltalkImageDirectory.
		dir rename: oldChangesName toBe: oldChangesName, '.old' ].
	newChangesName _ self defaultChangesName.
	(FileDirectory smalltalkImageDirectory newFileNamed: newChangesName)
		timeStamp; close.
	LastQuitLogPosition _ 0.

	self openSourceFiles.
	self inform: 'Source files have been rewritten!!
 
Check that all is well, and then save/quit.
 
Otherwise, remove new sources/changes,
replace them with the former ones, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 6/17/2015 11:27'!
fullNameForChangesNamed: aName
	| newName |
	newName _ FileDirectory baseNameFor: (FileDirectory smalltalkImageDirectory fullNameFor: aName).
	^newName , FileDirectory dot, FileDirectory changeSuffix! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 6/17/2015 11:28'!
fullNameForImageNamed: aName
	| newName |
	newName _ FileDirectory baseNameFor: (FileDirectory smalltalkImageDirectory fullNameFor: aName).
	^newName , FileDirectory dot, FileDirectory imageSuffix! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 6/17/2015 11:29'!
getCurrentWorkingDirectory
	"Do our best effort to answer the path from wich Cuis was started.

	Smalltalk getCurrentWorkingDirectory


On Linux, starting as
	juan@juandebian:/media/sf_SharedWithLinux/testPayload03/payload$      coglinux/bin/squeak        Cuis-Smalltalk-Dev-master/Cuis4.2-2367.image
we get:
	Smalltalk vmPath                       					'/media/sf_SharedWithLinux/testPayload03/payload/coglinux/lib/squeak/4.5-3370/'
	Smalltalk imagePath   							'/media/sf_SharedWithLinux/testPayload03/payload/Cuis-Smalltalk-Dev-master'
	Smalltalk primGetCurrentWorkingDirectoryUnix    '/media/sf_SharedWithLinux/testPayload03/payload'
	Smalltalk getCurrentWorkingDirectory 			'/media/sf_SharedWithLinux/testPayload03/payload'


On Windows 7, starting as
	C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload>        cogwin\squeak.exe            Cuis-Smalltalk-Dev-master/Cuis4.2-2367.image
we get:
	Smalltalk vmPath    										'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload\cogwin\'
	Smalltalk imagePath   									'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload\Cuis-Smalltalk-Dev-master'
	Smalltalk primGetCurrentWorkingDirectoryWindows       	'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload\Cuis-Smalltalk-Dev-master'
	Smalltalk getCurrentWorkingDirectory  					'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload'

	Easy on Unix. Harder on Windows, because #primGetCurrentWorkingDirectoryWindows insists on lying and answers the directory containing the image.
	"
	
	| fullImagePathName imageSpecCmdLineArgument |

	"Easy on Unix"
	 self primGetCurrentWorkingDirectoryUnix ifNotNil: [ :cwd | ^ cwd ].

	"On Windows, extract shell path if image is in subtree"
	fullImagePathName _ Smalltalk imageName.
	imageSpecCmdLineArgument _ Smalltalk getSystemAttribute: 1.
	(imageSpecCmdLineArgument size < fullImagePathName size and: [			"Not if they are equal, as if they are both fully qualified (absolute) paths"
		fullImagePathName asPathTokens endsWith: imageSpecCmdLineArgument asPathTokens]) ifTrue: [
			^ fullImagePathName copyFrom: 1 to: fullImagePathName size - imageSpecCmdLineArgument size - 1 ].

	"If we don't know better, answer nil. If appropriate, senders do:
		Smalltalk getCurrentWorkingDirectory ifNil: [ Smalltalk imagePath ]
	to have, at least, some valid directory
	"
	^ nil! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 6/17/2015 11:30'!
logError: errMsg inContext: aContext to: aFilename
	"Log the error message and a stack trace to the given file."

	| ff |
	FileDirectory smalltalkImageDirectory deleteFileNamed: aFilename ifAbsent: nil.
	(ff _ FileStream fileNamed: aFilename) ifNil: [^ self "avoid recursive errors"].

  	ff nextPutAll: errMsg; newLine.
	aContext errorReportOn: ff.
	ff close.! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 6/17/2015 11:30'!
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
				nextPutAll: (FileDirectory smalltalkImageDirectory localNameFor: self imageName);
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

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 6/17/2015 11:30'!
saveAs: newName andQuit: aBoolean clearAllClassState: clearAllStateFlag
	"Save the image  under a new name."

	| newChangesName |
	self currentChangesName ifNotNil: [ :oldChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName _ self fullNameForChangesNamed: newName.
		FileDirectory smalltalkImageDirectory 
			copyFileWithoutOverwriteConfirmationNamed: oldChangesName
			toFileNamed: newChangesName ].

	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: aBoolean
		clearAllClassState: clearAllStateFlag! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 6/17/2015 11:34'!
saveAsEmbeddedImage
	"Save the current state of the system as an embedded image"

	| dir newName newImageName |
	dir _ FileDirectory smalltalkImageDirectory.
	newName _ FillInTheBlankMorph
		request: 'Select existing VM file'
		initialAnswer: (FileDirectory localNameFor: '').
	newName = '' ifTrue: [^ self].
	newName _ FileDirectory baseNameFor: newName asFileName.

	newImageName _ newName.
	(dir includesKey: newImageName) ifFalse:
		[^ self inform: 'Unable to find name ', newName, ' Please choose another name.'].

	self logChange: '----SAVEAS (EMBEDDED) ', newName, '----', Date dateAndTimeNow printString.
	self imageName: (dir fullNameFor: newImageName).
	LastImageName _ self imageName.
	self closeSourceFiles.
	self snapshot: true andQuit: true embedded: true clearAllClassState: false
! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 6/17/2015 11:30'!
saveAsNewVersion
	"Save the image/changes using the next available version number."
	"
	Smalltalk saveAsNewVersion
	"
	| fileName newName changesName systemVersion |
	self okayToSave ifFalse: [ ^ self ].
	systemVersion _ SystemVersion current.
	fileName _ String streamContents: [ :strm |
		strm
			nextPutAll: 'Cuis';
			print: systemVersion versionMajor;
			nextPut: $.;
			print: systemVersion versionMinor;
			nextPut: $-;
			print: systemVersion highestUpdate ].
	newName _ fileName, FileDirectory dot,  FileDirectory imageSuffix.
	(FileDirectory smalltalkImageDirectory includesKey: newName) ifTrue: [
		newName _ FileDirectory smalltalkImageDirectory
			nextNameFor: fileName
			extension: FileDirectory imageSuffix ].
	changesName _ self fullNameForChangesNamed: newName.
	"Check to see if there is a .changes file that would cause a problem if we saved a new .image file with the new version number"
	(FileDirectory smalltalkImageDirectory includesKey: changesName) ifTrue: [
		^ self inform:
'There is already .changes file of the desired name,
', newName, '
curiously already present, even though there is
no corresponding .image file.   Please remedy
manually and then repeat your request.' ].
	"Try to clear all user state, including all class vars, preferences, etc"
	self saveAs: newName andQuit: false clearAllClassState: true! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 6/17/2015 14:18'!
externalizeSources   
	"Write the sources and changes streams onto external files."
 	"Smalltalk externalizeSources"

	| sourcesName changesName aFile |
	sourcesName _ self defaultSourcesName.
	(FileDirectory smalltalkImageDirectory fileExists: sourcesName)
		ifTrue: [^ self inform:
'Sorry, you must first move or remove the
file named ', sourcesName].
	changesName _ self defaultChangesName.
	(FileDirectory smalltalkImageDirectory fileExists: changesName)
		ifTrue: [^ self inform:
'Sorry, you must first move or remove the
file named ', changesName].

	aFile _ FileDirectory smalltalkImageDirectory newFileNamed: sourcesName.
	aFile nextPutAll: SourceFiles first originalContents.
	aFile close.
	SourceFiles at: 1 put: (FileDirectory smalltalkImageDirectory readOnlyFileNamed: sourcesName).

	aFile _ FileDirectory smalltalkImageDirectory newFileNamed: changesName.
	aFile nextPutAll: SourceFiles last contents.
	aFile close.
	SourceFiles at: 2 put: (FileDirectory smalltalkImageDirectory oldFileNamed: changesName).

	self inform: 'Sources successfully externalized'! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 6/17/2015 14:19'!
openSourcesAndChanges
	"Open the changes and sources files and install them in SourceFiles. Inform the user of problems regarding write permissions or Lf/CrLf mixups."
	"Note: SourcesName and imageName are full paths; changesName is a  
	local name."
	| sourcesPathName changesPathName sources changes msg wmsg |
	msg _ 'Cuis cannot locate XfileRef
Please check that the file is named properly and is in the
same directory as this image.'.
	wmsg _ 'Cuis cannot write to XfileRef.

Please check that you have write permission for this file.

You won''t be able to save this image correctly until you fix this.'.

	"Do not open source files if internalized (i.e. notNil)"
	sources _ SourceFiles at: 1.
	sources ifNil: [
		sourcesPathName _ Smalltalk defaultSourcesName.
		sources _ FileDirectory smalltalkImageDirectory oldFileOrNoneNamed: sourcesPathName.
		sources ifNil: [
			sourcesPathName _ Smalltalk alternativeSourcesName.
			sources _ FileDirectory smalltalkImageDirectory oldFileOrNoneNamed: sourcesPathName ]].
	(sources isNil and: [ Preferences valueOfFlag: #warnIfNoSourcesFile ])
		ifTrue: [
			Smalltalk platformName = 'Mac OS' ifTrue: [
				msg _ msg , String newLineString, 'Make sure the sources file is not an Alias.'].
			self inform: (msg copyReplaceAll: 'XfileRef' with: 'the sources file named ' , sourcesPathName) ].

	changesPathName _ Smalltalk defaultChangesName.
	"Do not open source files if internalized (i.e. notNil)"
	changes _ (SourceFiles at: 2) ifNil: [ FileDirectory smalltalkImageDirectory oldFileNamed: changesPathName ].
	(changes isNil and: [ Preferences valueOfFlag: #warnIfNoChangesFile ])
		ifTrue: [self inform: (msg copyReplaceAll: 'XfileRef' with: 'the changes file named ' , changesPathName)].

	((Preferences valueOfFlag: #warnIfNoChangesFile) and: [changes notNil]) ifTrue: [
		changes isReadOnly ifTrue: [
			self inform: (wmsg copyReplaceAll: 'XfileRef' with: 'the changes file named ' , changesPathName)].

		((changes next: 200) includesSubString: String crlfString) ifTrue: [
			self inform: 'The changes file named ' , changesPathName , '
has been injured by an unpacking utility.  Lfs were changed to CrLfs.
Please set the preferences in your decompressing program to 
"do not convert text files" and unpack the system again.']].

	SourceFiles _ Array with: sources with: changes! !

!SystemDictionary methodsFor: 'toDeprecate' stamp: 'jmv 6/17/2015 11:29'!
getFileNameFromUser

	| newName |
	newName _ FillInTheBlankMorph
		request: 'New File Name?'
		initialAnswer: (FileDirectory localNameFor: self imageName).
	newName isEmpty ifTrue: [^nil].
	((FileDirectory smalltalkImageDirectory fileOrDirectoryExists: (self fullNameForImageNamed: newName)) or:
	 [FileDirectory smalltalkImageDirectory fileOrDirectoryExists: (self fullNameForChangesNamed: newName)]) ifTrue: [
		(self confirm: ('{1} already exists. Overwrite?' format: {newName})) ifFalse: [^nil]].
	^newName
! !

!SystemDictionary methodsFor: 'image format' stamp: 'jmv 6/17/2015 14:30'!
imageFormatVersionFromFileAsIs
	"Answer an integer identifying the type of image on file. The image version number may
	identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements
	of the image (e.g. block closure support required). If the image file has a different
	endianness than the VM, the format version will appear byte-swapped."
	"
	Smalltalk imageFormatVersionFromFileAsIs
	"
	| answer file |
	file _ FileDirectory smalltalkImageDirectory readOnlyFileNamed: Smalltalk imageName.
	[
	answer _ (file
		 binary;
		 next: 4)
		unsignedLongAt: 1
		bigEndian: Smalltalk isBigEndian ] ensure: [ file close ].
	^ answer! !


!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 6/17/2015 11:55'!
findPackageFileAsReqOf: mainFeatureOrNil
	"Look in known places for packages providing required feature.
	Answer wether search was successful."
	| packageFileName |
	pathName ifNotNil: [
		"the directory here is dummy. if pathName is not nil it is a full path (change this to FileMan, please!!)"
		(FileDirectory smalltalkImageDirectory fileExists: pathName) ifTrue: [ ^ true ]].
	packageFileName _ self packageFileName.
	(mainFeatureOrNil ifNil: [ self ]) placesToLookForPackagesDo: [ :fileDirectory |
		(fileDirectory fileExists: packageFileName) ifTrue: [
			pathName _ fileDirectory fullNameFor: packageFileName.
			^ true ]].
	^ false! !

!FeatureRequirement methodsFor: 'private' stamp: 'jmv 6/17/2015 13:45'!
placesToLookForPackagesDo: aBlock

	| base myDir |

	"Look inside my own folder"
	pathName ifNotNil: [
		myDir _ FileDirectory forFullFileName: pathName.
		aBlock value: myDir ].

	"Look in Cuis image folder and reasonable subfolders"
	base _ FileDirectory smalltalkImageDirectory.
	self inPackagesSubtreeOf: base do: aBlock.
	
	"Look in parent directory and reasonable subfolders. 
	Useful when image is stored in a subdirectory of the main app directory.
	This could be the case when the package comes from a 'main' git repo, and image is copied from gitHub"
	self inPackagesSubtreeOf: base containingDirectory do: aBlock.

	"Also look in host OS current directory"
	base _ FileDirectory currentDirectory.
	self inPackagesSubtreeOf: base do: aBlock.! !


!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/17/2015 14:32'!
oldFileOrNoneNamed: fileName
	"If the file exists, answer a read-only FileStream on it. If it doesn't, answer nil."
	| fullName |

	"If full path name is not specified, get it assuming current directory."
	fullName _ self fullNameFor: fileName.

	^ FileStream oldFileOrNoneNamed: fullName
! !

!FileDirectory methodsFor: 'enumeration' stamp: 'jmv 6/17/2015 11:21'!
directoryNames
	"Return a collection of names for the subdirectories of this directory."
	"FileDirectory smalltalkImageDirectory directoryNames"
	^ (self entries select: [ :entry |
		entry isDirectory ]) collect: [ :entry |
		entry name ]! !

!FileDirectory methodsFor: 'enumeration' stamp: 'jmv 6/17/2015 11:21'!
entries
	"Return a collection of directory entries for the files and directories in the directory with the given path. See primLookupEntryIn:index: for further details."
	"
	FileDirectory smalltalkImageDirectory entries
	"

	| entries index done entryArray |
	entries _ OrderedCollection new: 200.
	index _ 1.
	done _ false.
	[ done ] whileFalse: [
		entryArray _ self primLookupEntryIn: pathName index: index.
		#badDirectoryPath == entryArray ifTrue: [
			^ (InvalidDirectoryError pathName: pathName) signal ].
		entryArray
			ifNil: [ done _ true ]
			ifNotNil: [ entries addLast: (DirectoryEntry fromArray: entryArray directory: self) ].
		index _ index + 1 ].

	^ entries asArray! !

!FileDirectory methodsFor: 'enumeration' stamp: 'jmv 6/17/2015 11:21'!
fileAndDirectoryNames
	"FileDirectory smalltalkImageDirectory fileAndDirectoryNames"

	^ self entries collect: [:entry | entry name]
! !

!FileDirectory methodsFor: 'enumeration' stamp: 'jmv 6/17/2015 11:23'!
fileNames
	"Return a collection of names for the files (but not directories) in this directory."
	"FileDirectory smalltalkImageDirectory fileNames"
	^ (self entries select: [ :entry |
		entry isDirectory not ]) collect: [ :entry |
		entry name ]! !

!FileDirectory methodsFor: 'testing' stamp: 'jmv 6/17/2015 11:21'!
directoryExists: filenameOrPath
	"Answer true if a directory of the given name exists. The given name may be either a full path name or a local directory within this directory."
	"FileDirectory smalltalkImageDirectory directoryExists: FileDirectory smalltalkImageDirectory pathName"

	| fName dir |
	self class activeDirectoryClass splitName: filenameOrPath to:
		[:filePath :name |
			fName _ name.
			filePath isEmpty
				ifTrue: [dir _ self]
				ifFalse: [dir _ self directoryNamed: filePath]].

	^dir exists and: [
		self isCaseSensitive 
			ifTrue:[dir directoryNames includes: fName]
			ifFalse:[dir directoryNames anySatisfy: [:name| name sameAs: fName]]].
! !

!FileDirectory methodsFor: 'testing' stamp: 'jmv 6/17/2015 11:23'!
fileExists: filenameOrPath
	"Answer true if a file of the given name exists. The given name may be either a full path name or a local file within this directory."
	"FileDirectory smalltalkImageDirectory fileExists: Smalltalk defaultSourcesName"

	| fName dir |
	self class activeDirectoryClass splitName: filenameOrPath to:
		[:filePath :name |
			fName _ name.
			filePath isEmpty
				ifTrue: [dir _ self]
				ifFalse: [dir _ FileDirectory on: filePath]].
	self isCaseSensitive 
		ifTrue:[^dir fileNames includes: fName]
		ifFalse:[^dir fileNames anySatisfy: [:name| name sameAs: fName]].	! !

!FileDirectory methodsFor: 'file operations' stamp: 'jmv 6/17/2015 11:21'!
copyFileNamed: fileName1 toFileNamed: fileName2
	"Copy the contents of the existing file with the first name into a new file with the second name. Both files are assumed to be in this directory."
	"FileDirectory smalltalkImageDirectory copyFileNamed: 'todo.txt' toFileNamed: 'todocopy.txt'"

	| file1 file2 |
	file1 _ (self readOnlyFileNamed: fileName1) binary.
	file2 _ (self newFileNamed: fileName2) binary.
	self copyFile: file1 toFile: file2.
	file1 close.
	file2 close.
! !

!FileDirectory methodsFor: 'file operations' stamp: 'jmv 6/17/2015 11:21'!
copyFileWithoutOverwriteConfirmationNamed: fileName1 toFileNamed: fileName2
	"Copy the contents of the existing file with the first name into a file with the second name (which may or may not exist). If the second file exists, force an overwrite without confirming.  Both files are assumed to be in this directory."
	"FileDirectory smalltalkImageDirectory copyFileWithoutOverwriteConfirmationNamed: 'todo.txt' toFileNamed: 'todocopy.txt'"

	| file1 file2 |
	fileName1 = fileName2 ifTrue: [^ self].
	file1 _ (self readOnlyFileNamed: fileName1) binary.
	file2 _ (self forceNewFileNamed: fileName2) binary.
	self copyFile: file1 toFile: file2.
	file1 close.
	file2 close.! !

!FileDirectory methodsFor: 'file operations' stamp: 'jmv 6/17/2015 11:24'!
fileOrDirectoryExists: filenameOrPath
	"Answer true if either a file or a directory file of the given name exists. The given name may be either a full path name or a local name within this directory."
	"FileDirectory smalltalkImageDirectory fileOrDirectoryExists: Smalltalk defaultSourcesName"

	| fName dir |
	self class activeDirectoryClass splitName: filenameOrPath to: [ :filePath :name |
		fName _ name.
		dir _ filePath isEmpty
			ifTrue: [self]
			ifFalse: [FileDirectory on: filePath]].

	^ (dir includesKey: fName) or: [ fName = '' and:[ dir entries size > 1]]! !

!FileDirectory methodsFor: 'file name utilities' stamp: 'jmv 6/17/2015 11:23'!
fileNamesMatching: pat
	"FileDirectory smalltalkImageDirectory fileNamesMatching: '*'"

	^ self fileNames select: [:name | pat match: name]
! !

!FileDirectory methodsFor: 'searching' stamp: 'jmv 6/17/2015 11:24'!
filesContaining: searchString caseSensitive: aBoolean
	| aList |
	"Search the contents of all files in the receiver and its subdirectories for the search string.  Return a list of paths found.  Make the search case sensitive if aBoolean is true."

	aList _ OrderedCollection new.
	self withAllFilesDo: [ :stream |
			(stream contentsOfEntireFile includesSubstring: searchString caseSensitive: aBoolean)
				ifTrue:	[ aList add: stream name ]]
		andDirectoriesDo: [ :d | d pathName ].
	^ aList

"FileDirectory smalltalkImageDirectory filesContaining: 'includesSubstring:'  caseSensitive: true"! !


!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 6/17/2015 11:24'!
baseNameFor: filename
	"Return the given file name without its extension, if any. We have to remember that many (most?) OSs allow extension separators within directory names and so the leaf filename needs to be extracted, trimmed and rejoined. Yuck"
	"The test is 
		FileDirectory baseNameFor: ((FileDirectory smalltalkImageDirectory directoryNamed: 'foo.bar') fullNameFor:'blim.blam') 
		should end 'foo.bar/blim' (or as appropriate for your platform AND
		
		FileDirectory baseNameFor: ((FileDirectory smalltalkImageDirectory directoryNamed: 'foo.bar') fullNameFor:'blim')
		should be the same and NOT  'foo'
		
		Oh, and
		FileDirectory baseNameFor: 'foo.bar'
		should be 'foo' not '/foo' "

	| ext |
	ext _ self extensionFor: filename.
	ext isEmpty ifTrue: [
		^filename ].
	^filename copyFrom: 1 to: filename size - ext size - 1.
! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 6/17/2015 13:43'!
checkName: fileName fixErrors: flag
	"Check a string fileName for validity as a file name on the current default file system. Answer the original file name if it is valid. If the name is not valid (e.g., it is too long or contains illegal characters) and fixing is false, raise an error. If fixing is true, fix the name (usually by truncating and/or tranforming characters), and answer the corrected name. The default behavior is to truncate the name to 31 chars. Subclasses can do any kind of checking and correction appropriate to the underlying platform."

"This doesn't really belong in the class. Different file systems (mounted on the same machine)
can have different ideas about this. In any case, maybe this dies when we adopt FileMan."

	^ self smalltalkImageDirectory
		checkName: fileName
		fixErrors: flag
! !

!FileDirectory class methodsFor: 'create/delete file' stamp: 'jmv 6/17/2015 11:58'!
lookInUsualPlaces: fileName
	"Check the current directory, the imagePath, and the vmPath (and the vmPath's owner) for this file."

	| dir |
	((dir _ FileDirectory currentDirectory) fileExists: fileName)
		ifTrue: [^ dir fileNamed: fileName].

	((dir _ FileDirectory smalltalkImageDirectory) fileExists: fileName)
		ifTrue: [^ dir fileNamed: fileName].

	((dir _ FileDirectory vmDirectory) fileExists: fileName)
		ifTrue: [^ dir fileNamed: fileName].

	((dir _ dir containingDirectory) fileExists: fileName)
		ifTrue: [^ dir fileNamed: fileName].

	^ nil! !

!FileDirectory class methodsFor: 'cacher state access' stamp: 'jmv 6/17/2015 11:05'!
releaseClassCachedState

	DirectoryClass _ nil.
	ImageDirectory _ nil.
	VMDirectory _ nil.
	CurrentDirectory _ nil! !


!AcornFileDirectory methodsFor: 'enumeration' stamp: 'jmv 6/17/2015 13:19'!
entries
	"Return a collection of directory entries for the files and directories in 
	the directory with the given path. See primLookupEntryIn:index: for 
	further details."
	"
	FileDirectory smalltalkImageDirectory entries
	"

	| entries extraPath |
	entries _ super entries.
	pathName isEmpty
		ifTrue: [
			"For Acorn we also make sure that at least the parent of the current dir 
			is added - sometimes this is in a filing system that has not been (or 
			cannot be) polled for disc root names"
			extraPath _  self class smalltalkImageDirectory containingDirectory.
			"Only add the extra path if we haven't already got the root of the current dir in the list"
			(entries anySatisfy: [:ent | extraPath fullName beginsWith: ent name]) 
				ifFalse: [
					entries _ entries
								copyWith: (DirectoryEntryDirectory
										directory: self
										name: extraPath fullName
										creationTime: 0
										modificationTime: 0
										fileSize: 0) ]].
	^ entries! !


!FmFileEntry class methodsFor: 'examples' stamp: 'jmv 6/17/2015 11:26'!
example1
	"FmFileEntry example1"

	"- Create subDirectory named: 'subDir'.
	 - Put a new file named: 'file1'.
	 - Write contents 'Hello!! to that file'"

	"Traditional way (Squeak)"
	"| subDir str |
	subDir := FileDirectory smalltalkImageDirectory directoryNamed: 'subDir'.
	subDir assureExistence.
	[str := subDir newFileNamed: 'file1'.
	str nextPutAll: 'Hello!!']
		ensure: [str close]."

	"FileMan"

	'./subDir' asDirectoryEntry at: 'file2' put: 'Hello!!'! !


!FmDirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 6/17/2015 11:47'!
on: aFileDirectry 
	"FmDirectory on: FileDirectory smalltalkImageDirectory"
	
	| inst |
	inst := self pathName: aFileDirectry pathName.
	^ inst! !


!Form methodsFor: 'fileIn/Out' stamp: 'jmv 6/17/2015 13:32'!
writeOnFileNamed: fileName 
	"Saves the receiver on the file fileName in the format:
		fileCode, depth, extent, offset, bits."
	| file |
	file _ FileStream forceNewFileNamed: fileName.
	file binary.
	file nextPut: 2.  "file code = 2"
	self writeOn: file.
	file close
"
 | f |
[(f _ Form fromUser) boundingBox area>25] whileTrue:
	[f writeOnFileNamed: 'test.form'.
	(Form fromFileNamed: 'test.form') display].
"! !


!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 6/17/2015 13:32'!
putForm: aForm onFileNamed: fileName
	"Store the given form on a file of the given name."

	| writer |
	writer _ self onBinaryStream: (FileStream forceNewFileNamed: fileName) binary.
	[ writer nextPutImage: aForm ] ensure: [
		writer close ]! !


!BMPReadWriter class methodsFor: 'testing' stamp: 'jmv 6/17/2015 11:42'!
displayAllFrom: fd
	"BMPReadWriter displayAllFrom: FileDirectory smalltalkImageDirectory"
	fd fileNames do:[:fName|
		(fName endsWith: '.bmp') ifTrue:[
			[(Form fromBinaryStream: (fd readOnlyFileNamed: fName) binary) display.
			Display forceDisplayUpdate] on: Error do:[:nix|].
		].
	].
	fd directoryNames do:[:fdName|
		self displayAllFrom: (fd directoryNamed: fdName)
	].! !

!BMPReadWriter class methodsFor: 'testing' stamp: 'jmv 6/17/2015 11:42'!
readAllFrom: fd
	"MessageTally spyOn:[BMPReadWriter readAllFrom: FileDirectory smalltalkImageDirectory]"
	fd fileNames do:[:fName|
		(fName endsWith: '.bmp') ifTrue:[
			[Form fromBinaryStream: (fd readOnlyFileNamed: fName) binary] on: Error do:[:nix].
		].
	].
	fd directoryNames do:[:fdName|
		self readAllFrom: (fd directoryNamed: fdName)
	].! !


!JPEGReadWriter2 class methodsFor: 'image reading/writing' stamp: 'jmv 6/17/2015 13:30'!
putForm: aForm quality: quality progressiveJPEG: progressiveFlag onFileNamed: fileName
	"Store the given Form as a JPEG file of the given name, overwriting any existing file of that name. Quality goes from 0 (low) to 100 (high), where -1 means default. If progressiveFlag is true, encode as a progressive JPEG."

	| writer |
	writer _ self onBinaryStream: (FileStream forceNewFileNamed: fileName) binary.
	writer nextPutImage: aForm quality: quality progressiveJPEG: progressiveFlag.
	writer close.
! !


!InstructionPrinter class methodsFor: 'printing' stamp: 'jmv 6/17/2015 14:10'!
printClass: class 
	"Create a file whose name is the argument followed by '.bytes'. Store on 
	the file the symbolic form of the compiled methods of the class."
	| file |
	file := FileDirectory smalltalkImageDirectory newFileNamed: class name , '.bytes'.
	class selectorsDo: [ :sel | 
		file newLine; nextPutAll: sel; newLine.
		(self on: (class compiledMethodAt: sel)) printInstructionsOn: file].
	file close
	"InstructionPrinter printClass: Parser."
! !


!MessageTally class methodsFor: 'spying' stamp: 'jmv 6/17/2015 14:10'!
spyOn: aBlock toFileNamed: fileName reportOtherProcesses: aBoolean
	"Spy on the evaluation of aBlock. Write the data collected on a file
	named fileName.
	Not supported (yet) by AndreasSystemProfiler"

	| file value node |
	node _ self new.
	node reportOtherProcesses: aBoolean.
	value _ node spyEvery: self defaultPollPeriod on: aBlock.
	file _ FileDirectory smalltalkImageDirectory newFileNamed: fileName.
	node report: file.
	file close.
	^value! !


!FileListWindow class methodsFor: 'instance creation' stamp: 'jmv 6/17/2015 11:43'!
openFileList
	"
	FileListWindow openFileList
	"
	FileListWindow open: (FileList new directory: FileDirectory currentDirectory) label: nil! !


!StandardFileMenu methodsFor: 'basic control sequences' stamp: 'jmv 6/17/2015 14:00'!
getTypedFileName: aResult

	| name |
	name _ FillInTheBlankMorph 
		request: 'Enter a new file name' 
		initialAnswer: ''.
	name = '' ifTrue: [^self startUpWithCaption: 'Select a File:' ].
	name _ aResult directory fullNameFor: name.
	^ StandardFileMenuResult
			directory: (FileDirectory forFullFileName: name)
			name: (FileDirectory localNameFor: name)
! !


!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 6/17/2015 14:11'!
fileOut
	| f |
	f := (FileDirectory smalltalkImageDirectory newFileNamed: self name,'.st').
	self fileOutOn: f.
	self needsInitialize ifTrue:[
		f newLine; nextChunkPut: self name,' initialize'.
	].
	f close! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 6/17/2015 14:11'!
fileOutCategory: categoryName
	| f |
	f := (FileDirectory smalltalkImageDirectory newFileNamed: self name,'-',categoryName,'.st').
	self fileOutMethods: (self organization listAtCategoryNamed: categoryName)
			on: f.
	f close
! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 6/17/2015 14:11'!
fileOutMethod: selector
	| f |
	f := (FileDirectory smalltalkImageDirectory newFileNamed: self name,'-', selector, '.st').
	self fileOutMethods: (Array with: selector)
			on: f.
	f close! !


!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 6/17/2015 11:26'!
compareTallyIn: beforeFileName to: afterFileName
	"SpaceTally new compareTallyIn: 'tally' to: 'tally2'"

	| answer s beforeDict a afterDict allKeys before after diff |
	beforeDict _ Dictionary new.
	s _ FileDirectory smalltalkImageDirectory fileNamed: beforeFileName.
	 [s atEnd ] whileFalse: [
		a _ Array readFrom: s nextLine.
		beforeDict at: a first put: a allButFirst.
	].
	s close.
	afterDict _ Dictionary new.
	s _ FileDirectory smalltalkImageDirectory fileNamed: afterFileName.
	[ s atEnd ] whileFalse: [
		a _ Array readFrom: s nextLine.
		afterDict at: a first put: a allButFirst.
	].
	s close.
	answer _ WriteStream on: String new.
	allKeys _ (Set new addAll: beforeDict keys; addAll: afterDict keys; yourself) asArray sort.
	allKeys do: [ :each |
		before _ beforeDict at: each ifAbsent: [#(0 0 0)].
		after _ afterDict at: each ifAbsent: [#(0 0 0)].
		diff _ before with: after collect: [ :vBefore :vAfter | vAfter - vBefore].
		diff = #(0 0 0) ifFalse: [
			answer nextPutAll: each, '  ', diff printString; newLine.
		].
	].
	TextModel new contents: answer contents; openLabel: 'space diffs'! !

!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 6/17/2015 14:11'!
printSpaceAnalysis	
	"SpaceTally new printSpaceAnalysis"

	| stream |
	stream _ FileDirectory smalltalkImageDirectory newFileNamed: 'STspace.text'.
	[ self printSpaceAnalysis: 1 on: stream ] ensure: [ stream close ]! !

!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 6/17/2015 14:29'!
printSpaceDifferenceFrom: fileName1 to: fileName2
	"For differential results, run printSpaceAnalysis twice with different fileNames,
	then run this method...
		SpaceTally new printSpaceAnalysis: 0 on: 'STspace.text1'.
			--- do something that uses space here ---
		SpaceTally new printSpaceAnalysis: 0 on: 'STspace.text2'.
		SpaceTally new printSpaceDifferenceFrom: 'STspace.text1' to: 'STspace.text2'
"
	| f coll1 coll2 item |
	f _ FileDirectory smalltalkImageDirectory readOnlyFileNamed: fileName1.
	coll1 _ OrderedCollection new.
	[f atEnd] whileFalse: [coll1 add: f crLfNextLine].
	f close.
	f _ FileDirectory smalltalkImageDirectory readOnlyFileNamed: fileName2.
	coll2 _ OrderedCollection new.
	[f atEnd] whileFalse: [
		item _ f crLfNextLine.
		((coll1 includes: item) and: [(item endsWith: 'percent') not])
			ifTrue: [coll1 remove: item]
			ifFalse: [coll2 add: item]].
	f close.
	(TextModel new contents: (String streamContents: 
			[ :s | 
			s nextPutAll: fileName1; newLine.
			coll1 do: [:x | s nextPutAll: x; newLine].
			s newLine; newLine.
			s nextPutAll: fileName2; newLine.
			coll2 do: [:x | s nextPutAll: x; newLine]]))
		openLabel: 'Differential Space Analysis'.
! !

!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 6/17/2015 11:26'!
saveTo: aFileName
	"| st |
	st := SpaceTally new.
	st spaceTally: (Array with: TextMorph with: Point).
	st saveTo: 'spaceTally2'"
	| s |
	(FileDirectory smalltalkImageDirectory fileExists: aFileName) ifTrue: [
		FileDirectory smalltalkImageDirectory deleteFileNamed: aFileName].
	s _ FileDirectory smalltalkImageDirectory fileNamed: aFileName.
	results do: [:each | s nextPutAll: each analyzedClassName asString ; 
						nextPutAll: ' '; nextPutAll: each codeSize printString; 
						nextPutAll: ' '; nextPutAll: each instanceCount printString; 
						nextPutAll: ' '; nextPutAll: each spaceForInstances printString; newLine].
	s close! !


!StandardFileStream methodsFor: 'access' stamp: 'jmv 6/17/2015 13:49'!
directory
	"Return the directory containing this file."

	^ FileDirectory forFullFileName: self fullName
! !


!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 6/17/2015 14:01'!
crc16OfFileNamed: fileName
	"
	StandardFileStream crc16OfFileNamed: 'cursor.jpeg'
	StandardFileStream crc16OfFileNamed: 'deafultPID.txt'
	"

	| f answer |
	f _ self new open: (FileDirectory smalltalkImageDirectory fullNameFor: fileName) forWrite: false.
	f ifNil: [^nil].
	[ answer _ f crc16 ] ensure: [ f close ].
	^answer
! !

!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 6/17/2015 14:31'!
fileNamed: fileName
	"Open a file with the given name for reading and writing. If the name has no directory part, then the file will be created in the currentDirectory directory. If the file already exists, its prior contents may be modified or replaced, but the file will not be truncated on close."

	^ self new open: (FileDirectory currentDirectory fullNameFor: fileName) forWrite: true
! !

!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 6/17/2015 14:31'!
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
	localName _ FileDirectory localNameFor: fullName.
	dir
		deleteFileNamed: localName
		ifAbsent: [(CannotDeleteFileException new
			messageText: 'Could not delete the old version of file ' , fullName) signal].
	f _ self new open: fullName forWrite: true.
	^ f
		ifNil: ["Failed to open the file"
			(FileDoesNotExistException fileName: fullName) signal]! !

!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 6/17/2015 14:31'!
isAFileNamed: fileName
	"Answer true if a file of the given name exists."
	^self isAFileFullNamed: (FileDirectory currentDirectory fullNameFor: fileName)! !

!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 6/17/2015 14:31'!
newFileNamed: fileName
 	"Create a new file with the given name, and answer a stream opened for writing on that file. If the file already exists, ask the user what to do."

	| fullName |
	fullName _ FileDirectory currentDirectory fullNameFor: fileName.

	^(self isAFileFullNamed: fullName)
		ifTrue: ["file already exists:"
			(FileExistsException fileName: fullName fileClass: self) signal]
		ifFalse: [self new open: fullName forWrite: true]

! !

!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 6/17/2015 14:32'!
oldFileNamed: fileName
	"Open an existing file with the given name for reading and writing. If the name has no directory part, then the file will be created in the currentDirectory directory. If the file already exists, its prior contents may be modified or replaced, but the file will not be truncated on close."

	| fullName |
	fullName _ FileDirectory currentDirectory fullNameFor: fileName.

	^(self isAFileFullNamed: fullName)
		ifTrue: [self new open: fullName forWrite: true]
		ifFalse: ["File does not exist..."
			(FileDoesNotExistException fileName: fullName) signal]! !

!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 6/17/2015 14:33'!
oldFileOrNoneNamed: fileName
	"If the file exists, answer a read-only FileStream on it. If it doesn't, answer nil."

	| fullName |

	"If full path name is not specified, get it assuming current directory."
	fullName _ FileDirectory currentDirectory fullNameFor: fileName.

	^ (self isAFileFullNamed: fullName)
		ifTrue: [ self readOnlyFileFullNamed: fullName ]! !

!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 6/17/2015 14:31'!
readOnlyFileNamed: fileName 
	"Open an existing file with the given name for reading."

	| fullName |
	fullName _ FileDirectory currentDirectory fullNameFor: fileName.
	^ self readOnlyFileFullNamed: fullName

	"StandardFileStream readOnlyFileNamed: 'kjsd.txt' "! !

!StandardFileStream class methodsFor: 'error handling' stamp: 'jmv 6/17/2015 13:49'!
fileExistsUserHandling: fullFileName
	| dir localName choice newName |
	dir _ FileDirectory forFullFileName: fullFileName.
	localName _ FileDirectory localNameFor: fullFileName.
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

!StandardFileStream class methodsFor: 'error handling' stamp: 'jmv 6/17/2015 13:50'!
readOnlyFileDoesNotExistUserHandling: fullFileName

	| dir files choices selection newName fileName |
	dir _ FileDirectory forFullFileName: fullFileName.
	files _ dir fileNames.
	fileName _ FileDirectory localNameFor: fullFileName.
	choices _ fileName correctAgainst: files.
	choices add: 'Choose another name'.
	choices add: 'Cancel'.
	selection _ (PopUpMenu labelArray: choices lines: (Array with: 5) )
		startUpWithCaption: (FileDirectory localNameFor: fullFileName), '
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


!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 6/17/2015 14:29'!
reportCPUandRAM
	"Write several text files with useful analysis for profiling purposes.
	Overwrites any existing report.
	Utilities reportCPUandRAM
	"	

	| stream tally |
	
	"VM statistics (Memory use and GC, mainly)"
	stream _ FileDirectory smalltalkImageDirectory forceNewFileNamed: 'MemoryStats.txt'.
	[ stream nextPutAll: Utilities vmStatisticsReportString ] 
		ensure: [ stream close ].
	
	"Process list"
	stream _ FileStream forceNewFileNamed: 'ProcessList.txt'.
	[
		ProcessBrowser new processNameList 
			do: [ :each | 
				stream nextPutAll: each; newLine ]
	] ensure: [ stream close ].

"Fork all these, so they run in sequence, as the system is back running"
[
	
	"Process taking most CPU"
	stream _ FileDirectory smalltalkImageDirectory forceNewFileNamed: 'ThePig.txt'.
	ProcessBrowser dumpPigStackOn: stream andClose: true.
	
	"Tally of all processes"
	stream _ FileDirectory smalltalkImageDirectory forceNewFileNamed: 'FullTally.txt'.
	[
		tally _ MessageTally new.
		tally reportOtherProcesses: true.	"actually irrelevant"
		tally spyAllEvery: 1 on: [ (Delay forMilliseconds: 1000) wait ].
		tally report: stream ] ensure: [ stream close ].

	"Memory Analysis"
	stream _ FileDirectory smalltalkImageDirectory forceNewFileNamed: 'MemoryAnalysis.txt'.
	[ SpaceTally new printSpaceAnalysis: 1 on: stream ]
		ensure: [ stream close ]

] forkNamed: 'CPU usage analysis'! !

!methodRemoval: FileStream class #fullName:!
FileStream class removeSelector: #fullName:!
!methodRemoval: FileDirectory class #default!
FileDirectory class removeSelector: #default!
!methodRemoval: FileDirectory class #deleteFilePath:!
FileDirectory class removeSelector: #deleteFilePath:!
!methodRemoval: FileDirectory class #directoryEntryFor:!
FileDirectory class removeSelector: #directoryEntryFor:!
!methodRemoval: FileDirectory class #forFileName:!
FileDirectory class removeSelector: #forFileName:!
!methodRemoval: FileDirectory class #isLegalFileName:!
FileDirectory class removeSelector: #isLegalFileName:!
!classDefinition: #FileDirectory category: #'System-Files'!
Object subclass: #FileDirectory
	instanceVariableNames: 'pathName'
	classVariableNames: 'CurrentDirectory DirectoryClass ImageDirectory VMDirectory'
	poolDictionaries: ''
	category: 'System-Files'!
!methodRemoval: FeatureRequirement #cuisAndCuisPackagesSubdirectoriesOf:do:!
FeatureRequirement removeSelector: #cuisAndCuisPackagesSubdirectoriesOf:do:!
!methodRemoval: CodeFile class #fromFileNamed:!
CodeFile class removeSelector: #fromFileNamed:!
!methodRemoval: CodeFile #fromFileNamed:!
CodeFile removeSelector: #fromFileNamed:!
!methodRemoval: ChangeSet class #defaultChangeSetDirectory!
ChangeSet class removeSelector: #defaultChangeSetDirectory!
!methodRemoval: ChangeSet #defaultChangeSetDirectory!
ChangeSet removeSelector: #defaultChangeSetDirectory!
