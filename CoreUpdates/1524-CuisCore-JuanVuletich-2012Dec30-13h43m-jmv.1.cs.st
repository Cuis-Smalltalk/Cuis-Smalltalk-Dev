'From Cuis 4.1 of 12 December 2012 [latest update: #1523] on 30 December 2012 at 2:44:14 pm'!

!ChangeSet commentStamp: 'jmv 12/30/2012 14:29' prior: 0!
ChangeSets keep track of the changes made to a system, so they can be written on a file as source code (a "fileOut").
There are separate ChangeSets for caturing changes done to the Cuis base system and changes done to Packages. Usually the ChangeSets for Packages are not important, and ignored, because Packages are saved on pck.st files. Change sets for the Cuis base system are the standard way of capturing changes to Cuis, and are used to evolve Cuis itself.

name - a String used to name the changeSet, and thus any associated project or fileOut.

preamble and postscript:  two strings that serve as prefix (useful for documentation) and suffix (useful for doits) to the fileout of the changeSet.

changeRecords -  Dictionary {class name -> a ClassChangeRecord}.
These classChangeRecords (qv) remember all of the system changes.

structures -    Dictionary {#Rectangle -> #(<classVersionInteger> 'origin' 'corner')}.
Of  the names of the instances variables before any changes for all classes in classChanges, and all of their superclasses.  In the same format used in SmartRefStream.  Inst var names are strings.  

superclasses -    Dictionary {#Rectangle -> #Object}.
Of all classes in classChanges, and all of their superclasses.

Structures and superclasses save the instance variable names of this class and all of its superclasses.  Later we can tell how it changed and write a conversion method.  The conversion method is used when old format objects are brought in from the disk from ImageSegment files (.extSeg) or SmartRefStream files (.obj .morph .bo .sp).

NOTE:  It should be fairly simple, by adding a bit more information to the classChangeRecords, to reconstruct the information now stored in 'structures' and 'superclasses'.  This would be a welcome simplification.
!


!CodePackage commentStamp: 'jmv 12/30/2012 14:29' prior: 0!
A CodePackage is a package that is currently loaded in the system. If saved (.pck.st), then it is stored in a file that can be dealt with as an instance of PackageFile. As the code is already in the system, all we need to know is the packageName. Implementation is originally based on PackageInfo, but has diverged.

CodePackage instances are usually created when installing CodePackageFiles. These instances track the code for that package, that we'll need to save if we don't want to lose changes. These instances are held in the InstalledPackages class variable.

We can also create 'transient' instances with whatever name (and classes and extension methods) we chose, like
	(CodePackage named: 'Collections' createIfAbsent: true registerIfNew: false) inspect; save
This won't mean the system is actually partitioned in such way.

(CodePackage named: 'TestPackage' createIfAbsent: true registerIfNew: false) inspect; save!


!CodePackageFile commentStamp: 'jmv 12/30/2012 14:31' prior: 0!
A CodePackageFile represents a file with code for a package, regardless of whether it is installed (as a CodePackage) or not. It supports Cuis' .pck.st as well as .mcz. If formats diverge in the future, maybe a separate class for .mcz compatibility is in order.

Note: .mcz compatibility is only for browsing .mcz files and installing code (selectively or as a whole). Cuis doesn't support writing .mcz files.!


!CodeFileBrowser commentStamp: 'jmv 12/30/2012 14:43' prior: 0!
I am a class browser view on a fileout (either a source file (.st) or change set (.cs.st)). I do not actually load the code into to the system, nor do I alter the classes in the image. Use me to vet code in a comfortable way before loading it into your image.

From a FileList, I can be invoked by selecting a source file and selecting the "browse code" menu item from the context (right click) menu.

I use PseudoClass, PseudoClassOrganizers, and PseudoMetaclass to model the class structure of the source file.!


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 12/30/2012 14:32'!
fileOut
	"File out the receiver, to a file whose name is a function of the  
	change-set name and either of the date & time or chosen to have a  
	unique numeric tag, depending on the preference  
	'changeSetVersionNumbers'"
	| slips nameToUse |
	self checkForConversionMethods.
	nameToUse := Preferences changeSetVersionNumbers
				ifTrue: [self defaultChangeSetDirectory nextNameFor: self name coda: '-', Utilities authorInitials extension: 'cs.st']
				ifFalse: [(self name , FileDirectory dot , Utilities dateTimeSuffix , FileDirectory dot , 'cs.st') asFileName].
	Cursor write
		showWhile: [
			| file |
			file := self defaultChangeSetDirectory newFileNamed: nameToUse.
			[
				file timeStamp.
				self fileOutPreambleOn: file.
				self fileOutOn: file.
				self fileOutPostscriptOn: file]
					ensure: [file close]].
	
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


!CodePackage methodsFor: 'saving' stamp: 'jmv 12/30/2012 14:31'!
save
	| nameToUse |
	fullFileName ifNil: [
		fullFileName _
			ChangeSet defaultChangeSetDirectory fullNameFor: (self packageName, FileDirectory dot, 'pck.st')].
	nameToUse _ fullFileName.
"	nameToUse _ Preferences changeSetVersionNumbers
		ifTrue: [
			ChangeSet defaultChangeSetDirectory
				nextNameFor: self packageName coda: '-', Utilities authorInitials
				extension: 'pck.st' ]
		ifFalse: [ (self packageName , FileDirectory dot , Utilities dateTimeSuffix , FileDirectory dot , 'pck.st') asFileName ]."
	Cursor write
		showWhile: [
			| file |
			file _ ChangeSet defaultChangeSetDirectory forceNewFileNamed: nameToUse.
			[
				file timeStamp.
				self writeOnStream: file ]
					ensure: [ file close ]].
	self hasUnsavedChanges: false.
	ChangeSorter removeChangeSet: (ChangeSet changeSetForPackage: self)! !


!CodeProvider methodsFor: 'annotation' stamp: 'jmv 12/30/2012 14:35'!
annotationForSelector: aSelector ofClass: aClass 
	"Provide a line of content for an annotation pane, representing  
	information about the given selector and class"
	| stamp sendersCount implementorsCount aCategory separator aString aList aComment aStream requestList |
	aSelector == #Comment
		ifTrue: [^ self annotationForClassCommentFor: aClass].
	aSelector == #Definition
		ifTrue: [^ self annotationForClassDefinitionFor: aClass].
	aSelector == #Hierarchy
		ifTrue: [^ self annotationForHierarchyFor: aClass].
	aStream _ ReadWriteStream on: ''.
	requestList _ self annotationRequests.
	separator _ requestList size > 1
				ifTrue: [self annotationSeparator]
				ifFalse: [''].
	requestList
		do: [:aRequest | 
			aRequest == #firstComment
				ifTrue: [
					aComment _ aClass firstCommentAt: aSelector.
					aComment isEmptyOrNil
						ifFalse: [aStream nextPutAll: aComment , separator]].
			aRequest == #masterComment
				ifTrue: [
					aComment _ aClass supermostPrecodeCommentFor: aSelector.
					aComment isEmptyOrNil
						ifFalse: [aStream nextPutAll: aComment , separator]].
			aRequest == #documentation
				ifTrue: [
					aComment _ aClass precodeCommentOrInheritedCommentFor: aSelector.
					aComment isEmptyOrNil
						ifFalse: [aStream nextPutAll: aComment , separator]].
			aRequest == #timeStamp
				ifTrue: [
					stamp _ self timeStamp.
					aStream
						nextPutAll: (stamp size > 0
								ifTrue: [stamp , separator]
								ifFalse: ['no timeStamp' , separator])].
			aRequest == #messageCategory
				ifTrue: [
					aCategory _ aClass organization categoryOfElement: aSelector.
					aCategory
						ifNotNil: ["woud be nil for a method no longer present,  
							e.g. in a recent-submissions browser"
							aStream nextPutAll: aCategory , separator]].
			aRequest == #sendersCount
				ifTrue: [
					sendersCount _ Smalltalk numberOfSendersOf: aSelector.
					sendersCount _ sendersCount = 1
								ifTrue: ['1 sender']
								ifFalse: [sendersCount printString , ' senders'].
					aStream nextPutAll: sendersCount , separator].
			aRequest == #implementorsCount
				ifTrue: [
					implementorsCount _ Smalltalk numberOfImplementorsOf: aSelector.
					implementorsCount _ implementorsCount = 1
								ifTrue: ['1 implementor']
								ifFalse: [implementorsCount printString , ' implementors'].
					aStream nextPutAll: implementorsCount , separator].
			aRequest == #priorVersionsCount
				ifTrue: [
					self
						addPriorVersionsCountForSelector: aSelector
						ofClass: aClass
						to: aStream].
			aRequest == #priorTimeStamp
				ifTrue: [
					stamp _ VersionsBrowser
								timeStampFor: aSelector
								class: aClass
								reverseOrdinal: 2.
					stamp
						ifNotNil: [aStream nextPutAll: 'prior time stamp: ' , stamp , separator]].
			aRequest == #recentChangeSet
				ifTrue: [
					aString _ ChangeSorter mostRecentChangeSetWithChangeForClass: aClass selector: aSelector.
					aString size > 0
						ifTrue: [aStream nextPutAll: aString , separator]].
			aRequest == #allChangeSets
				ifTrue: [
					aList _ ChangeSorter allChangeSetsWithClass: aClass selector: aSelector.
					aList size > 0
						ifTrue: [aList size = 1
								ifTrue: [aStream nextPutAll: 'only in change set ']
								ifFalse: [aStream nextPutAll: 'in change sets: '].
							aList
								do: [:aChangeSet | aStream nextPutAll: aChangeSet name , ' ']]
						ifFalse: [aStream nextPutAll: 'in no change set'].
					aStream nextPutAll: separator].
			aRequest == #allBaseSystemChangeSets
				ifTrue: [
					aList _ (ChangeSorter allChangeSetsWithClass: aClass selector: aSelector) select: [ :it | it isForBaseSystem ].
					aList size > 0
						ifTrue: [
							aList size = 1
								ifTrue: [aStream nextPutAll: 'only in base system change set']
								ifFalse: [aStream nextPutAll: 'in base system change sets:'].
							aList do: [:aChangeSet | aStream nextPut: Character space; nextPutAll: aChangeSet name ]]
						ifFalse: [aStream nextPutAll: 'in no base system change set'].
					aStream nextPutAll: separator].
			aRequest == #closuresInfo
				ifTrue: [
					aString _ aClass closuresInfoAt: aSelector.
					aString size > 0
						ifTrue: [aStream nextPutAll: aString , separator]].
			aRequest == #packages
				ifTrue: [
					(aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :cm |
						(CodePackage packageOfMethod: cm methodReference ifNone: nil)
							ifNil: [ aStream nextPutAll: 'part of base system (i.e. not in a package)' ]
							ifNotNil: [ :codePackage |
								aStream nextPutAll: 'in package '; nextPutAll: codePackage name ].
						aStream nextPutAll: separator]].
		].
	^ aStream contents! !


!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 12/30/2012 13:43'!
browseMCZContents: aStream
	"Browse the selected file."
	| unzipped changeList fullName packageFile pkName |
	
	"For Monticello packages we do as we do with our own .pck.st files: Instead of just browsing
	contents, also include what is no longer part of the package (and should therefore be removed on install)
	See #browsePackageContents:
	However, this was never tested to run!!"
	self flag: #jmvVer.

	fullName _ aStream name.
	pkName _ CodePackageFile monticelloPackageNameFrom: fullName.
	unzipped _ aStream asUnZippedStream: 'snapshot/source.st'.
	unzipped ascii.
	Cursor read showWhile: [
		changeList _ self new scanFile: unzipped from: 0 to: unzipped size.
		aStream reset.
		packageFile _ CodePackageFile
			buildFileStream: unzipped
			packageName: pkName
			fullName: fullName ].
	"Add deletions of methods and classes that are in the PackageInfo (i.e., active in the system)
	but are no longer in the PackageFile being viewed."
	packageFile methodsToRemove do: [ :methodReference |
		changeList
			addItem: (MethodDeletionChangeRecord new methodReference: methodReference)
			text: 'method no longer in package: ', methodReference asStringOrText ].
	packageFile classesToRemove do: [ :clsName |
		changeList
			addItem: (ClassDeletionChangeRecord new clsName: clsName)
			text: 'class no longer in package: ', clsName ].
	changeList clearSelections.
	ChangeListWindow open: changeList label: aStream name! !

!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 12/30/2012 14:27'!
fileReaderServicesForFile: fullName suffix: suffix
	| services |
	services _ OrderedCollection new.
	(FileStream isSourceFileSuffix: suffix)
		ifTrue: [ services add: self serviceBrowseChangeFile ].
	(suffix = 'pck') | (suffix = 'pck.st')
		ifTrue: [ services add: self serviceBrowsePackageFile ].
	(suffix = 'changes') | (suffix = '*')
		ifTrue: [ services add: self serviceBrowseDotChangesFile ].
	(suffix = 'mcz' or: [ suffix = '*'])
		ifTrue: [ services add: self serviceBrowseMCZContents ].
	^services! !


!ChangeSorter class methodsFor: 'fileIn/Out' stamp: 'jmv 12/30/2012 14:39'!
fileReaderServicesForFile: fullName suffix: suffix

	^(FileStream isSourceFileSuffix: suffix)
		ifTrue: [ self services]
		ifFalse: [#()]! !


!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'jmv 12/30/2012 14:28'!
fileReaderServicesForFile: fullName suffix: suffix

	(FileStream isSourceFileSuffix: suffix)
		ifTrue: [ ^ { self serviceBrowseCode } ].

	suffix = 'mcz'
		ifTrue: [ ^ { self serviceBrowseMCZCode. self serviceInstallMonticelloPackage } ].

	(suffix = 'pck') | (suffix = 'pck.st')
		ifTrue: [ ^ { self serviceBrowseCode. self serviceInstallPackage } ].
	^#()! !


!FileDirectory methodsFor: 'file operations' stamp: 'jmv 12/30/2012 13:54'!
mimeTypesFor: fileName
	"Return a list of MIME types applicable to the receiver. This default implementation uses the file name extension to figure out what we're looking at but specific subclasses may use other means of figuring out what the type of some file is. Some systems like the macintosh use meta data on the file to indicate data type"

	| ext |
	ext _ FileDirectory extensionFor: fileName.
	^StandardMIMEMappings at: ext asLowercase ifAbsent: nil! !


!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 12/30/2012 14:06'!
baseNameFor: filename
	"Return the given file name without its extension, if any. We have to remember that many (most?) OSs allow extension separators within directory names and so the leaf filename needs to be extracted, trimmed and rejoined. Yuck"
	"The test is 
		FileDirectory baseNameFor: ((FileDirectory default directoryNamed: 'foo.bar') fullNameFor:'blim.blam') 
		should end 'foo.bar/blim' (or as appropriate for your platform AND
		
		FileDirectory baseNameFor: ((FileDirectory default directoryNamed: 'foo.bar') fullNameFor:'blim')
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

!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 12/30/2012 14:12'!
extensionFor: pathName
	"Return the extension of given file name, if any.
	FileDirectory extensionFor: 'writings.txt'
	FileDirectory extensionFor: 'optionalstuff.pck.st'
	FileDirectory extensionFor: 'optionalstuff.pck'
	FileDirectory extensionFor: 'code.cs.st'
	FileDirectory extensionFor: 'code.cs'
	"

	| delim i |
	self splitName: pathName to: [ :path :filename |
		delim _ DirectoryClass extensionDelimiter.
		{ 'cs.st' . 'pck.st' } do: [ :specialExtension |
			(filename endsWith: delim asString, specialExtension)
				ifTrue: [ ^specialExtension ]].
		i _ filename findLast: [ :c | c = delim ].
		^ i = 0
			ifTrue: [ '']
			ifFalse: [ filename copyFrom: i + 1 to: filename size ]]! !


!FileList methodsFor: 'file list menu' stamp: 'jmv 12/30/2012 14:22'!
itemsForFile: fullName
	"Answer a list of services appropriate for a file of the given full name"
	| suffix |
	suffix _ (FileDirectory extensionFor: fullName) asLowercase.
	^ (self class itemsForFile: fullName) , (self myServicesForFile: fullName suffix: suffix)! !


!FileList class methodsFor: 'file reader registration' stamp: 'jmv 12/30/2012 14:22'!
itemsForFile: fullName
	"Answer a list of services appropriate for a file of the given full name"

	| services suffix |
	suffix _ (FileDirectory extensionFor: fullName) asLowercase.
	services _ OrderedCollection new.
	self registeredFileReaderClasses do: [ :reader |
		reader ifNotNil: [services addAll: (reader fileReaderServicesForFile: fullName suffix: suffix)]].
	^ services! !


!FileStream class methodsFor: 'file reader services' stamp: 'jmv 12/30/2012 14:26'!
isSourceFileSuffix: suffix

	^#('st' 'cs' 'cs.st' '*') anySatisfy: [ :each | each = suffix ]! !


!GZipReadStream class methodsFor: 'fileIn/Out' stamp: 'jmv 12/30/2012 13:55'!
saveContents: fullFileName
	"Save the contents of a gzipped file"
	| zipped buffer unzipped newName |
	newName _ FileDirectory baseNameFor: fullFileName.
	unzipped _ FileStream newFileNamed: newName.
	unzipped binary.
	zipped _ GZipReadStream on: (FileStream readOnlyFileNamed: fullFileName).
	buffer _ ByteArray new: 50000.
	'Extracting ' , fullFileName
		displayProgressAt: Sensor mousePoint
		from: 0
		to: zipped sourceStream size
		during: [ :bar | 
			[ zipped atEnd ]
				whileFalse: [
					bar value: zipped sourceStream position.
					unzipped nextPutAll: (zipped nextInto: buffer) ].
			zipped close.
			unzipped close ].
	^ newName! !

!methodRemoval: FileList class #suffixOf:!
FileList class removeSelector: #suffixOf:!
