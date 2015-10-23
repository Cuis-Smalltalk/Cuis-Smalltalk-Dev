'From Cuis 4.2 of 25 July 2013 [latest update: #2542] on 20 October 2015 at 3:25:05.902519 pm'!
!classDefinition: #FmFileEntry category: #'FileMan-Core'!
Object subclass: #FmFileEntry
	instanceVariableNames: 'drive pathComponents parent name fileSize creationTime modificationTime isDirectory options '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FileMan-Core'!

!FmFileEntry methodsFor: 'actions-path' stamp: 'jmv 10/20/2015 15:03'!
concatPathComponents: components defaultToFile: aBoolean
	| entry entryComponents parentEntry lastParentEntry |
	components ifEmpty: [ ^self ].
	parentEntry := self isRoot ifFalse: [ self ].
	lastParentEntry _ parentEntry.
	entryComponents := self pathComponents.

	"Firstly, assume it is a directory, as the rest of the chain."
	components do: [ :eachComponent |
		entryComponents := entryComponents copyWith: eachComponent.
		entry := FmDirectoryEntry pathComponents: entryComponents drive: self drive.
		parentEntry ifNotNil: [
			entry setParent: parentEntry ].
		lastParentEntry _ parentEntry.
		parentEntry := entry ].

	"But if it doesn't exist as a directory, assume it must be a file, regardless of its existance on the file system."
	(aBoolean and: [ entry exists not ]) ifTrue: [
		entry := FmFileEntry pathComponents: entryComponents drive: self drive.
		lastParentEntry ifNotNil: [
			entry setParent: lastParentEntry ]].

	^entry! !


!FmDirectoryEntry methodsFor: 'actions-path' stamp: 'jmv 10/20/2015 15:06'!
directoryNamed: subdirectoryName
	"Needed when we want an instance of FmDirectoryEntry even for a directory that is not (yet) on the file system."
	^self concatPathComponents: { subdirectoryName } defaultToFile: false! !


!FileList methodsFor: 'file menu action' stamp: 'jmv 10/20/2015 15:05'!
addNewDirectory

	self 
		addNew: 'Directory'
		byEvaluating: [ :newName | (directory directoryNamed: newName) assureExistence ].
	self updateDirectory.
	self changed: #initialDirectoryList! !


!ChangeSet class methodsFor: 'services' stamp: 'jmv 10/20/2015 15:14'!
fileIn: anFmFileEntry
	"File in the entire contents of the file specified by the name provided"

	anFmFileEntry ifNil: [^ Smalltalk beep ].
	anFmFileEntry readStream: [ :stream |
		stream fileIn ]! !

!ChangeSet class methodsFor: 'services' stamp: 'jmv 10/20/2015 15:14'!
install: anFmFileEntry
	"File in the entire contents of the file specified by the name provided.
	Do not affect the user change sets, store changes in separate one"

	ChangeSet installing: anFmFileEntry name do: [ self fileIn: anFmFileEntry ].
	('Installed ChangeSet: ', anFmFileEntry name) print! !

!ChangeSet class methodsFor: 'services' stamp: 'jmv 10/20/2015 15:05'!
installNewUpdates
 	
	| updatesFileDirectory |

	updatesFileDirectory _ FmDirectoryEntry smalltalkImageDirectory directoryNamed: 'CoreUpdates'.

	updatesFileDirectory exists
		ifFalse: [ self inform: 'Could not find a CoreUpdates folder\No updates loaded' withNewLines ] 
		ifTrue: [ ChangeSet installNewUpdates: updatesFileDirectory ]! !


!String methodsFor: '*fileman-core-converting' stamp: 'jmv 10/20/2015 14:47'!
asDirectoryEntry
	"esta está ok. PEro:
	
	'C:\Windows' asDirectoryEntry
	('C:' asDirectoryEntry / 'Windows')
	- Deben entregar lo mismo. Una instancia de FmDirectoryEntry que existe y todo eso.
	
	"
	^FmDirectoryEntry pathName: self! !

!String methodsFor: '*fileman-core-converting' stamp: 'jmv 10/20/2015 14:46'!
asFileEntry
	"esta está ok (que exista, no la implementacion!!)
	'C:\Windows' asFileEntry exists
	'C:\Windows' asDirectoryEntry exists
	('C:' asDirectoryEntry / 'Windows') exists
	('C:' asDirectoryEntry / 'Windows') asDirectoryEntry exists
	
	Yo creo que
	
	'C:\Windows' asFileEntry exists -> false esta bien. Si lo queres como archivo es cosa tuya. Ahora:
	- que no exista (ditto)
	- que no se pueda grabar nada ni con force, ni con assure ni con nada!!
	- que no haya ningun polimorpfismo loco con directory
	
	'C:\Windows' asDirectoryEntry
	('C:' asDirectoryEntry / 'Windows')
	- Deben entregar lo mismo. Una instancia de FmDirectoryEntry que existe y todo eso.
	
	Y algun dia, que Directory no herede de File!!!!!!!!!!!!
	Mientras, al menos, que de error PARA TODO EL PROTOCOLO ABSURDO
	
	'.' asFileEntry children
	"
	self isRelativeMark ifTrue: [ ^self error: 'Maybe you need to call #asDirectoryEntry!!' ].
	^FmFileEntry pathName: self! !


!ContentPack methodsFor: 'importing' stamp: 'jmv 10/20/2015 15:06'!
path: aString 
	
	| contentPacks directory |

	self flag: #todo. "Consider renaming this method. --cbr"

	directory _ FmDirectoryEntry smalltalkImageDirectory / aString.

	(self supportedFilesIn: directory) do: [ :i |
		| filename |
		filename _ directory pathName , '/', (i at: 1).
			
		self flag: #todo. "Add hook for other media types here. Also consider renaming this method. --cbr"
		self at: i name 
			put: (self import: [ Form fromFileNamed: filename ]) "This may yet be a cross-cutting concern, and need to be refactored when other media types become present. --cbr"
	].

	contentPacks _ directory directoryNames collect: [ :i |	
		i ->  (ContentPack new path: (directory directoryNamed: i) pathName)
	].
	
	^ self union: (contentPacks as: Dictionary)! !


!FeatureRequirement methodsFor: 'private' stamp: 'jmv 10/20/2015 15:06'!
inPackagesSubtreeOf: anFmDirectoryEntry do: aBlock

	| pckDir compatPckDir |

	"Look in the requested directory"
	aBlock value: anFmDirectoryEntry.

	"Look in the usual Packages subfolders"
	pckDir _ anFmDirectoryEntry directoryNamed: 'Packages'.
	pckDir exists ifTrue: [
		aBlock value: pckDir ].
	compatPckDir _ anFmDirectoryEntry directoryNamed: 'CompatibilityPackages'.
	compatPckDir exists ifTrue: [
		aBlock value: compatPckDir ].

	"Finally look in folders that follow the convention of naming package repositories
	with the 'Cuis-Smalltalk' prefix, and their possible 'Packages' subdir."
	anFmDirectoryEntry children do: [ :entry |
		(entry isDirectory and: [ entry name beginsWith: 'Cuis-Smalltalk' ]) ifTrue: [
			aBlock value: entry.
			pckDir _ entry directoryNamed: 'Packages'.
			pckDir exists ifTrue: [
				aBlock value: pckDir ].
			compatPckDir _ entry directoryNamed: 'CompatibilityPackages'.
			compatPckDir exists ifTrue: [
				aBlock value: compatPckDir ]]]! !


!FmFileEntry methodsFor: 'actions-path' stamp: 'jmv 10/20/2015 15:03'!
/ pathString
"todo esto deberia bajar a la subclase... aca arriba no tiene sentido!!!!!!!!!!!!"
	^self, pathString! !

!FmFileEntry methodsFor: 'actions-path' stamp: 'jmv 10/20/2015 15:04'!
concat: pathString
	^self concatPathComponents: pathString asString asPathComponents defaultToFile: true! !

!FmFileEntry methodsFor: 'actions-path' stamp: 'jmv 10/20/2015 15:00'!
concatPathComponents: components
	| entry entryComponents parentEntry |
	components ifEmpty: [ ^self ].
	parentEntry := self isRoot ifFalse: [ self ].
	entryComponents := self pathComponents.

	"Firstly, assume it is a directory, as the rest of the chain."
	components do: [ :eachComponent |
		entryComponents := entryComponents copyWith: eachComponent.
		entry := FmDirectoryEntry pathComponents: entryComponents drive: self drive.
		parentEntry ifNotNil: [
			entry setParent: parentEntry ].
		parentEntry := entry ].

	"But if it doesn't exist as a directory, assume it must be a file, regardless of its existance on the file system."
	entry exists ifFalse: [
		entry := FmFileEntry pathComponents: entryComponents drive: self drive.
		parentEntry ifNotNil: [
			entry setParent: parentEntry ]].

	^entry! !

!FmFileEntry methodsFor: 'enumeration' stamp: 'jmv 10/20/2015 15:11'!
childrenDo: aBlock
	self error: 'no'.
	^self asDirectoryEntry childrenDo: aBlock! !

!FmFileEntry methodsFor: 'enumeration' stamp: 'jmv 10/20/2015 15:11'!
directoriesDo: aBlock
	self error: 'no'.
	^self asDirectoryEntry directoriesDo: aBlock! !

!FmFileEntry methodsFor: 'enumeration' stamp: 'jmv 10/20/2015 15:11'!
directoriesMatches: selectionBlock
	self error: 'no'.
	^self asDirectoryEntry directoriesMatches: selectionBlock! !

!FmFileEntry methodsFor: 'enumeration' stamp: 'jmv 10/20/2015 15:11'!
filesDo: aBlock
	self error: 'no'.
	^self asDirectoryEntry filesDo: aBlock! !

!FmFileEntry methodsFor: 'enumeration' stamp: 'jmv 10/20/2015 15:11'!
filesMatches: selectionBlock
	self error: 'no'.
	^self asDirectoryEntry filesMatches: selectionBlock! !

!FmFileEntry methodsFor: 'enumeration' stamp: 'jmv 10/20/2015 15:11'!
latestFileMatches: selectionBlock
	self error: 'no'.
	^self asDirectoryEntry latestFileMatches: selectionBlock! !

!FmFileEntry methodsFor: 'enumeration' stamp: 'jmv 10/20/2015 15:11'!
oldestFileMatches: selectionBlock
	self error: 'no'.
	^self asDirectoryEntry oldestFileMatches: selectionBlock! !

!FmFileEntry methodsFor: 'actions-file' stamp: 'jmv 10/20/2015 15:16'!
copyTo: filename 
	| targetEntry |
	(self fileAccessor directoryExists: self pathName) ifTrue: [
		
		self error: 'no'.
		^self asDirectoryEntry copyTo: filename ].
	
	targetEntry := filename asFileEntry.
	targetEntry isDirectory
		ifTrue: [targetEntry := (targetEntry / self name) ensureAsFile].
	self assureExistence.
	targetEntry assureExistence.
	self fileAccessor copy: self to: targetEntry! !

!FmFileEntry methodsFor: 'dictionary-like' stamp: 'jmv 10/20/2015 15:11'!
at: localFileName
	self error: 'no'.
	^self asDirectoryEntry at: localFileName asString ifAbsent:[]! !

!FmFileEntry methodsFor: 'dictionary-like' stamp: 'jmv 10/20/2015 15:11'!
at: localFileName ifAbsent: block
	self error: 'no'.
	^self asDirectoryEntry at: localFileName asString ifAbsent: block! !

!FmFileEntry methodsFor: 'dictionary-like' stamp: 'jmv 10/20/2015 15:11'!
at: localFileName put: contents
	self error: 'no'.
	^self asDirectoryEntry at: localFileName asString put: contents! !

!FmFileEntry methodsFor: 'dictionary-like' stamp: 'jmv 10/20/2015 15:11'!
binaryAt: localFileName 
	self error: 'no'.
	^self asDirectoryEntry binaryAt: localFileName asString ifAbsent:[]! !

!FmFileEntry methodsFor: 'dictionary-like' stamp: 'jmv 10/20/2015 15:11'!
binaryAt: localFileName ifAbsent: block
	self error: 'no'.
	^self asDirectoryEntry binaryAt: localFileName asString ifAbsent: block! !

!FmFileEntry methodsFor: 'dictionary-like' stamp: 'jmv 10/20/2015 15:11'!
includesKey: fileName
	self error: 'no'.
	^self asDirectoryEntry includesKey: fileName! !

!FmFileEntry methodsFor: 'dictionary-like' stamp: 'jmv 10/20/2015 15:11'!
keys
	self error: 'no'.
	^self asDirectoryEntry keys! !

!FmFileEntry methodsFor: 'dictionary-like' stamp: 'jmv 10/20/2015 15:12'!
removeKey: localFileName 
	self error: 'no'.
	^self asDirectoryEntry removeKey: localFileName asString! !

!FmFileEntry methodsFor: 'dictionary-like' stamp: 'jmv 10/20/2015 15:12'!
removeKey: localFileName ifAbsent: failBlock
	self error: 'no'.
	^self asDirectoryEntry removeKey: localFileName asString ifAbsent: failBlock! !

!FmFileEntry methodsFor: 'accessing' stamp: 'jmv 10/20/2015 15:11'!
children
	self error: 'no'.
	^self asDirectoryEntry children! !

!FmFileEntry methodsFor: 'accessing' stamp: 'jmv 10/20/2015 15:11'!
directories
	self error: 'no'.
	^self asDirectoryEntry directories! !

!FmFileEntry methodsFor: 'accessing' stamp: 'jmv 10/20/2015 15:11'!
files
	self error: 'no'.
	^self asDirectoryEntry files! !

!FmFileEntry methodsFor: 'testing' stamp: 'jmv 10/20/2015 14:50'!
isDirectory
	^false! !

!FmFileEntry methodsFor: 'actions-pipe' stamp: 'jmv 10/20/2015 15:18'!
pipe: filterBlock to: filename 
	| nextEntry inStream outStream |
	nextEntry := filename asFileEntry.
	[inStream := self privateReadStream.
	outStream := nextEntry privateWriteStream.
	filterBlock value: inStream value: outStream]
		ensure: [
			inStream close.
			outStream close].
	^nextEntry
! !

!FmFileEntry methodsFor: 'actions-pipe' stamp: 'jmv 10/20/2015 15:18'!
pipeRepeat: filterBlock to: filename 
	^self pipeRepeat: filterBlock while: [:in :out | in atEnd not] to: filename ! !

!FmFileEntry methodsFor: 'actions-pipe' stamp: 'jmv 10/20/2015 15:18'!
pipeRepeat: filterBlock while: terminateBlock to: filename 
	| nextEntry inStream outStream |
	nextEntry := filename asFileEntry.
	[inStream := self privateReadStream.
	outStream := nextEntry privateWriteStream.
	[terminateBlock value: inStream value: outStream]
		whileTrue: [filterBlock value: inStream value: outStream]]
		ensure: [
			inStream close.
			outStream close].
	^nextEntry! !


!FmDirectoryEntry methodsFor: 'converting' stamp: 'jmv 10/20/2015 14:36'!
ensureAsFile

	^FmFileEntry pathComponents: self pathComponents drive: self drive! !

!FmDirectoryEntry methodsFor: 'actions-directory' stamp: 'jmv 10/20/2015 15:17'!
copyTo: filename 
	| fileEnt toDir |
	fileEnt := filename asDirectoryEntry.
	(self fileAccessor fileExists: fileEnt pathName) ifTrue: [^self error: 'Cannot copy directory to file'].
	
	toDir := fileEnt assureExistence.

	self
		filesDo: [:file | file copyTo: (toDir / file name)].
	
	self
		directoriesDo: [ :dir |
			dir copyTo:  dir name ]! !

!FmDirectoryEntry methodsFor: 'private' stamp: 'jmv 10/20/2015 15:22'!
initChildren

	self exists ifFalse: [ ^children _ #()] .
	children _ self fileAccessor entriesIn: self pathName from: self.
	^children! !

!FmDirectoryEntry methodsFor: 'testing' stamp: 'jmv 10/20/2015 15:09'!
exists
	
	(self isDriveSupported and: [self pathComponents isEmpty])
		 ifTrue: [^self fileAccessor drives includes: self ].

	self isRoot ifTrue: [^true].
	
	self fileAccessor onUnix ifFalse: [^self fileAccessor directoryExists: self pathName]. 
	
	^self fileAccessor directoryExists: self name in: self parent pathName! !

!FmDirectoryEntry methodsFor: 'testing' stamp: 'jmv 10/20/2015 14:50'!
isDirectory
	^true! !


!FmDirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 10/20/2015 15:10'!
roots
	"Appropriate for all systems, including those with several roots, each being a logical 'drive' (Like Windows),
	and for systems having a single root with file systems (i.e. 'drives') mounted anywhere in the tree (Unix, MacOS)
	"
	"
	FmDirectoryEntry roots
	"
	^ FmFileIOAccessor default drives
		ifEmpty: [
			"
			On Linux. Check on Mac.
			En Mac estaria bien. Seria un raiz del sistema donde estan los discos como 1er subfolder"
			{ '/' asDirectoryEntry } ]

	"Chequear en Windows. Deberia contestar el root de cada disco, bien formado.
	El Name seria vacio. El disco bien puesto. #name contesta el disco."
	"aca, mandar asFileEntry o asDirectoryEntry deberia contestar lo mismo.
	Es mas, no tiene sentido que existan esos 2 metodos........
	En todo caso, #exists es falso y no hace nada!!"! !


!FmFileIOAccessor methodsFor: 'private' stamp: 'jmv 10/20/2015 14:49'!
entriesIn: directoryFullPath from: parentEntryOrNil
	"
	Warning: Private. Only to be called from within FileMan.
	Warning: If parentEntryOrNil is nil, answered instances are broken, 
	and essentially only useful for asking back the attributes we have just set.
	Not pretty.

		FmFileIOAccessor default entriesIn: '' from: nil
	
		FmFileIOAccessor default entriesIn: '.' from: '.' asFileEntry
		
	YAW (YetAnotherWarning): On Unix, directoryFullPath = '' means 
		FmDirectoryEntry currentDirectory
		FmDirectoryEntry smalltalkImageDirectory
			(NOT SURE!!!!!!!!)
	In Windows, it means 'give me the disk drives'...
	On Mac?
	"
	| entries index done entryArray entry isDirectory |
	entries := OrderedCollection new: 200.
	index := 1.
	done := false.
	[done] whileFalse: [
		entryArray := self lookupEntryIn: directoryFullPath index: index.
		#badDirectoryPath = entryArray ifTrue: [
			^#()].
		entryArray == nil
			ifTrue: [done := true]
			ifFalse: [
				isDirectory _ entryArray at: 4.
				entry _ isDirectory
					ifTrue: [FmDirectoryEntry new]
					ifFalse: [FmFileEntry new].
				entry name: (entryArray at: 1).
				entry creationTime: (entryArray at: 2).
				entry modificationTime: (entryArray at: 3).
				entry fileSize: (entryArray at: 5).
				parentEntryOrNil ifNotNil: [
					entry parent: parentEntryOrNil ]
				ifNil: [	"Tratar de arreglar lo que dice (horrible) el comentario de este metodo"
					entry pathName: entry name ].
				entries addLast: entry ].
		index := index + 1].

	^entries asArray! !

!FmFileIOAccessor methodsFor: 'accessing' stamp: 'jmv 10/20/2015 15:09'!
drives
	"
	Answer a collection of Strings
	FmFileIOAccessor default drives
	"
	drives _ nil. 		"will change if you mount or unmount drives!!"
	drives ifNil: [
		drives _ self onUnix
			ifTrue: [ #() ]
			ifFalse: [ (self entriesIn: '' from: nil)]].
	^drives! !


!FileDirectoryWrapper methodsFor: 'as yet unclassified' stamp: 'jmv 10/20/2015 14:59'!
contents

	^((model directoryNamesFor: item) asArray sort: [ :a :b | a caseInsensitiveLessOrEqual: b]) collect: [ :n | 
		FileDirectoryWrapper with: item / n name: n model: self ]! !

!methodRemoval: FmDirectoryEntry #/!
FmDirectoryEntry removeSelector: #/!
!methodRemoval: FmDirectoryEntry #allChildrenDo:filesMatches:directoryMatches:!
FmDirectoryEntry removeSelector: #allChildrenDo:filesMatches:directoryMatches:!
!methodRemoval: FmDirectoryEntry #asDirectoryEntry!
FmDirectoryEntry removeSelector: #asDirectoryEntry!
!methodRemoval: FmDirectoryEntry #asFileEntry!
FmDirectoryEntry removeSelector: #asFileEntry!
!methodRemoval: FmDirectoryEntry #ensureAsDirectory!
FmDirectoryEntry removeSelector: #ensureAsDirectory!
!methodRemoval: FmFileEntry class #entry:!
FmFileEntry class removeSelector: #entry:!
!methodRemoval: FmFileEntry #allChildrenDo:filesMatches:directoryMatches:!
FmFileEntry removeSelector: #allChildrenDo:filesMatches:directoryMatches:!
!methodRemoval: FmFileEntry #as:!
FmFileEntry removeSelector: #as:!
!methodRemoval: FmFileEntry #asDirectoryEntry!
FmFileEntry removeSelector: #asDirectoryEntry!
!methodRemoval: FmFileEntry #asDirectoryEntry:!
FmFileEntry removeSelector: #asDirectoryEntry:!
!methodRemoval: FmFileEntry #asFileEntry!
FmFileEntry removeSelector: #asFileEntry!
!methodRemoval: FmFileEntry #asFileEntry:!
FmFileEntry removeSelector: #asFileEntry:!
!methodRemoval: FmFileEntry #ensureAsDirectory!
FmFileEntry removeSelector: #ensureAsDirectory!
!methodRemoval: FmFileEntry #isDirectory:!
FmFileEntry removeSelector: #isDirectory:!
!methodRemoval: FmFileEntry #isFileEntry!
FmFileEntry removeSelector: #isFileEntry!
!classDefinition: #FmFileEntry category: #'FileMan-Core'!
Object subclass: #FmFileEntry
	instanceVariableNames: 'drive pathComponents parent name fileSize creationTime modificationTime options'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FileMan-Core'!
!methodRemoval: FileDirectory #asDirectoryEntry!
FileDirectory removeSelector: #asDirectoryEntry!
!methodRemoval: String #asDirectoryEntry:!
String removeSelector: #asDirectoryEntry:!
!methodRemoval: String #asFileEntry:!
String removeSelector: #asFileEntry:!
!methodRemoval: ByteArray #fileContents!
ByteArray removeSelector: #fileContents!
!methodRemoval: ByteArray #fileContents:!
ByteArray removeSelector: #fileContents:!
!methodRemoval: Object #asFileEntry!
Object removeSelector: #asFileEntry!
!methodRemoval: Object #isFileEntry!
Object removeSelector: #isFileEntry!
