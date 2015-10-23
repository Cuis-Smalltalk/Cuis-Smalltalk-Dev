'From Cuis 4.2 of 25 July 2013 [latest update: #2542] on 20 October 2015 at 3:55:50.461519 pm'!

!FmFileIOAccessor methodsFor: 'private' stamp: 'jmv 10/20/2015 15:54'!
entriesIn: parentEntryOrNil
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
		entryArray := self primLookupEntryIn: (parentEntryOrNil ifNil: [''] ifNotNil: [parentEntryOrNil pathName]) index: index.
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


!ContentPack methodsFor: 'exporting' stamp: 'jmv 10/20/2015 15:49'!
export

	"Answer true on success."

	"self break."

	self exportDirectory exists
		ifTrue: [ Utilities inform: 
			'Before you can export, you must move, rename or delete this directory: ' ,
			self exportDirectory fullName.
			
			^ false.
		].
	
	self exportInto: self exportDirectory.
	
	^ true! !


!FmFileEntry methodsFor: 'actions-file' stamp: 'jmv 10/20/2015 15:54'!
copyTo: filename 
	| targetEntry |
	(self asDirectoryEntry exists) ifTrue: [
		
		self error: 'no'.
		^self asDirectoryEntry copyTo: filename ].
	
	targetEntry := filename asFileEntry.
	targetEntry isDirectory
		ifTrue: [targetEntry := (targetEntry / self name) ensureAsFile].
	self assureExistence.
	targetEntry assureExistence.
	self fileAccessor copy: self to: targetEntry! !

!FmFileEntry methodsFor: 'testing' stamp: 'jmv 10/20/2015 15:55'!
exists

	| fileNames |
	fileNames := self fileAccessor fileNamesIn: self parent.
	^self fileAccessor isCaseSensitive 
		ifTrue: [ fileNames includes: self name ]
		ifFalse: [ fileNames anySatisfy: [ :n | n sameAs: self name ]]! !


!FmDirectoryEntry methodsFor: 'actions-directory' stamp: 'jmv 10/20/2015 15:39'!
copyTo: filename 
	
	| toDir |
	filename asFileEntry exists ifTrue: [^self error: 'Cannot copy directory to file'].
	
	toDir := filename asDirectoryEntry assureExistence.

	self
		filesDo: [:file | file copyTo: (toDir / file name)].
	
	self
		directoriesDo: [ :dir |
			dir copyTo:  dir name ]! !

!FmDirectoryEntry methodsFor: 'private' stamp: 'jmv 10/20/2015 15:33'!
assureExistenceFileNamed: localName
	
	localName isEmpty ifTrue: [ ^self ]. "Assumed to exist"
	(self fileAccessor fileOrDirectoryExists: localName in: self) ifTrue: [^ self]. "exists"

	"otherwise check parent first and then create local dir"
	self parent ifNotNil: [:p | p assureExistenceFileNamed: self name].

	self fileAccessor createDirectory: (self / localName) pathName! !

!FmDirectoryEntry methodsFor: 'private' stamp: 'jmv 10/20/2015 15:48'!
initChildren

	self exists ifFalse: [ ^children _ #()] .
	children _ self fileAccessor entriesIn: self.
	^children! !

!FmDirectoryEntry methodsFor: 'testing' stamp: 'jmv 10/20/2015 15:55'!
exists
	
	| dirNames pathName |
	(self isDriveSupported and: [self pathComponents isEmpty])
		 ifTrue: [^self fileAccessor drives includes: self ].

	self isRoot ifTrue: [^true].
	
	self fileAccessor onUnix ifFalse: [
		pathName _ self pathName.
		pathName = self fileAccessor slash ifTrue: [ ^ true ].
		^self fileAccessor basicDirectoryExists: self pathName]. 

	dirNames := self fileAccessor directoryNamesIn: self parent.

	^self fileAccessor isCaseSensitive 
		ifTrue: [ dirNames includes: self name ]
		ifFalse: [ dirNames anySatisfy: [ :n | n sameAs: self name ]]! !


!FmFileIOAccessor methodsFor: 'actions' stamp: 'jmv 10/20/2015 15:32'!
fileOrDirectoryExists: localName in: anFmDirectoryEntry

	| entryNames |
	entryNames := self entryNamesIn: anFmDirectoryEntry.

	^self isCaseSensitive 
		ifTrue:[entryNames includes: localName]
		ifFalse:[entryNames anySatisfy: [:name| name sameAs: localName]].! !

!FmFileIOAccessor methodsFor: 'actions' stamp: 'jmv 10/20/2015 15:38'!
rename: oldFileFullName to: newFileFullName 
	| selection |
	(self try: [self primRename: oldFileFullName to: newFileFullName]
			forFileNamed: oldFileFullName) ifTrue: [^ self].

	oldFileFullName asFileEntry exists ifFalse: [^ self error: 'Attempt to rename a non-existent file'].
	(newFileFullName asFileEntry exists or: [ newFileFullName asDirectoryEntry exists ])
		ifTrue: [
			selection := (PopUpMenu labels: 'delete old version
cancel')
						startUpWithCaption: 'Trying to rename a file to be
' , newFileFullName , '
and it already exists.'.
			selection = 1
				ifTrue: [self deleteFile: newFileFullName.
					^ self rename: oldFileFullName to: newFileFullName]].
	^ self error: 'Failed to rename file'! !

!FmFileIOAccessor methodsFor: 'actions' stamp: 'jmv 10/20/2015 15:50'!
renameDirectory: oldFileFullName to: newFileFullName 
	| selection |
	(self try: [self primRename: oldFileFullName to: newFileFullName]
			forFileNamed: oldFileFullName) ifTrue: [^ self].

	oldFileFullName asDirectoryEntry exists ifFalse: [^ self error: 'Attempt to rename a non-existent file'].
	newFileFullName asDirectoryEntry exists
		ifTrue: [selection := (PopUpMenu labels: 'delete old version
cancel')
						startUpWithCaption: 'Trying to rename a directory to be
' , newFileFullName , '
and it already exists.'.
			selection = 1
				ifTrue: [newFileFullName asDirectoryEntry recursiveDelete.
					^ self renameDirectory: oldFileFullName to: newFileFullName]].
	^ self error: 'Failed to rename file'! !

!FmFileIOAccessor methodsFor: 'private' stamp: 'jmv 10/20/2015 15:54'!
basicDirectoryExists: fullPathName

	| result |
	result := self primLookupEntryIn: fullPathName index: 1.
 	^result ~= #badDirectoryPath! !

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'jmv 10/20/2015 15:48'!
directoryNamesIn: anFmDirectoryEntry
	"
	FmFileIOAccessor default directoryNamesIn: 'C:\Windows' asDirectoryEntry
	
FmFileIOAccessor default directoryNamesIn: '' asDirectoryEntry
	Linux: Ojo. Esto en Unix NO VA AL ROOT. (RECHEQUEAR)
		Equivale a 
		FmFileIOAccessor default directoryNamesIn: '.' asDirectoryEntry
			Confirmado. Va a 	FmDirectoryEntry currentDirectory
	Windows
		 #('C:' 'D:')
	Mac
		?
	"
	
	^(self entriesIn: anFmDirectoryEntry)
		select: [ :each | each isDirectory]
		thenCollect: [ :each | each name]! !

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'jmv 10/20/2015 15:48'!
entryNamesIn: anFmDirectoryEntry
	"
	FmFileIOAccessor default entryNamesIn: 'C:\Windows\' asDirectoryEntry
	"
	
	^(self entriesIn: anFmDirectoryEntry) collect: [ :each | each name]! !

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'jmv 10/20/2015 15:48'!
fileNamesIn: anFmDirectoryEntry
	"
	FmFileIOAccessor default fileNamesIn: 'C:\Windows' asDirectoryEntry
	"
	
	^((self entriesIn: anFmDirectoryEntry)
		reject: [ :each | each isDirectory ])
		collect: [ :each | each name ]! !

!FmFileIOAccessor methodsFor: 'accessing' stamp: 'jmv 10/20/2015 15:48'!
drives
	"
	Answer a collection of Strings
	FmFileIOAccessor default drives
	"
	drives _ nil. 		"will change if you mount or unmount drives!!"
	drives ifNil: [
		drives _ self onUnix
			ifTrue: [ #() ]
			ifFalse: [ (self entriesIn: nil)]].
	^drives! !

!methodRemoval: FmFileIOAccessor #directoryExists:!
FmFileIOAccessor removeSelector: #directoryExists:!
!methodRemoval: FmFileIOAccessor #directoryExists:in:!
FmFileIOAccessor removeSelector: #directoryExists:in:!
!methodRemoval: FmFileIOAccessor #entriesIn:from:!
FmFileIOAccessor removeSelector: #entriesIn:from:!
!methodRemoval: FmFileIOAccessor #fileExists:!
FmFileIOAccessor removeSelector: #fileExists:!
!methodRemoval: FmFileIOAccessor #fileExists:in:!
FmFileIOAccessor removeSelector: #fileExists:in:!
!methodRemoval: FmFileIOAccessor #lookupEntryIn:index:!
FmFileIOAccessor removeSelector: #lookupEntryIn:index:!
