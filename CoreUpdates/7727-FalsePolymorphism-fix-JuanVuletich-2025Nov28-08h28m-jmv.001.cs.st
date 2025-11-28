'From Cuis7.5 [latest update: #7725] on 28 November 2025 at 8:34:12 am'!

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 28/Nov/2025 07:56:15'!
writeSystemTimeStamp: aStream 
	"Writes system version and current time on stream aStream."

	| dateTime |
	dateTime := DateAndTime now.
	aStream
		nextPutAll: 'From ';
		nextPutAll: self versionString;
		nextPutAll: ' [';
		nextPutAll: self lastUpdateString;
		nextPutAll: '] on '.
	dateTime date printOn: aStream.
	aStream
		nextPutAll: ' at '.
	dateTime time print24: false showSeconds: true on: aStream! !


!WriteStream methodsFor: 'fileIn/Out' stamp: 'jmv 28/Nov/2025 07:56:31'!
writeSystemTimeStamp
	"Append the current time to the receiver as a String."
	self nextChunkPut:	"double string quotes and !!s"
		(self collectionSpecies streamContents: [:s | Smalltalk writeSystemTimeStamp: s]) printString.
	self newLine! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 28/Nov/2025 07:56:42'!
fileOutCategory: catName
	"FileOut the named category"

	DirectoryEntry fileOutsDirQuery: (self name , '-' , catName , '.st') writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self fileOutCategory: catName on: fileStream moveSource: false toFile: 0 ].! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 28/Nov/2025 07:56:46'!
fileOutMethod: selector
	"Write source code of a single method on a file.  Make up a name for the file."

	| nameBody |
	(selector == #Comment) ifTrue: [^ self inform: 'Sorry, cannot file out class comment in isolation.'].
	(self includesSelector: selector) ifFalse: [^ self error: 'Selector ', selector asPlainString, ' not found'].
	nameBody := self name , '-' , (selector copyReplaceAll: ':' with: '').
	DirectoryEntry fileOutsDirQuery: (nameBody asFileName, '.st') writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self printMethodChunk: selector withPreamble: true
			on: fileStream moveSource: false toFile: 0 ].! !


!Class methodsFor: 'fileIn/Out' stamp: 'jmv 28/Nov/2025 07:56:50'!
fileOut
	"File a description of the receiver onto a new file whose base name is the name of the receiver.
	Method ordering is by method categories, and in each category, alphabetical."

	DirectoryEntry fileOutsDirQuery: (self name, '.st') writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self sharedPools size > 0 ifTrue: [
			self shouldFileOutPools
				ifTrue: [ self fileOutSharedPoolsOn: fileStream ]].
		self fileOutOn: fileStream moveSource: false toFile: 0 ].! !

!Class methodsFor: 'fileIn/Out' stamp: 'jmv 28/Nov/2025 07:56:53'!
fileOutAlphabetically
	"File a description of the receiver onto a new file whose base name is the name of the receiver.
	Method ordering is alphabetical, igoring categories."

	DirectoryEntry fileOutsDirQuery: (self name, '.st') writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self sharedPools size > 0 ifTrue: [
			self shouldFileOutPools
				ifTrue: [ self fileOutSharedPoolsOn: fileStream ]].
		self fileOutOn: fileStream moveSource: false toFile: 0 initializing: true sortMethodsByCategory: false ].! !

!Class methodsFor: 'fileIn/Out' stamp: 'jmv 28/Nov/2025 07:56:56'!
fileOutHierarchy
	"File a description of the receiver onto a new file whose base name is the name of the receiver.
	Morph fileOutHierarchy
	"
	DirectoryEntry fileOutsDirQuery: (self name, '-hierarchy.st') writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self sharedPools size > 0 ifTrue: [
			self shouldFileOutPools
				ifTrue: [ self fileOutSharedPoolsOn: fileStream ]].
		self withAllSubclassesDo: [ :each |
			each fileOutOn: fileStream moveSource: false toFile: 0 ]].! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 28/Nov/2025 08:00:41'!
condenseChanges
	"Move all the changes onto a compacted sources file."
	"
	Smalltalk condenseChanges
	"

	| oldChanges classCount oldChangesLocalName oldChangesPathName |
	DirectoryEntry smalltalkImageDirectory // 'ST80.temp' forceWriteStreamDo: [ :f |
		f writeSystemTimeStamp.
		'Condensing Changes File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: self classNames size
			during: [ :barBlock |
				classCount := 0.
				self allClassesDo: [ :class | 
					barBlock value: (classCount := classCount + 1).
					class moveChangesTo: f.
					class putClassCommentToCondensedChangesFile: f.
					class class moveChangesTo: f ]].
		LastQuitLogPosition := f position ].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	oldChanges := SourceFiles at: 2.
	oldChangesPathName := oldChanges name.
	oldChangesLocalName := oldChanges localName.
	oldChanges close.
	(oldChangesPathName, '.old') asFullFileEntry delete.
	oldChangesPathName asFullFileEntry rename: oldChangesLocalName, '.old'.
	DirectoryEntry smalltalkImageDirectory // 'ST80.temp' rename: oldChangesLocalName.
	
	SourceFiles
			at: 2 put: oldChangesPathName asFullFileEntry appendStream.

	self inform: 'Changes file has been rewritten!!

Check that all is well, and then save/quit.
 
Otherwise, remove new changes,
replace it with the former one, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 28/Nov/2025 08:03:05'!
condenseSources: newVersionString
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseSources"

	| classCount oldChanges oldChangesLocalName oldChangesPathName newChangesPathName newSourcesName |
	SourceFileVersionString := newVersionString.

	"Write all sources with fileIndex 1"
	newSourcesName := self defaultSourcesName.
	newSourcesName asFullFileEntry writeStreamDo: [ :f |
		f writeSystemTimeStamp.
		'Condensing Sources File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: self classNames size
			during: [ :barBlock |
				classCount := 0.
				self allClassesDo: [ :class |
					barBlock value: (classCount := classCount + 1).
					class fileOutOn: f moveSource: true toFile: 1]]].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	"Make a new empty changes file"
	oldChanges := SourceFiles at: 2.
	oldChangesPathName := oldChanges name.
	oldChangesLocalName := oldChanges localName.
	self closeSourceFiles.
	oldChangesPathName ifNotNil: [
		(oldChangesPathName, '.old') asFullFileEntry delete.
	oldChangesPathName asFullFileEntry rename: oldChangesLocalName, '.old' ].
	newChangesPathName := self defaultChangesName.
	newChangesPathName asFullFileEntry writeStreamDo: [ :stream |
		stream writeSystemTimeStamp ].
	LastQuitLogPosition := 0.

	self openSourceFiles.! !


!ReadWriteStream methodsFor: 'fileIn/Out' stamp: 'jmv 28/Nov/2025 08:00:50'!
fileOutObject: theObject
	"Write a file that has both the source code for the named class and an object as bits.  Any instance-specific object will get its class written automatically."

	| srefStream |
	self writeSystemTimeStamp.

	"Append the object's raw data"
	srefStream := SmartRefStream on: self.
	srefStream nextPut: theObject.  "and all subobjects"
	srefStream close.		"also closes me"
! !


!CodeProvider methodsFor: 'message list menu' stamp: 'jmv 28/Nov/2025 07:57:00'!
fileOutMessage

	self selectedMessageName ifNotNil: [
		^self selectedClassOrMetaClass fileOutMethod: self selectedMessageName].

	self messageList ifNotNil: [ :theMethods |
		DirectoryEntry fileOutsDirQuery: 'methods.st' writeStreamDo: [ :fileStream |
			fileStream writeSystemTimeStamp.
			theMethods do: [ :methodRef |
				methodRef actualClass
					printMethodChunk: methodRef methodSymbol
					withPreamble: true
					on: fileStream
					moveSource: false
					toFile: 0 ]]].! !


!MethodSet methodsFor: 'message functions' stamp: 'jmv 28/Nov/2025 07:57:03'!
fileOutAllMethods

	DirectoryEntry fileOutsDirQuery: 'methods.st' writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self messageList do: [ :methodRef |
			methodRef actualClass
				printMethodChunk: methodRef methodSymbol
				withPreamble: true
				on: fileStream
				moveSource: false
				toFile: 0 ]].! !


!ChangeList methodsFor: 'menu actions' stamp: 'jmv 28/Nov/2025 07:57:05'!
fileOutCurrentVersionsOfSelections

	DirectoryEntry fileOutsDirQuery: 'CurrentVersions.st' writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self currentVersionsOfSelections do: [ :methodRef |
			methodRef actualClass
				printMethodChunk: methodRef methodSymbol
				withPreamble: true
				on: fileStream
				moveSource: false
				toFile: 0 ]].! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 28/Nov/2025 07:57:08'!
fileOutSelections

	DirectoryEntry fileOutsDirQuery: 'SelectedChanges.st' writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		listSelections with: changeList do: [ :selected :item |
			selected ifTrue: [ item fileOutOn: fileStream ]]].! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 28/Nov/2025 08:01:49'!
save
	"If we can't save, find a new destination directory."
	fullFileName ifNotNil: [
		fullFileName asFullFileEntry parent exists ifFalse: [
			fullFileName := nil ]].

	"If we were never saved, or never saved since image was moved, or target directory disappeared,
	then ask where to save, suggesting the NewPackages folder."
	fullFileName ifNil: [
		fullFileName :=
			self request: 'Save on path: '
				initialAnswer: (DirectoryEntry newPackagesDirectory // self packageFileName) pathName
				orCancel: [ ^self ]].

	fullFileName asFullFileEntry forceWriteStreamDo: [ :stream |
		stream writeSystemTimeStamp.
		self writeOnStream: stream ].

	self hasUnsavedChanges: false.
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: self)! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 28/Nov/2025 07:57:12'!
fileOut
	"File out the receiver, to a file whose name is a function of the  
	change-set name and a unique numeric tag."

	| folder slips nameToUse |
	folder := DirectoryEntry savedChangeSetsDirectory.
	nameToUse := self name.
	nameToUse := nameToUse copyReplaceAll: 'AuthorName' with: Utilities authorName asUnaccented asCamelCase.
	nameToUse := folder nextNameFor: nameToUse coda: '-', Utilities authorInitials extension: 'cs.st'.

	folder // nameToUse writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			fileStream writeSystemTimeStamp.
			self fileOutPreambleOn: fileStream.
			self fileOutOn: fileStream.
			self fileOutPostscriptOn: fileStream ]].

	self hasUnsavedChanges: false.
	(Preferences at: #checkForSlips) 		ifFalse: [^ self].
	slips := self checkForSlips.
	(slips notEmpty
			and: [(PopUpMenu withCaption: 'Methods in this fileOut have halts
or references to the Transcript
or other ''slips'' in them.
Would you like to browse them?' chooseFrom: 'Ignore\Browse slips')
					= 2])
		ifTrue: [ Smalltalk browseMessageList: slips name: 'Possible slips in ' , name ].! !


!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 28/Nov/2025 08:02:41'!
printSpaceAnalysis: threshold on: aStream
	"Warning!!
	This is most likely wrong. It was written for 32 bit V3 images.
	Please take a look at #spaceForInstancesOf:
	Review both methods, and any other one that might be related.
	"
	"
	SpaceTally new printSpaceAnalysis: 1 on:(FileStream forceNewFileNamed: 'STspace.text')
	"
	"sd-This method should be rewrote to be more coherent within the rest of the class 
	ie using preAllocate and spaceForInstanceOf:"

	"If threshold > 0, then only those classes with more than that number
	of instances will be shown, and they will be sorted by total instance space.
	If threshold = 0, then all classes will appear, sorted by name."

	| codeSpace instCount instSpace totalCodeSpace totalInstCount totalInstSpace eltSize n totalPercent percent |
	Smalltalk garbageCollect.
	totalCodeSpace := totalInstCount := totalInstSpace := n := 0.
	results := OrderedCollection new.
	'Taking statistics...'
		displayProgressAt: Sensor mousePoint
		from: 0 to: Smalltalk classNames size
		during: [ :barBlock |
			Smalltalk allClassesDo: [ :cl |
				codeSpace := cl spaceUsed.
				barBlock value: (n := n+1).
				Smalltalk garbageCollectMost.
				instCount := cl instanceCount.
				instSpace := 8 * instCount. "Object headers""Warning: The 3rd header word for big objects is not considered!!"
				cl isVariable
					ifTrue: [
						eltSize := cl bytesPerBasicElement.
						cl allInstancesDo: [ :x |
							instSpace := instSpace + (x basicSize * eltSize)]]
					ifFalse: [instSpace := instSpace + (cl instSize * instCount * 4)].
				results add: (SpaceTallyItem analyzedClassName: cl name codeSize: codeSpace instanceCount:  instCount spaceForInstances: instSpace).
				totalCodeSpace := totalCodeSpace + codeSpace.
				totalInstCount := totalInstCount + instCount.
				totalInstSpace := totalInstSpace + instSpace]].
	totalPercent := 0.0.

	aStream writeSystemTimeStamp.
	aStream
		nextPutAll: ('Class' padded: #right to: 30 with: $ );
		nextPutAll: ('code space' padded: #left to: 12 with: $ );
		nextPutAll: ('# instances' padded: #left to: 12 with: $ );
		nextPutAll: ('inst space' padded: #left to: 12 with: $ );
		nextPutAll: ('percent' padded: #left to: 8 with: $ ); newLine.

	threshold > 0 ifTrue: [
		"If inst count threshold > 0, then sort by space"
		results := (results select: [:s | s instanceCount >= threshold or: [s spaceForInstances > (totalInstSpace // 500)]])
			asArray sort: [:s :s2 | s spaceForInstances > s2 spaceForInstances]].

	results do: [:s |
		aStream
			nextPutAll: (s analyzedClassName padded: #right to: 30 with: $ );
			nextPutAll: (s codeSize printString padded: #left to: 12 with: $ );
			nextPutAll: (s instanceCount printString padded: #left to: 12 with: $ );
			nextPutAll: (s spaceForInstances printString padded: #left to: 14 with: $ ).
		percent := s spaceForInstances*100.0/totalInstSpace.
		totalPercent := totalPercent + percent.
		percent >= 0.1 ifTrue: [
			percent printOn: aStream integerDigits: 6 padWith: $  fractionDigits: 1 positiveIndicator: nil ].
		aStream newLine].

	aStream
		newLine; nextPutAll: ('Total' padded: #right to: 30 with: $ );
		nextPutAll: (totalCodeSpace printString padded: #left to: 12 with: $ );
		nextPutAll: (totalInstCount printString padded: #left to: 12 with: $ );
		nextPutAll: (totalInstSpace printString padded: #left to: 14 with: $ ).
	totalPercent printOn: aStream integerDigits: 6 padWith: $  fractionDigits: 1 positiveIndicator: nil! !

!methodRemoval: WriteStream #timeStamp stamp: 'jmv 28/Nov/2025 08:33:21'!
WriteStream removeSelector: #timeStamp!
!methodRemoval: SystemDictionary #timeStamp: stamp: 'jmv 28/Nov/2025 08:30:06'!
SystemDictionary removeSelector: #timeStamp:!
