'From Cuis 5.0 of 7 November 2016 [latest update: #3383] on 27 July 2018 at 9:01:05 pm'!
'Description '!
!provides: 'Goodies' 1 9!
SystemOrganization addCategory: #'Goodies-AsyncFile'!
SystemOrganization addCategory: #'Goodies-Collections'!
SystemOrganization addCategory: #'Goodies-Tools-FileList'!
SystemOrganization addCategory: #'Goodies-Tools-Profiling'!
SystemOrganization addCategory: #'Goodies-Kernel-Chronology'!


!classDefinition: #MappedCollection category: #'Goodies-Collections'!
SequenceableCollection subclass: #MappedCollection
	instanceVariableNames: 'domain map'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Goodies-Collections'!
!classDefinition: 'MappedCollection class' category: #'Goodies-Collections'!
MappedCollection class
	instanceVariableNames: ''!

!classDefinition: #SkipList category: #'Goodies-Collections'!
Collection subclass: #SkipList
	instanceVariableNames: 'sortBlock pointers numElements level splice'
	classVariableNames: 'Rand'
	poolDictionaries: ''
	category: 'Goodies-Collections'!
!classDefinition: 'SkipList class' category: #'Goodies-Collections'!
SkipList class
	instanceVariableNames: ''!

!classDefinition: #IdentitySkipList category: #'Goodies-Collections'!
SkipList subclass: #IdentitySkipList
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Goodies-Collections'!
!classDefinition: 'IdentitySkipList class' category: #'Goodies-Collections'!
IdentitySkipList class
	instanceVariableNames: ''!

!classDefinition: #StandardFileMenu category: #'Goodies-Tools-FileList'!
SelectionMenu subclass: #StandardFileMenu
	instanceVariableNames: 'canTypeFileName pattern'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Goodies-Tools-FileList'!
!classDefinition: 'StandardFileMenu class' category: #'Goodies-Tools-FileList'!
StandardFileMenu class
	instanceVariableNames: ''!

!classDefinition: #AsyncFile category: #'Goodies-AsyncFile'!
Object subclass: #AsyncFile
	instanceVariableNames: 'name writeable semaphore fileHandle'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Goodies-AsyncFile'!
!classDefinition: 'AsyncFile class' category: #'Goodies-AsyncFile'!
AsyncFile class
	instanceVariableNames: ''!

!classDefinition: #SkipListNode category: #'Goodies-Collections'!
Object subclass: #SkipListNode
	instanceVariableNames: 'pointers object'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Goodies-Collections'!
!classDefinition: 'SkipListNode class' category: #'Goodies-Collections'!
SkipListNode class
	instanceVariableNames: ''!

!classDefinition: #StandardFileMenuResult category: #'Goodies-Tools-FileList'!
Object subclass: #StandardFileMenuResult
	instanceVariableNames: 'directory name'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Goodies-Tools-FileList'!
!classDefinition: 'StandardFileMenuResult class' category: #'Goodies-Tools-FileList'!
StandardFileMenuResult class
	instanceVariableNames: ''!

!classDefinition: #ProtocolCatcher2 category: #'Goodies-Tools-Profiling'!
Object subclass: #ProtocolCatcher2
	instanceVariableNames: 'realObject'
	classVariableNames: 'Protocols Started'
	poolDictionaries: ''
	category: 'Goodies-Tools-Profiling'!
!classDefinition: 'ProtocolCatcher2 class' category: #'Goodies-Tools-Profiling'!
ProtocolCatcher2 class
	instanceVariableNames: ''!

!classDefinition: #Stopwatch category: #'Goodies-Kernel-Chronology'!
Object subclass: #Stopwatch
	instanceVariableNames: 'timespans state'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Goodies-Kernel-Chronology'!
!classDefinition: 'Stopwatch class' category: #'Goodies-Kernel-Chronology'!
Stopwatch class
	instanceVariableNames: ''!

!classDefinition: #ProtocolCatcher category: #'Goodies-Tools-Profiling'!
ProtoObject subclass: #ProtocolCatcher
	instanceVariableNames: 'realObject'
	classVariableNames: 'Protocols Started'
	poolDictionaries: ''
	category: 'Goodies-Tools-Profiling'!
!classDefinition: 'ProtocolCatcher class' category: #'Goodies-Tools-Profiling'!
ProtocolCatcher class
	instanceVariableNames: ''!


!MappedCollection commentStamp: '<historical>' prior: 0!
I represent an access mechanism for a sequencable collection re-ordering or filtering its elements.!

!SkipList commentStamp: 'KLC 2/26/2004 12:04' prior: 0!
From "Skip Lists: A Probabilistic Alternative to Balanced Trees" by William Pugh ( http://epaperpress.com/sortsearch/download/skiplist.pdf ):

"Skip lists are a data structure that can be used in place of balanced trees.  Skip lists use probabilistic balancing rather than strictly enforcing balancing and as a result the algorithms for insertion and deletion in skip lists are much simpler and significantly faster than equivalent algorithms for balanced trees."

Notes:

The elements of the skip list must implement #< or you must provide a sort block.

!

!IdentitySkipList commentStamp: '<historical>' prior: 0!
Like a SkipList, except that elements are compared with #== instead of #= .

See the comment of IdentitySet for more information.
!

!StandardFileMenu commentStamp: 'jmv 5/31/2016 10:37' prior: 0!
I represent a SelectionMenu which operates like a modal dialog for selecting files, somewhat similar to the StandardFile dialogs in MacOS and Java Swing.

Try for example, the following:

	(StandardFileMenu oldFileFrom: DirectoryEntry smalltalkImageDirectory) inspect
	((StandardFileMenu new newFileFrom: DirectoryEntry smalltalkImageDirectory withPattern: '*.st') startUpWithCaption: 'dale') inspect!

!AsyncFile commentStamp: '<historical>' prior: 0!
An asynchronous file allows simple file read and write operations to be performed in parallel with other processing. This is useful in multimedia applications that need to stream large amounts of sound or image data from or to a file while doing other work.
!

!ProtocolCatcher2 commentStamp: 'jmv 11/3/2008 14:49' prior: 0!
See class comment in ProtocolCatcher.

The only difference between ProtocolCatcher and ProtocolCatcher2 is that this is subclass of Object. This means it will not catch protocol inherited from Object. This is not usually a problem, and inspectors are more comfortable to use if the catcher is not that transparent.!

!Stopwatch commentStamp: '<historical>' prior: 0!
A Stopwatch maintains a collection of timespans.!

!ProtocolCatcher commentStamp: '<historical>' prior: 0!
Protocol catchers use #doesNotUnderstand: to detect the protocol actually used for classes under study. The subject of study is classes, not particular objects. And all instances of ProtocolCatcher work as a team. The captured protocols are stored in a class variable. In this way, you can do the following, anytime, without worrying of keeping track of each instance of ProtocolCatcher:

"Standard way to change existing instances to catchers: As ivar realObject is initialized with self, after the #become: it points to the observed object, and it is the solely reference to it (at that moment!!)"
| objects catchers |
ProtocolCatcher clear.
objects _ ParagraphEditor allSubInstances asArray.
catchers _ Array new: objects size.
1 to: catchers size do: [ :i |
	catchers at: i put: ProtocolCatcher new ].
objects elementsExchangeIdentityWith: catchers

The other way to use it is to replace 'ObservedClass new' to 'ProtocolCatcher on: ObservedClass new'. In this pattern, as there is no #become:, we must set the observed object explicitly.

After that you can use:

ProtocolCatcher clear
ProtocolCatcher start
ProtocolCatcher stop
ProtocolCatcher observedClasses
ProtocolCatcher protocolFor: ParagraphEditor

Finally, this class also serves as an example on how to do a ProtocolCatcher. Sometimes, when replacing an existing class in the system, it is useful to know the actually used protocol. But it can also be useful to keep the forwarding mechanism available after the new class has been installed. This way it is easier to add behavior that is still missing.!

!MappedCollection methodsFor: 'adding'!
add: newObject

	self shouldNotImplement! !

!MappedCollection methodsFor: 'accessing'!
at: anIndex

	^domain at: (map at: anIndex)! !

!MappedCollection methodsFor: 'accessing'!
at: anIndex put: anObject

	^domain at: (map at: anIndex) put: anObject! !

!MappedCollection methodsFor: 'accessing'!
contents
	"Answer the receiver's domain for mapping, a Dictionary or 
	SequenceableCollection."

	^map collect: [:mappedIndex | domain at: mappedIndex]! !

!MappedCollection methodsFor: 'accessing'!
size

	^map size! !

!MappedCollection methodsFor: 'enumerating'!
collect: aBlock 
	"Refer to the comment in Collection|collect:."

	| aStream |
	aStream _ WriteStream on: (self species new: self size).
	self do:
		[:domainValue | 
		aStream nextPut: (aBlock value: domainValue)].
	^aStream contents! !

!MappedCollection methodsFor: 'enumerating'!
do: aBlock 
	"Refer to the comment in Collection|do:."

	map do:
		[:mapValue | aBlock value: (domain at: mapValue)]! !

!MappedCollection methodsFor: 'enumerating'!
select: aBlock 
	"Refer to the comment in Collection|select:."

	| aStream |
	aStream _ WriteStream on: (self species new: self size).
	self do:
		[:domainValue | 
		(aBlock value: domainValue)
			ifTrue: [aStream nextPut: domainValue]].
	^aStream contents! !

!MappedCollection methodsFor: 'private'!
setCollection: aCollection map: aDictionary

	domain _ aCollection.
	map _ aDictionary! !

!MappedCollection methodsFor: 'private'!
species

	^domain species! !

!MappedCollection methodsFor: 'printing' stamp: 'sma 5/11/2000 19:53'!
storeOn: aStream
	aStream
		nextPut: $(;
		nextPutAll: self class name;
		nextPutAll: ' collection: ';
		store: domain;
		nextPutAll: ' map: ';
		store: map;
		nextPut: $)! !

!MappedCollection class methodsFor: 'instance creation'!
collection: aCollection map: aSequenceableCollection 
	"Answer an instance of me that maps aCollection by 
	aSequenceableCollection."

	^self basicNew setCollection: aCollection map: aSequenceableCollection! !

!MappedCollection class methodsFor: 'instance creation'!
new

	self error: 'MappedCollections must be created using the collection:map: message'! !

!MappedCollection class methodsFor: 'instance creation'!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."

	^ self collection: aCollection map: (1 to: aCollection size)

"	MappedCollection newFrom: {1. 2. 3}
	{4. 3. 8} as: MappedCollection
"! !

!SkipList methodsFor: 'adding' stamp: 'LC 6/18/2001 18:30'!
add: element 
	self add: element ifPresent: nil.
	^ element! !

!SkipList methodsFor: 'adding' stamp: 'LC 6/18/2001 20:42'!
add: element ifPresent: aBlock
	| node lvl s |
	node _ self search: element updating: splice.
	node ifNotNil: [aBlock ifNotNil: [^ aBlock value: node]].
	lvl _ self randomLevel.
	node _ SkipListNode on: element level: lvl.
	level + 1 to: lvl do: [:i | splice at: i put: self].
	1 to: lvl do: [:i |
				s _ splice at: i.
				node atForward: i put: (s forward: i).
				s atForward: i put: node].
	numElements _ numElements + 1.
	splice atAllPut: nil.
	^ element
! !

!SkipList methodsFor: 'private' stamp: 'LC 6/18/2001 19:26'!
atForward: i put: node
	level _ node
		ifNil: [pointers findLast: [:n | n notNil]]
		ifNotNil: [level max: i].
	^ pointers at: i put: node! !

!SkipList methodsFor: 'private' stamp: 'LC 6/18/2001 11:21'!
forward: i 
	^ pointers at: i! !

!SkipList methodsFor: 'private' stamp: 'LC 6/18/2001 20:14'!
is: node before: element 
	| object |
	node ifNil: [^ false].
	object _ node object.
	^ sortBlock
		ifNil: [object < element]
		ifNotNil: [(self is: object equalTo: element) ifTrue: [^ false].
			sortBlock value: object value: element]! !

!SkipList methodsFor: 'private' stamp: 'LC 6/18/2001 13:19'!
is: node theNodeFor: element 
	node ifNil: [^ false].
	node == self ifTrue: [^ false].
	^ self is: node object equalTo: element! !

!SkipList methodsFor: 'private' stamp: 'LC 6/18/2001 16:15'!
next
	^ pointers first! !

!SkipList methodsFor: 'private' stamp: 'LC 6/18/2001 15:37'!
randomLevel
	| p answer max |
	p _ 0.5.
	answer _ 1.
	max _ self maxLevel.
	[Rand next < p and: [answer < max]]
		whileTrue: [answer _ answer + 1].
	^ answer! !

!SkipList methodsFor: 'private' stamp: 'LC 6/18/2001 19:28'!
search: element updating: array
	| node forward |
	node _ self.
	level to: 1 by: -1 do: [:i |
			[forward _ node forward: i.
			self is: forward before: element] whileTrue: [node _ forward].
			"At this point: node < element <= forward"
			array ifNotNil: [array at: i put: node]].
	node _ node next.
	^ (self is: node theNodeFor: element) ifTrue: [node]! !

!SkipList methodsFor: 'enumerating' stamp: 'LC 6/18/2001 15:39'!
do: aBlock
	self nodesDo: [:node | aBlock value: node object]! !

!SkipList methodsFor: 'testing' stamp: 'LC 6/18/2001 16:59'!
includes: element
	^ (self search: element updating: nil) notNil! !

!SkipList methodsFor: 'testing' stamp: 'LC 6/18/2001 12:53'!
isEmpty
	^ numElements = 0! !

!SkipList methodsFor: 'initialization' stamp: 'LC 6/18/2001 20:08'!
initialize: maxLevel
	pointers _ Array new: maxLevel.
	splice _ Array new: maxLevel.
	numElements _ 0.
	level _ 0.
	Rand ifNil: [Rand _ Random new]! !

!SkipList methodsFor: 'element comparison' stamp: 'LC 6/18/2001 10:14'!
is: element1 equalTo: element2
	^ element1 = element2! !

!SkipList methodsFor: 'accessing' stamp: 'LC 6/18/2001 19:22'!
level
	^ level! !

!SkipList methodsFor: 'accessing' stamp: 'LC 6/17/2001 12:05'!
maxLevel
	^ pointers size! !

!SkipList methodsFor: 'accessing' stamp: 'LC 6/18/2001 20:18'!
maxLevel: n
	| newLevel oldPointers |
	newLevel _ n max: level.
	oldPointers _ pointers.
	pointers _ Array new: newLevel.
	splice _ Array new: newLevel.
	1 to: level do: [:i | pointers at: i put: (oldPointers at: i)]
! !

!SkipList methodsFor: 'accessing' stamp: 'LC 6/18/2001 15:40'!
size
	^ numElements! !

!SkipList methodsFor: 'accessing' stamp: 'LC 6/18/2001 20:19'!
sortBlock
	^ sortBlock! !

!SkipList methodsFor: 'accessing' stamp: 'LC 6/18/2001 17:30'!
sortBlock: aBlock
	sortBlock _ aBlock! !

!SkipList methodsFor: 'node enumeration' stamp: 'LC 6/18/2001 19:30'!
nodesDo: aBlock
	| node |
	node _ pointers first.
	[node notNil]
		whileTrue:
			[aBlock value: node.
			node _ node next]! !

!SkipList methodsFor: 'removing' stamp: 'LC 6/18/2001 20:42'!
remove: element ifAbsent: aBlock
	| node i s |
	node _ self search: element updating: splice.
	node ifNil: [^ aBlock value].
	i _ 1.
	[s _ splice at: i.
	i <= level and: [(s forward: i) == node]]
				whileTrue:
					[s atForward: i put: (node forward: i).
					i _ i + 1].
	numElements _ numElements - 1.
	splice atAllPut: nil.
	^ node object
! !

!SkipList methodsFor: 'removing' stamp: 'LC 6/18/2001 20:25'!
removeAll
	pointers atAllPut: nil.
	splice atAllPut: nil.
	numElements _ 0.
	level _ 0.! !

!SkipList class methodsFor: 'instance creation' stamp: 'LC 6/18/2001 17:33'!
maxLevel: maxLevel
	"
	SkipList maxLevel: 5
	"
	^ super new initialize: maxLevel! !

!SkipList class methodsFor: 'instance creation' stamp: 'LC 6/18/2001 17:34'!
maxLevel: anInteger sortBlock: aBlock
	^ (self maxLevel: anInteger) sortBlock: aBlock! !

!SkipList class methodsFor: 'instance creation' stamp: 'LC 6/17/2001 11:52'!
new
	"
	SkipList new
	"
	^ super new initialize: 10! !

!SkipList class methodsFor: 'instance creation' stamp: 'LC 6/18/2001 17:39'!
new: anInteger
	^ self maxLevel: (anInteger log: 2) ceiling! !

!SkipList class methodsFor: 'instance creation' stamp: 'LC 6/18/2001 18:40'!
new: anInteger sortBlock: aBlock
	^ (self new: anInteger) sortBlock: aBlock! !

!SkipList class methodsFor: 'instance creation' stamp: 'LC 6/18/2001 17:32'!
sortBlock: aBlock
	^ self new sortBlock: aBlock! !

!IdentitySkipList methodsFor: 'element comparison' stamp: 'LC 6/18/2001 20:28'!
is: element1 equalTo: element2
	^ element1 == element2! !

!StandardFileMenu methodsFor: 'private' stamp: 'KenD 10/27/2015 19:13'!
advance: anInteger containingDirectoriesFrom: aDirectory

	| theDirectory |
	theDirectory _ aDirectory.
	1 to: anInteger do: [:i | theDirectory _ theDirectory parent].
	^theDirectory! !

!StandardFileMenu methodsFor: 'private' stamp: 'jmv 7/9/2009 08:44'!
newFileFrom: aDirectory withPattern: aPattern

	canTypeFileName _ true.
	self pattern: aPattern.
	^self makeFileMenuFor: aDirectory! !

!StandardFileMenu methodsFor: 'private' stamp: 'acg 4/15/1999 22:03'!
oldFileFrom: aDirectory

	canTypeFileName _ false.
	^self makeFileMenuFor: aDirectory! !

!StandardFileMenu methodsFor: 'private' stamp: 'jmv 7/9/2009 08:44'!
oldFileFrom: aDirectory withPattern: aPattern

	canTypeFileName _ false.
	self pattern: aPattern.
	^self makeFileMenuFor: aDirectory! !

!StandardFileMenu methodsFor: 'private' stamp: 'jmv 7/9/2009 08:44'!
pattern: patString
	" * for all files, or '*.cs' for changeSets, etc.  Just like fileLists
	jmv- Added support for multiple patterns, separated by spaces.
	For example '*.cs *.st' will show both changesets and fileouts."

	pattern _ patString substrings! !

!StandardFileMenu methodsFor: 'private' stamp: 'jmv 7/9/2009 08:45'!
patternMatches: aString

	^pattern anySatisfy: [ :pat | pat match: aString ]! !

!StandardFileMenu methodsFor: 'basic control sequences' stamp: 'KenD 10/27/2015 19:00'!
confirmExistingFiles: aResult

	|choice|
	(aResult directory includesKey: aResult name) ifFalse: [^aResult].
	
	choice _ (PopUpMenu
		labels:
'overwrite that file
choose another name
cancel')
		startUpWithCaption: aResult name, '
already exists.'.

	choice = 1 ifTrue: [
		aResult directory 
			removeKey: aResult name
			ifAbsent: 
				[^self startUpWithCaption: 
'Can''t delete ', aResult name, '
Select another file'].
		^aResult].
	choice = 2 ifTrue: [^self startUpWithCaption: 'Select Another File'].
	^nil
 ! !

!StandardFileMenu methodsFor: 'basic control sequences' stamp: 'KenD 10/27/2015 19:03'!
getTypedFileName: aResult

	| name |
	name _ FillInTheBlankMorph 
		request: 'Enter a new file name' 
		initialAnswer: ''.
	name = '' ifTrue: [^self startUpWithCaption: 'Select a File:' ].
	^ StandardFileMenuResult
			directory: (aResult directory)
			name: name
! !

!StandardFileMenu methodsFor: 'basic control sequences' stamp: 'jmv 7/27/2018 20:59:10'!
startUpWithCaption: aString at: location allowKeyboard: aBoolean

	|result|
	result _ super startUpWithCaption: aString at: location allowKeyboard: aBoolean.
	result ifNil: [^nil].
	result isDirectory ifTrue:
		[self makeFileMenuFor: result directory.
		 ^self startUpWithCaption: aString at: location allowKeyboard: aBoolean].
	result isCommand ifTrue: 
		[result _ self getTypedFileName: result.
		result ifNil: [^nil]].
	canTypeFileName ifTrue: [^self confirmExistingFiles: result].
	^result
	! !

!StandardFileMenu methodsFor: 'menu building' stamp: 'di 5/12/2000 10:31'!
directoryNamesString: aDirectory
"Answer a string concatenating the directory name strings in aDirectory, each string followed by a '[...]' indicator, and followed by a newLine."

	^ String streamContents:
		[:s | aDirectory directoryNames do: 
				[:dn | s nextPutAll: dn withBlanksTrimmed , ' [...]'; newLine]]

! !

!StandardFileMenu methodsFor: 'menu building' stamp: 'jmv 7/9/2009 08:43'!
fileNamesString: aDirectory
"Answer a string concatenating the file name strings in aDirectory, each string followed by a newLine."

	^String streamContents:
		[:s | 
			aDirectory fileNames do: 
				[:fn |
					(self patternMatches: fn) ifTrue: [
						s nextPutAll: fn withBlanksTrimmed; newLine]]]! !

!StandardFileMenu methodsFor: 'menu building' stamp: 'jmv 5/26/2013 15:53'!
makeFileMenuFor: aDirectory
"Initialize an instance of me to operate on aDirectory"

	| theMenu |
	pattern ifNil: [ self pattern: '*'].
	self 
		labels: 	(self menuLabelsString: aDirectory)
		lines: 	(self menuLinesArray: aDirectory).
	theMenu _ self selections: (self menuSelectionsArray: aDirectory).
	^theMenu! !

!StandardFileMenu methodsFor: 'menu building' stamp: 'acg 4/15/1999 21:57'!
menuLabelsString: aDirectory
"Answer a menu labels object corresponding to aDirectory"

	^ String streamContents: 
		[:s | 
			canTypeFileName ifTrue: 
				[s nextPutAll: 'Enter File Name...'; newLine].
			s nextPutAll: (self pathPartsString: aDirectory).
			s nextPutAll: (self directoryNamesString: aDirectory).
			s nextPutAll: (self fileNamesString: aDirectory).
			s skip: -1]! !

!StandardFileMenu methodsFor: 'menu building' stamp: 'jmv 10/19/2015 16:36'!
menuLinesArray: aDirectory
"Answer a menu lines object corresponding to aDirectory"

	| typeCount nameCnt dirDepth|
	typeCount _ canTypeFileName 
		ifTrue: [1] 
		ifFalse: [0].
	nameCnt _ aDirectory directoryNames size.
	dirDepth _ aDirectory pathComponents size.
	^Array streamContents: [:s |
		canTypeFileName ifTrue: [s nextPut: 1].
		s nextPut: dirDepth + typeCount + 1.
		s nextPut: dirDepth + nameCnt + typeCount + 1]! !

!StandardFileMenu methodsFor: 'menu building' stamp: 'pb 5/25/2016 01:13'!
menuSelectionsArray: aDirectory
"Answer a menu selections object corresponding to aDirectory.  The object is an array corresponding to each item, each element itself constituting a two-element array, the first element of which contains a selector to operate on and the second element of which contains the parameters for that selector."

	|dirSize|
	dirSize _ aDirectory pathComponents size.
	^Array streamContents: [:s |
		canTypeFileName ifTrue:
			[s nextPut: (StandardFileMenuResult
				directory: aDirectory
				name: nil)].
		s nextPut: (StandardFileMenuResult
			directory: (DirectoryEntry roots first)
			name: '').
		aDirectory pathComponents withIndexDo: 
			[:d :i | s nextPut: (StandardFileMenuResult
					directory: (self 
						advance: dirSize - i
						containingDirectoriesFrom: aDirectory)
					name: '')].
		aDirectory directoriesDo:
			[:dir |  s nextPut: (StandardFileMenuResult
						directory: dir
						name: '')].
		aDirectory fileNames do: 
			[:fn | (self patternMatches: fn) ifTrue: [
					s nextPut: (StandardFileMenuResult
						directory: aDirectory
						name: fn)]]]! !

!StandardFileMenu methodsFor: 'menu building' stamp: 'jmv 4/2/2016 23:07'!
pathPartsString: aDirectory
"Answer a string concatenating the path parts strings in aDirectory, each string followed by a newLine."

	^String streamContents:
		[:s | 
			s nextPutAll: '[]'; newLine.
			aDirectory pathComponents asArray withIndexDo: 
				[:part :i |
					s next: i put: $ .
					s nextPutAll: part withBlanksTrimmed; newLine]]! !

!StandardFileMenu class methodsFor: 'standard file operations' stamp: 'jmv 1/21/2013 22:59'!
oldFileFrom: aDirectory

	^(self oldFileMenu: aDirectory)
		startUpWithCaption: 'Select a File:'! !

!StandardFileMenu class methodsFor: 'instance creation' stamp: 'pb 5/25/2016 01:13'!
oldFileMenu: aDirectory

	(aDirectory isKindOf: DirectoryEntry)
		ifFalse: [ self error: 'I require a DirectoryEntry'].

	^self new oldFileFrom: aDirectory! !

!AsyncFile methodsFor: 'as yet unclassified'!
close

	fileHandle ifNil: [^ self].  "already closed"
	self primClose: fileHandle.
	Smalltalk unregisterExternalObject: semaphore.
	semaphore _ nil.
	fileHandle _ nil.
! !

!AsyncFile methodsFor: 'as yet unclassified' stamp: 'di 7/6/1998 10:58'!
fileHandle
	^ fileHandle! !

!AsyncFile methodsFor: 'as yet unclassified' stamp: 'jm 6/25/1998 07:54'!
open: fullFileName forWrite: aBoolean
	"Open a file of the given name, and return a handle for that file. Answer the receiver if the primitive succeeds, nil otherwise.
	If openForWrite is true, then:
		if there is no existing file with this name, then create one
		else open the existing file in read-write mode
	otherwise:
		if there is an existing file with this name, then open it read-only
		else answer nil."
	"Note: if an exisiting file is opened for writing, it is NOT truncated. If truncation is desired, the file should be deleted before being opened as an asynchronous file."
	"Note: On some platforms (e.g., Mac), a file can only have one writer at a time."

	| semaIndex |
	name _ fullFileName.
	writeable _ aBoolean.
	semaphore _ Semaphore new.
	semaIndex _ Smalltalk registerExternalObject: semaphore.
	fileHandle _ self primOpen: name forWrite: writeable semaIndex: semaIndex.
	fileHandle ifNil: [
		Smalltalk unregisterExternalObject: semaphore.
		semaphore _ nil.
		^ nil].
! !

!AsyncFile methodsFor: 'as yet unclassified' stamp: 'jmv 4/19/2015 08:49'!
readByteCount: byteCount fromFilePosition: fPosition onCompletionDo: aBlock
	"Start a read operation to read byteCount's from the given position in this file. and fork a process to await its completion. When the operation completes, evaluate the given block. Note that, since the completion block may run asynchronous, the client may need to use a SharedQueue or a semaphore for synchronization."

	| buffer |
	buffer _ String new: byteCount.
	self primReadStart: fileHandle fPosition: fPosition count: byteCount.
	"here's the process that awaits the results:"
	[
		| n |
		[	semaphore wait.
		  	n _ self primReadResult: fileHandle intoBuffer: buffer at: 1 count: byteCount.
		  	n = -1 "Busy".
		] whileTrue.  "loop while busy in case the semaphore had excess signals"
		n = -2 "Error" ifTrue: [^ self error: 'asynchronous read operation failed'].
		aBlock value: buffer.
	] forkAt: Processor userInterruptPriority.
! !

!AsyncFile methodsFor: 'as yet unclassified' stamp: 'jmv 9/16/2015 08:30'!
test: byteCount fileName: fileName
	"
	AsyncFile new test: 10000 fileName: 'testData'
	"

	| buf1 buf2 bytesWritten bytesRead |
	buf1 _ String new: byteCount withAll: $x.
	buf2 _ String new: byteCount.
	self open: fileName asAbsolutePathName forWrite: true.
	self primWriteStart: fileHandle
		fPosition: 0
		fromBuffer: buf1
		at: 1
		count: byteCount.
	semaphore wait.
	bytesWritten _ self primWriteResult: fileHandle.
	self close.
	
	self open: fileName asAbsolutePathName forWrite: false.
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

!AsyncFile methodsFor: 'as yet unclassified' stamp: 'di 7/6/1998 10:58'!
waitForCompletion
	semaphore wait! !

!AsyncFile methodsFor: 'as yet unclassified' stamp: 'jmv 4/19/2015 08:49'!
writeBuffer: buffer atFilePosition: fPosition onCompletionDo: aBlock
	"Start an operation to write the contents of the buffer at given position in this file, and fork a process to await its completion. When the write completes, evaluate the given block. Note that, since the completion block runs asynchronously, the client may need to use a SharedQueue or a semaphore for synchronization."

	self primWriteStart: fileHandle
		fPosition: fPosition
		fromBuffer: buffer
		at: 1
		count: buffer size.
	"here's the process that awaits the results:"
	[
		| n |
		[	semaphore wait.
		  	n _ self primWriteResult: fileHandle.
		  	n = -1 "Busy".
		] whileTrue.  "loop while busy in case the semaphore had excess signals"
		n = -2 "Error" ifTrue: [^ self error: 'asynchronous write operation failed'].
		n = buffer size ifFalse: [^ self error: 'did not write the entire buffer'].
		aBlock value.
	] forkAt: Processor userInterruptPriority.
! !

!AsyncFile methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primClose: fHandle
	"Close this file. Do nothing if primitive fails."

	<primitive: 'primitiveAsyncFileClose' module: 'AsynchFilePlugin'>
! !

!AsyncFile methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primOpen: fileName forWrite: openForWrite semaIndex: semaIndex
	"Open a file of the given name, and return a handle for that file. Answer the receiver if the primitive succeeds, nil otherwise."

	<primitive: 'primitiveAsyncFileOpen' module: 'AsynchFilePlugin'>
	^ nil
! !

!AsyncFile methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primReadResult: fHandle intoBuffer: buffer at: startIndex count: count
	"Copy the result of the last read operation into the given buffer starting at the given index. The buffer may be any sort of bytes or words object, excluding CompiledMethods. Answer the number of bytes read. A negative result means:
		-1 the last operation is still in progress
		-2 the last operation encountered an error"

	<primitive: 'primitiveAsyncFileReadResult' module: 'AsynchFilePlugin'>
	self primitiveFailed
! !

!AsyncFile methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primReadStart: fHandle fPosition: fPosition count: count
	"Start a read operation of count bytes starting at the given offset in the given file."

	<primitive: 'primitiveAsyncFileReadStart' module: 'AsynchFilePlugin'>
	self error: 'READ THE COMMENT FOR THIS METHOD.'

"NOTE: This method will fail if there is insufficient C heap to allocate an internal buffer of the required size (the value of count).  If you are trying to read a movie file, then the buffer size will be height*width*2 bytes.  Each Squeak image retains a value to be used for this allocation, and it it initially set to 0.  If you are wish to play a 640x480 movie, you need room for a buffer of 640*480*2 = 614400 bytes.  You should execute the following...

	Smalltalk extraVMMemory 2555000.

Then save-and-quit, restart, and try to open the movie file again.  If you are using Async files in another way, find out the value of count when this failure occurs (call it NNNN), and instead of the above, execute...

	Smalltalk extraVMMemory: Smalltalk extraVMMemory + NNNN

then save-and-quit, restart, and try again.
"

! !

!AsyncFile methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primWriteResult: fHandle
	"Answer the number of bytes written. A negative result means:
		-1 the last operation is still in progress
		-2 the last operation encountered an error"

	<primitive: 'primitiveAsyncFileWriteResult' module: 'AsynchFilePlugin'>
	self primitiveFailed
! !

!AsyncFile methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primWriteStart: fHandle fPosition: fPosition fromBuffer: buffer at: startIndex count: count
	"Start a write operation of count bytes starting at the given index in the given buffer. The buffer may be any sort of bytes or words object, excluding CompiledMethods. The contents of the buffer are copied into an internal buffer immediately, so the buffer can be reused after the write operation has been started. Fail if there is insufficient C heap to allocate an internal buffer of the requested size."

	<primitive: 'primitiveAsyncFileWriteStart' module: 'AsynchFilePlugin'>
	writeable ifFalse: [^ self error: 'attempt to write a file opened read-only'].
	self primitiveFailed
! !

!SkipListNode methodsFor: 'accessing' stamp: 'LC 6/17/2001 12:55'!
atForward: i put: node
	^ pointers at: i put: node! !

!SkipListNode methodsFor: 'accessing' stamp: 'LC 6/18/2001 13:34'!
forward: i 
	^ pointers at: i! !

!SkipListNode methodsFor: 'accessing' stamp: 'LC 6/18/2001 12:21'!
level
	^ pointers size! !

!SkipListNode methodsFor: 'accessing' stamp: 'LC 6/18/2001 19:20'!
next
	^ pointers first! !

!SkipListNode methodsFor: 'accessing' stamp: 'LC 6/18/2001 10:40'!
object
	^ object! !

!SkipListNode methodsFor: 'initialization' stamp: 'LC 6/17/2001 11:54'!
initialize: maxLevel
	pointers _ Array new: maxLevel! !

!SkipListNode methodsFor: 'private' stamp: 'LC 6/18/2001 10:18'!
object: anObject
	object _ anObject! !

!SkipListNode methodsFor: 'printing' stamp: 'LC 6/18/2001 15:26'!
printOn: aStream
	| first |
	aStream
		nextPut: $[;
		nextPutAll: object printString;
		nextPutAll: ']-->('.
	first _ true.
	pointers do: [:node |
		first ifTrue: [first _ false] ifFalse: [aStream space].
		aStream nextPutAll: (node ifNil: ['*'] ifNotNil: [node object printString])].
	aStream nextPut: $)
! !

!SkipListNode class methodsFor: 'instance creation' stamp: 'LC 6/17/2001 09:16'!
new: maxLevel
	^ super new initialize: maxLevel! !

!SkipListNode class methodsFor: 'instance creation' stamp: 'LC 6/18/2001 10:20'!
on: element level: maxLevel 
	^ (self new: maxLevel)
		object: element! !

!SkipListNode class methodsFor: 'instance creation' stamp: 'LC 6/18/2001 12:44'!
tailOfLevel: n
	^ self on: nil level: n! !

!StandardFileMenuResult methodsFor: 'accessing' stamp: 'acg 4/15/1999 08:43'!
directory

	^directory! !

!StandardFileMenuResult methodsFor: 'accessing' stamp: 'acg 4/15/1999 08:43'!
directory: aDirectory

	^directory _ aDirectory! !

!StandardFileMenuResult methodsFor: 'accessing' stamp: 'acg 4/15/1999 08:43'!
name

	^name! !

!StandardFileMenuResult methodsFor: 'accessing' stamp: 'acg 4/15/1999 08:43'!
name: aString

	^name _ aString! !

!StandardFileMenuResult methodsFor: 'accessing' stamp: 'sw 6/9/1999 11:50'!
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: ' with directory: '.
	directory printOn: aStream.
	aStream nextPutAll: ' name: '.
	name printOn: aStream

"StandardFileMenu oldFile"! !

!StandardFileMenuResult methodsFor: 'private' stamp: 'acg 4/15/1999 08:42'!
directory: aDirectory name: aString

	directory _ aDirectory.
	name _ aString.
	^self! !

!StandardFileMenuResult methodsFor: 'testing' stamp: 'acg 4/15/1999 09:05'!
isCommand

	^name isNil! !

!StandardFileMenuResult methodsFor: 'testing' stamp: 'acg 4/15/1999 20:57'!
isDirectory

	^name = ''! !

!StandardFileMenuResult class methodsFor: 'instance creation' stamp: 'acg 4/15/1999 08:42'!
directory: aDirectory name: aString

	^super new directory: aDirectory name: aString! !

!ProtocolCatcher2 methodsFor: 'catching' stamp: 'jmv 11/3/2008 13:03'!
doesNotUnderstand: aMessage

	| protocol object answer |
	Started ifTrue: [
		protocol _ Protocols at: realObject class ifAbsentPut: Set new.
		protocol add: aMessage selector ].
	realObject == self ifTrue: [
		self halt.
		^nil].
	object _ self realObject.
	answer _ aMessage sendTo: object.
	^answer == object 
		ifTrue: [ self ]
		ifFalse: [ answer ]! !

!ProtocolCatcher2 methodsFor: 'services' stamp: 'jmv 11/3/2008 13:00'!
initialize
	realObject _ self! !

!ProtocolCatcher2 methodsFor: 'accessing' stamp: 'jmv 11/3/2008 14:55'!
realObject
	realObject == self ifTrue: [^self halt].
	^realObject class == ProtocolCatcher2
		ifTrue: [ realObject realObject ]
		ifFalse: [ realObject ]! !

!ProtocolCatcher2 class methodsFor: 'services' stamp: 'jmv 11/3/2008 12:51'!
clear
	Protocols _ Dictionary new.
	Started _ false! !

!ProtocolCatcher2 class methodsFor: 'services' stamp: 'jmv 11/3/2008 12:51'!
observedClasses

	^Protocols keys! !

!ProtocolCatcher2 class methodsFor: 'services' stamp: 'jmv 11/3/2008 12:51'!
protocolFor: aClass

	^Protocols at: aClass! !

!ProtocolCatcher2 class methodsFor: 'services' stamp: 'jmv 11/3/2008 12:51'!
start
	Started _ true! !

!ProtocolCatcher2 class methodsFor: 'services' stamp: 'jmv 11/3/2008 12:51'!
stop
	Started _ false! !

!ProtocolCatcher2 class methodsFor: 'instance creation' stamp: 'jmv 4/14/2011 16:04'!
on: anObject
	| catcher |
	catcher _ self new.
	catcher instVarNamed: 'realObject' put: anObject.
	^catcher! !

!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/24/2003 23:12'!
activate

	self isSuspended ifTrue:
		[self timespans add: 
			(Timespan starting: DateAndTime now duration: Duration zero).
		self state: #active]
! !

!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/24/2003 23:45'!
duration

	| ts last |
	self isSuspended 
		ifTrue:
			[ (ts _ self timespans) isEmpty ifTrue: 
				[ ts _ { Timespan starting: DateAndTime now duration: Duration zero } ] ]
		ifFalse:
			[ last _ self timespans last.
			ts _ self timespans allButLast
				add: (last duration: (DateAndTime now - last start); yourself);
				yourself ].
		
	^ (ts collect: [ :t | t duration ]) sum
! !

!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/25/2003 11:21'!
end

	^ self timespans last next

! !

!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/24/2003 22:48'!
isActive

	^ self state = #active
! !

!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/24/2003 22:48'!
isSuspended

	^ self state = #suspended

! !

!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/25/2003 13:25'!
printOn: aStream

	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: self state;
		nextPut: $:;
		print: self duration;
		nextPut: $).

! !

!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/25/2003 12:03'!
reActivate

	self 
		suspend;
		activate.
! !

!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/25/2003 11:54'!
reset

	self suspend.
	timespans _ nil.

! !

!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/24/2003 23:18'!
start

	^ self timespans first start

! !

!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/24/2003 22:47'!
state

	^ state ifNil: [ state _ #suspended ]
! !

!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/24/2003 22:46'!
state: aSymbol

	state _ aSymbol
! !

!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/24/2003 23:13'!
suspend

	| ts |
	self isActive ifTrue:
		[ ts _ self timespans last.
		ts duration: (DateAndTime now - ts start).
		self state: #suspended]
! !

!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/24/2003 22:44'!
timespans

	^ timespans ifNil: [ timespans _ OrderedCollection new ]
! !

!ProtocolCatcher methodsFor: 'catching' stamp: 'jmv 11/3/2008 13:03'!
doesNotUnderstand: aMessage

	| protocol object answer |
	Started ifTrue: [
		protocol _ Protocols at: realObject class ifAbsentPut: Set new.
		protocol add: aMessage selector ].
	realObject == self ifTrue: [
		self halt.
		^nil].
	object _ self realObject.
	answer _ aMessage sendTo: object.
	^answer == object 
		ifTrue: [ self ]
		ifFalse: [ answer ]! !

!ProtocolCatcher methodsFor: 'services' stamp: 'jmv 11/3/2008 12:40'!
initialize
	realObject _ self! !

!ProtocolCatcher methodsFor: 'accessing' stamp: 'jmv 11/3/2008 13:04'!
realObject
	realObject == self ifTrue: [^self halt].
	^realObject class == ProtocolCatcher
		ifTrue: [ realObject realObject ]
		ifFalse: [ realObject ]! !

!ProtocolCatcher methodsFor: 'accessing' stamp: 'jmv 3/5/2013 22:20'!
realObject: anObject
	realObject _ anObject! !

!ProtocolCatcher class methodsFor: 'services' stamp: 'jmv 11/3/2008 11:28'!
clear
	Protocols _ Dictionary new.
	Started _ false! !

!ProtocolCatcher class methodsFor: 'services' stamp: 'jmv 11/3/2008 11:10'!
observedClasses

	^Protocols keys! !

!ProtocolCatcher class methodsFor: 'services' stamp: 'jmv 11/3/2008 11:11'!
protocolFor: aClass

	^Protocols at: aClass! !

!ProtocolCatcher class methodsFor: 'services' stamp: 'jmv 11/3/2008 11:28'!
start
	Started _ true! !

!ProtocolCatcher class methodsFor: 'services' stamp: 'jmv 11/3/2008 11:28'!
stop
	Started _ false! !

!ProtocolCatcher class methodsFor: 'instance creation' stamp: 'jmv 3/5/2013 22:20'!
on: anObject
	| catcher |
	catcher _ self new.
	catcher realObject: anObject.
	^catcher! !

!Collection methodsFor: '*Goodies-Collections' stamp: 'LC 6/18/2001 20:30'!
asIdentitySkipList
	"Answer a IdentitySkipList whose elements are the elements of the 
	receiver. The sort order is the default less than or equal."

	^ self as: IdentitySkipList! !

!Collection methodsFor: '*Goodies-Collections' stamp: 'LC 6/18/2001 18:47'!
asSkipList
	"Answer a SkipList whose elements are the elements of the 
	receiver. The sort order is the default less than or equal."

	^ self as: SkipList! !

!Collection methodsFor: '*Goodies-Collections' stamp: 'jmv 12/18/2015 14:58'!
asSkipList: aSortBlock 
	"Answer a SkipList whose elements are the elements of the 
	receiver. The sort order is defined by the argument, aSortBlock."

	| skipList |
	skipList _ SkipList new: self size sortBlock: aSortBlock.
	skipList addAll: self.
	^ skipList! !
