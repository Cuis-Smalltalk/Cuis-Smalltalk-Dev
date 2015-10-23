'From Cuis 4.2 of 25 July 2013 [latest update: #2550] on 21 October 2015 at 1:30:02.087828 pm'!
!classDefinition: #FmEntry category: #'FileMan-Core'!
Object subclass: #FmEntry
	instanceVariableNames: 'drive pathComponents parent name creationTime modificationTime'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FileMan-Core'!

!FmEntry commentStamp: '<historical>' prior: 0!
I represent a single file entry (including directory).

You can write data by #fileContents: , and read the data by #fileContents.

---
mu 11/6/2006 20:21

--------------
See examples class category.

See FmDirectoryEntry.

See categories starting with '*fileman-' in String.
	Smalltalk imageName asFileEntry fileSize
	Smalltalk imageName asFileEntry parent directories do: [ :a | a print ]!


!FmEntry methodsFor: 'comparing' stamp: 'jmv 10/21/2015 12:38'!
= aFileEntry
	self class = aFileEntry class ifFalse: [^false].
	^self pathComponents = aFileEntry pathComponents and: [ self drive = aFileEntry drive ]! !

!FmEntry methodsFor: 'comparing' stamp: 'jmv 10/21/2015 12:38'!
hash
	^self pathComponents hash! !

!FmEntry methodsFor: 'accessing-file name' stamp: 'jmv 10/21/2015 12:43'!
baseName
	^self nameVersionExtension first! !

!FmEntry methodsFor: 'accessing-file name' stamp: 'jmv 10/21/2015 12:43'!
extension
	^self fileAccessor extensionFor: name! !

!FmEntry methodsFor: 'accessing-file name' stamp: 'jmv 10/21/2015 12:43'!
nameVersionExtension
	^self fileAccessor splitNameVersionExtensionFor: self name! !

!FmEntry methodsFor: 'accessing-file name' stamp: 'jmv 10/21/2015 12:43'!
nameWithoutExtension
	"
	'writings.txt' asFileEntry nameWithoutExtension
	'folder.ext/writings.txt' asFileEntry nameWithoutExtension
	'folder.ext/writings' asFileEntry nameWithoutExtension
	"
	^self fileAccessor baseNameFor: name! !

!FmEntry methodsFor: 'accessing-file name' stamp: 'jmv 10/21/2015 12:43'!
version
	^self nameVersionExtension second! !

!FmEntry methodsFor: 'accessing' stamp: 'jmv 10/21/2015 12:44'!
basicPathComponents: aCollection
	pathComponents := aCollection! !

!FmEntry methodsFor: 'accessing' stamp: 'jmv 10/21/2015 12:45'!
creationTime
	creationTime ifNil: [self initValuesFromParent].
	^creationTime! !

!FmEntry methodsFor: 'accessing' stamp: 'jmv 10/21/2015 12:45'!
creationTime: value
	creationTime := value! !

!FmEntry methodsFor: 'accessing' stamp: 'jmv 10/21/2015 12:53'!
drive
	self fileAccessor onUnix ifTrue: [^ drive := nil].
	^ drive! !

!FmEntry methodsFor: 'accessing' stamp: 'jmv 10/21/2015 12:45'!
drive: aString
	drive := aString! !

!FmEntry methodsFor: 'accessing' stamp: 'jmv 10/21/2015 12:45'!
modificationTime
	modificationTime ifNil: [self initValuesFromParent].
	^modificationTime! !

!FmEntry methodsFor: 'accessing' stamp: 'jmv 10/21/2015 12:45'!
modificationTime: value
	modificationTime := value! !

!FmEntry methodsFor: 'accessing' stamp: 'jmv 10/21/2015 12:45'!
name
	^name ifNil: [ drive ]! !

!FmEntry methodsFor: 'accessing' stamp: 'jmv 10/21/2015 12:45'!
name: aString 
	name := aString.
	self pathComponents
		ifNotEmpty: [self pathComponents at: self pathComponents size put: name]! !

!FmEntry methodsFor: 'accessing' stamp: 'jmv 10/21/2015 12:45'!
parent
	parent ifNil: [parent := self ensureParent].
	^parent! !

!FmEntry methodsFor: 'accessing' stamp: 'jmv 10/21/2015 12:46'!
parent: aFmDirectory
	parent := aFmDirectory.

	self pathName: aFmDirectory pathName, self fileAccessor slash, self name! !

!FmEntry methodsFor: 'accessing' stamp: 'jmv 10/21/2015 12:46'!
parents
	| ord par |
	par := self parent.
	ord := OrderedCollection with: par.
	[par isRoot] whileFalse: [
		par := par parent.
		ord add: par.
	].
	^ord! !

!FmEntry methodsFor: 'accessing' stamp: 'jmv 10/21/2015 12:46'!
pathComponents
	pathComponents ifNil: [pathComponents := OrderedCollection new].
	^pathComponents! !

!FmEntry methodsFor: 'accessing' stamp: 'jmv 10/21/2015 12:46'!
pathName

	^ String streamContents: [ :stream |
		self printPathOn: stream ]! !

!FmEntry methodsFor: 'private' stamp: 'jmv 10/21/2015 12:47'!
ensureParent
	self pathComponents isEmpty
		ifTrue: [^ nil].
	parent := FmDirectoryEntry
				pathComponents: (self pathComponents copyFrom: 1 to: self pathComponents size - 1)
				drive: self drive.
	^ parent! !

!FmEntry methodsFor: 'private' stamp: 'jmv 10/21/2015 12:50'!
fileAccessor

	^FmFileIOAccessor default! !

!FmEntry methodsFor: 'private' stamp: 'jmv 10/21/2015 12:47'!
initValuesFrom: otherEntry
	otherEntry ifNil: [^self].
	self creationTime: otherEntry creationTime.
	self modificationTime: otherEntry modificationTime! !

!FmEntry methodsFor: 'private' stamp: 'jmv 10/21/2015 12:48'!
initValuesFromParent
	| targets target |
	self ensureParent.
	self parent ifNil: [^self].

	targets := self isDirectory ifTrue: [self parent directories] ifFalse: [self parent files].
	target := targets detect: [:each | each = self] ifNone:[].

	self initValuesFrom: target! !

!FmEntry methodsFor: 'private' stamp: 'jmv 10/21/2015 12:49'!
setContentsOf: aStream to: aStringOrBytes

	aStringOrBytes isString
		ifFalse: [ aStream binary].
	aStream nextPutAll: aStringOrBytes! !

!FmEntry methodsFor: 'private' stamp: 'jmv 10/21/2015 12:49'!
setParent: anFmFileEntry
	parent := anFmFileEntry! !

!FmEntry methodsFor: 'testing' stamp: 'jmv 10/21/2015 12:50'!
isDirectory
	^false! !

!FmEntry methodsFor: 'testing' stamp: 'jmv 10/21/2015 12:52'!
isFile
	^false! !

!FmEntry methodsFor: 'testing' stamp: 'jmv 10/21/2015 12:52'!
isRoot
	^self parent isNil! !

!FmEntry methodsFor: 'initialize-release' stamp: 'jmv 10/21/2015 12:54'!
pathComponents: aCollection
	self pathComponents: aCollection detectDrive: true! !

!FmEntry methodsFor: 'initialize-release' stamp: 'jmv 10/21/2015 12:55'!
pathComponents: aCollection detectDrive: detectDrive
	| tokens firstToken  |
	aCollection isEmptyOrNil ifTrue: [^pathComponents := nil].
	tokens := aCollection.
	(detectDrive and: [(firstToken := aCollection first) isDriveName])
		ifTrue: [
			self drive: firstToken.
			self basicPathComponents: (tokens copyFrom: 2 to: tokens size)]
		ifFalse: [self basicPathComponents: tokens].

	pathComponents ifNotEmpty: [self name: pathComponents last]! !

!FmEntry methodsFor: 'initialize-release' stamp: 'jmv 10/21/2015 12:55'!
pathName: aString 
	| path tokens guessedDriveName |
	path := aString asAbsolutePathName.
	tokens := path findTokens: self fileAccessor slash.
	tokens ifEmpty: [^ nil].
	self fileAccessor isDriveSupported
		 ifTrue: [
			guessedDriveName := tokens first asDriveName.
			guessedDriveName ifNotNil: [
				self drive: guessedDriveName.
				tokens := tokens copyFrom: 2 to: tokens size ]].
	self pathComponents: tokens! !

!FmEntry methodsFor: 'initialize-release' stamp: 'jmv 10/21/2015 12:55'!
refresh
	creationTime := modificationTime := nil! !

!FmEntry methodsFor: 'printing' stamp: 'jmv 10/21/2015 12:56'!
printOn: aStream 
	self printPathOn: aStream! !

!FmEntry methodsFor: 'printing' stamp: 'jmv 10/21/2015 12:56'!
printPathOn: aStream 
	self drive
		ifNotNil: [:d | aStream nextPutAll: d].
	aStream nextPutAll: self fileAccessor slash.
	self pathComponents
		do: [:each | aStream nextPutAll: each]
		separatedBy: [aStream nextPutAll: self fileAccessor slash]! !


!FmEntry class methodsFor: 'instance creation' stamp: 'jmv 10/21/2015 12:57'!
pathComponents: comps
	| inst |
	inst := self new.
	inst pathComponents: comps.
	^inst! !

!FmEntry class methodsFor: 'instance creation' stamp: 'jmv 10/21/2015 12:57'!
pathComponents: comps drive: driveString
	| inst |
	inst := self new.
	inst pathComponents: comps detectDrive: false.
	inst drive: driveString.
	^inst! !

!FmEntry class methodsFor: 'instance creation' stamp: 'jmv 10/21/2015 12:57'!
pathName: aString
	| inst |
	inst := self new.
	inst pathName: aString.
	^inst! !

!FmEntry class methodsFor: 'class state access' stamp: 'jmv 10/21/2015 12:58'!
releaseClassCachedState

	self allSubInstancesDo: [ :each | each refresh]! !

