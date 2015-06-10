'From Cuis 4.2 of 25 July 2013 [latest update: #2354] on 9 June 2015 at 9:42:35.922197 am'!

!String methodsFor: '*fileman-core-converting' stamp: 'jmv 6/6/2015 23:44'!
withoutWindowsDriveName
	^self beginsWithWindowsDriveName 
		ifFalse: [ self ]
		ifTrue: [ self copyFrom: 3 to: self size ]! !

!String methodsFor: '*fileman-core-testing' stamp: 'jmv 6/6/2015 23:38'!
beginsWithWindowsDriveName
	"Answer doesn't depend on running on Windows or not"
	self size < 2 ifTrue: [ ^false ].
	^self first asCharacter isDriveLetter
		and: [ self second isDriveSeparator
			and: [ self size = 2 or: [ self third isPathSeparator ]]]! !


!String class methodsFor: '*fileman-core-constants' stamp: 'jmv 6/6/2015 23:54'!
pathSeparators
	^'\:/'! !


!FmFileEntry methodsFor: 'private' stamp: 'jmv 6/6/2015 21:09'!
setParent: anFmFileEntry
	parent _ anFmFileEntry! !


!String methodsFor: '*fileman-core-converting' stamp: 'jmv 6/6/2015 23:37'!
asDriveName

	FmFileIOAccessor default onWindows ifTrue: [
		self beginsWithWindowsDriveName ifTrue: [ 
		^self copyFrom: 1 to: 2 ]].

	(FmFileIOAccessor default onMac) ifTrue: [ 
		^self upToFirstPathSeparator ]! !

!String methodsFor: '*fileman-core-converting' stamp: 'jmv 6/6/2015 23:56'!
asPathTokens

	^self beginsWithWindowsDriveName
		ifTrue: [
			(OrderedCollection with: (self copyFrom: 1 to: 2)), 
				((self copyFrom: 3 to: self size) findTokens: String pathSeparators) ]
		ifFalse: [
			self findTokens: String pathSeparators ]! !

!String methodsFor: '*fileman-core-testing' stamp: 'jmv 6/6/2015 23:50'!
isAbsolutePathName
	| upperName |
	self isEmpty ifTrue: [^ false].
	self first isPathSeparator ifTrue: [^ true].
	upperName := self asUppercase.
	(FmFileIOAccessor default drives
			anySatisfy: [:each | (upperName beginsWith: each)
					and: [| nextPos | 
						nextPos := each size + 1 min: self size max: 1.
						(self at: nextPos) isPathSeparator ]]) ifTrue: [^ true].
	^ false! !

!String methodsFor: '*fileman-core-testing' stamp: 'jmv 6/6/2015 23:35'!
isDriveName
	FmFileIOAccessor default onWindows
		ifTrue: [
			^ (self size between: 2 and: 3)
				and: [self beginsWithWindowsDriveName]].

	FmFileIOAccessor default onMac ifTrue: [
		^FileDirectory root directoryNames includes: self].

	^false! !


!FmFileEntry methodsFor: 'actions-path' stamp: 'jmv 6/6/2015 21:13'!
concatPathComponents: components
	| entry entryComponents parentEntry |
	components ifEmpty: [ ^self ].
	parentEntry _ self isRoot ifFalse: [ self ].
	entryComponents _ self pathComponents.
	components do: [ :eachComponent |
		entryComponents _ entryComponents copyWith: eachComponent.
		entry _ self fileEntryClass pathComponents: entryComponents drive: self drive.
		parentEntry ifNotNil: [
			entry setParent: parentEntry asDirectoryEntry ].
		parentEntry _ entry ].
	^entry! !

!FmFileEntry methodsFor: 'initialize-release' stamp: 'jmv 6/6/2015 21:42'!
pathName: aString 
	| path tokens guessedDriveName |
	path := aString asAbsolutePathName.
	tokens := path findTokens: self slash.
	tokens ifEmpty: [^ nil].
	self isDriveSupported
		 ifTrue: [
			guessedDriveName := tokens first asDriveName.
			guessedDriveName isDriveName
				ifTrue: [
					self drive: guessedDriveName.
					tokens _ tokens copyFrom: 2 to: tokens size.]].
	self pathComponents: tokens! !


!FmDirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 6/6/2015 23:40'!
roots
	"For the systems having notion of drives"
	"FmDirectoryEntry roots"
	^ self drives
		ifEmpty: [Array with: self root]
		ifNotEmpty: [:drvs | drvs
				collect: [:each | each asFileEntry]]! !


!FmFileIOAccessor methodsFor: 'actions' stamp: 'jmv 6/7/2015 00:12'!
asAbsolutePathName: aString
	"Not complete, but in most cases it is OK"

	| tokens curDir childPath |
	tokens := aString asPathTokens.

	"Already an absolute path name? just normalize path separators"
	aString isAbsolutePathName ifTrue: [
		^String streamContents: [ :strm |
			aString first isPathSeparator ifTrue: [ strm nextPutAll: self slash ].
			tokens
				do: [ :each | strm nextPutAll: each ]
				separatedBy: [ strm nextPutAll: self slash ]]].

	aString = '.' ifTrue: [^self defaultDirectoryPath].
	aString = '..' ifTrue: [^self containingDirectoryPathOf: self defaultDirectoryPath].


	curDir := self defaultDirectoryPath.
	tokens reverseDo: [:each |
		each = '..' ifTrue: [curDir := self containingDirectoryPathOf: curDir]
	].

	tokens removeAllSuchThat: [:each | #('.' '..') includes: each ].
	childPath := WriteStream on: String new.
	tokens do: [:each | childPath nextPutAll: each]
					separatedBy: [childPath nextPutAll: self slash].
		
	^curDir, self slash, childPath contents! !

!FmFileIOAccessor methodsFor: 'private' stamp: 'jmv 6/6/2015 23:52'!
containingDirectoryPathOf: pathName 

	(((pathName isNil
			or: [pathName isEmpty])
			or: [pathName isPathSeparator])
			or: [pathName isDriveName])
		ifTrue: [^ nil].
	^ pathName copyFrom: 1 to: pathName indexOfLastPathSeparator-1! !

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'jmv 6/6/2015 19:19'!
encode: encoderName on: externalStream 
	encoderName ifNil: [^self]. "UTF-8 will be used"
"	externalStream
		converter: (TextConverter newForEncoding: encoderName)"! !


!FmFileDirectoryFileIOAccessorCuis methodsFor: 'actions' stamp: 'jmv 6/7/2015 00:23'!
asAbsolutePathName: aString
	"Not complete, but in most cases it is OK"

	| tokens curDir childPath |
	tokens := aString asPathTokens.

	"Already an absolute path name? just normalize path separators"
	aString isAbsolutePathName ifTrue: [
		^String streamContents: [ :strm |
			aString first isPathSeparator ifTrue: [ strm nextPutAll: self slash ].
			tokens
				do: [ :each | strm nextPutAll: each ]
				separatedBy: [ strm nextPutAll: self slash ]]].

	aString = '.' ifTrue: [^self defaultDirectoryPath].
	aString = '..' ifTrue: [^self containingDirectoryPathOf: self defaultDirectoryPath].


	curDir := self defaultDirectoryPath.
	tokens reverseDo: [:each |
		each = '..' ifTrue: [curDir := self containingDirectoryPathOf: curDir]
	].

	tokens removeAllSuchThat: [:each | #('.' '..') includes: each ].
	childPath := WriteStream on: String new.
	tokens do: [:each | childPath nextPutAll: each]
					separatedBy: [childPath nextPutAll: self slash].
		
	^curDir, self slash, childPath contents! !

!FmFileDirectoryFileIOAccessorCuis methodsFor: 'private' stamp: 'jmv 6/7/2015 00:23'!
containingDirectoryPathOf: pathName 

	(((pathName isNil
			or: [pathName isEmpty])
			or: [pathName isPathSeparator])
			or: [pathName isDriveName])
		ifTrue: [^ nil].
	^ pathName copyFrom: 1 to: pathName indexOfLastPathSeparator-1! !

!methodRemoval: FmFileEntry #setDriveNameAndPathComponentsFrom:!
FmFileEntry removeSelector: #setDriveNameAndPathComponentsFrom:!
!methodRemoval: String #beginsWithDriveLetter!
String removeSelector: #beginsWithDriveLetter!
