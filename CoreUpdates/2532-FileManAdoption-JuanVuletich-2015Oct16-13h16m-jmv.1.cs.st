'From Cuis 4.2 of 25 July 2013 [latest update: #2530] on 16 October 2015 at 1:59:07.803429 pm'!

!FmFileEntry methodsFor: 'accessing' stamp: 'jmv 10/16/2015 13:54'!
drive
	self onUnix ifTrue: [^ drive := nil].
	drive
		ifNil: [drive := self fileAccessor currentDriveName].
	^ drive! !

!FmFileEntry methodsFor: 'accessing' stamp: 'jmv 10/16/2015 13:53'!
parent: aFmDirectory
	parent := aFmDirectory.

	self pathName: aFmDirectory pathName, self fileAccessor slash, self name! !

!FmFileEntry methodsFor: 'initialize-release' stamp: 'jmv 10/16/2015 13:53'!
pathName: aString 
	| path tokens guessedDriveName |
	path := aString asAbsolutePathName.
	tokens := path findTokens: self fileAccessor slash.
	tokens ifEmpty: [^ nil].
	self isDriveSupported
		 ifTrue: [
			guessedDriveName := tokens first asDriveName.
			guessedDriveName isDriveName
				ifTrue: [
					self drive: guessedDriveName.
					tokens := tokens copyFrom: 2 to: tokens size.]].
	self pathComponents: tokens! !

!FmFileEntry methodsFor: 'printing' stamp: 'jmv 10/16/2015 13:53'!
printPathOn: aStream 
	self drive
		ifNotNil: [:d | aStream nextPutAll: d].
	aStream nextPutAll: self fileAccessor slash.
	self pathComponents
		do: [:each | aStream nextPutAll: each]
		separatedBy: [aStream nextPutAll: self fileAccessor slash]! !


!FmDirectoryEntry methodsFor: 'testing' stamp: 'jmv 10/16/2015 13:55'!
exists
	
	(self isDriveSupported and: [self pathComponents isEmpty])
		 ifTrue: [^self fileAccessor drives includes: self drive].

	self isRoot ifTrue: [^true].
	
	self fileAccessor onUnix ifFalse: [^self fileAccessor directoryExists: self pathName]. 
	
	^self fileAccessor directoryExists: self name in: self parent pathName! !


!FmDirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 10/16/2015 13:56'!
roots
	"For the systems having notion of drives"
	"FmDirectoryEntry roots"
	^ FmFileIOAccessor default drives
		ifEmpty: [Array with: self root]
		ifNotEmpty: [:drvs | drvs
				collect: [:each | each asFileEntry]]! !

!methodRemoval: FmFileEntry class #drives!
FmFileEntry class removeSelector: #drives!
!methodRemoval: FmFileEntry #currentDriveName!
FmFileEntry removeSelector: #currentDriveName!
!methodRemoval: FmFileEntry #drives!
FmFileEntry removeSelector: #drives!
!methodRemoval: FmFileEntry #slash!
FmFileEntry removeSelector: #slash!
