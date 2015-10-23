'From Cuis 4.2 of 25 July 2013 [latest update: #2537] on 19 October 2015 at 3:54:26.870164 pm'!

!String methodsFor: '*fileman-core-converting' stamp: 'jmv 10/19/2015 15:52'!
asDriveName
	"Answer a real drive name, or else answer nil.
	(Original FileMan implementation would answer first token on Mac even if it is not a Drive Name,
	and self in any case in other Unix variants)"

	| candidate |
	FmFileIOAccessor default onWindows ifTrue: [
		self beginsWithWindowsDriveName ifTrue: [ 
		^self copyFrom: 1 to: 2 ]].

	(FmFileIOAccessor default onMac) ifTrue: [
		candidate _ self upToFirstPathSeparator.
		(FileDirectory root directoryNames includes: candidate) ifTrue: [
			^candidate ]].
	
	^ nil! !


!FmFileEntry methodsFor: 'initialize-release' stamp: 'jmv 10/19/2015 15:53'!
pathName: aString 
	| path tokens guessedDriveName |
	path := aString asAbsolutePathName.
	tokens := path findTokens: self fileAccessor slash.
	tokens ifEmpty: [^ nil].
	self isDriveSupported
		 ifTrue: [
			guessedDriveName := tokens first asDriveName.
			guessedDriveName ifNotNil: [
				self drive: guessedDriveName.
				tokens := tokens copyFrom: 2 to: tokens size ]].
	self pathComponents: tokens! !


!FmFileIOAccessor methodsFor: 'accessing' stamp: 'jmv 10/19/2015 15:53'!
currentDriveName
	"
	self default currentDriveName
	(Might answer nil)
	"
	^Smalltalk imageName asDriveName! !

