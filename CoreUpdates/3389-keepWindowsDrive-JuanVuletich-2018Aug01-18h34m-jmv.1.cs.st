'From Cuis 5.0 of 7 November 2016 [latest update: #3386] on 1 August 2018 at 6:36:13 pm'!

!FileSystemEntry methodsFor: 'initialize-release' stamp: 'jmv 8/1/2018 18:34:46'!
pathString: aString
	| tokens |
	tokens _ FileIOAccessor default absolutePathComponentsFor: aString.
	tokens ifEmpty: [^ nil].
	self fileAccessor isDriveSupported
		 ifTrue: [
			tokens first asDriveName ifNotNil: [ :guessedDriveName |
				^ self pathComponents: (tokens copyFrom: 2 to: tokens size) drive: guessedDriveName ]].
	self pathComponents: tokens drive: nil! !

