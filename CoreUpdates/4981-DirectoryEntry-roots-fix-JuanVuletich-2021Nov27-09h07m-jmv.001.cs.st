'From Cuis 5.0 [latest update: #4975] on 27 November 2021 at 9:08:31 am'!

!FileSystemEntry methodsFor: 'initialize-release' stamp: 'jmv 11/27/2021 09:04:35'!
pathString: aString
	| tokens |
	tokens _ FileIOAccessor default absolutePathComponentsFor: aString.
	(tokens notEmpty and: [self fileAccessor isDriveSupported])
		 ifTrue: [
			tokens first asDriveName ifNotNil: [ :guessedDriveName |
				^ self pathComponents: (tokens copyFrom: 2 to: tokens size) drive: guessedDriveName ]].
	self pathComponents: tokens drive: nil! !

