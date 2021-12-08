'From Cuis 5.0 [latest update: #4975] on 24 November 2021 at 11:11:34 am'!

!FileSystemEntry methodsFor: 'initialize-release' stamp: 'jmv 11/24/2021 11:04:33'!
pathComponents: tokens drive: driveStringOrNil
	| firstToken |
	drive _ driveStringOrNil.
	pathComponents _ tokens.
	(driveStringOrNil isNil and: [
		tokens notEmpty and: [ (firstToken _ tokens first) isDriveName ]]) ifTrue: [
			drive _ firstToken.
			pathComponents _ tokens allButFirst ].
	pathComponents ifNotEmpty: [ name _ pathComponents last ].
	self invalidate.! !

!methodRemoval: FileSystemEntry #drive: stamp: 'jmv 11/24/2021 10:54:55'!
FileSystemEntry removeSelector: #drive:!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
FileSystemEntry allSubInstancesDo: [ :each | each pathComponents ]!

