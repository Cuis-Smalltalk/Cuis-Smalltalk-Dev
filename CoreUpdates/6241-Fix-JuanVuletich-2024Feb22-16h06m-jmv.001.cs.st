'From Cuis6.3 [latest update: #6240] on 22 February 2024 at 4:10:08 pm'!

!FileSystemEntry methodsFor: 'initialize-release' stamp: 'jmv 2/22/2024 15:59:56'!
pathComponents: tokens drive: driveStringOrNil
	| firstToken |
	drive := driveStringOrNil.
	pathComponents := tokens asArray.
	(driveStringOrNil isNil and: [
		pathComponents notEmpty and: [ (firstToken := pathComponents first) isDriveName ]]) ifTrue: [
			drive := firstToken.
			pathComponents := pathComponents allButFirst ].
	pathComponents ifNotEmpty: [ name := pathComponents last ].
	self invalidate.! !

