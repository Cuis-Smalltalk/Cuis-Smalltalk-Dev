'From Cuis7.9 [latest update: #8327] on 7 November 2026 at 9:53:31 am'!

!FileIOAccessor methodsFor: 'accessing' stamp: 'jmv 7/Nov/2026, 12:51:36 pm (UTC)'!
slashCharacter
	"Given that FileMan supports $/ as path delimiter regardless of the platform,
	this method is mostly for FileMan's own use, and general usage is discouraged.
	Just use '/' "
	slash ifNil: [ slash := self primPathNameDelimiter ].
	^slash asString first! !


!FileIOAccessor methodsFor: 'accessing' stamp: 'jmv 7/Nov/2026, 12:49:16 pm (UTC)'!
pathNameDelimiter
	"Given that FileMan supports $/ as path delimiter regardless of the platform,
	this method is mostly for FileMan's own use, and general usage is discouraged.
	Just use $/ "
	^self slashCharacter! !

!FileIOAccessor methodsFor: 'accessing' stamp: 'jmv 7/Nov/2026, 12:51:14 pm (UTC)'!
slash
	"Given that FileMan supports $/ as path delimiter regardless of the platform,
	this method is mostly for FileMan's own use, and general usage is discouraged.
	Just use '/' "
	slash ifNil: [ slash := self primPathNameDelimiter ].
	^slash asString! !


!FileSystemEntry methodsFor: 'printing' stamp: 'jmv 7/Nov/2026, 12:47:59 pm (UTC)'!
printPathOn: aStream 
	self drive
		ifNotNil: [:d | aStream nextPutAll: d].
	aStream nextPut: self fileAccessor slashCharacter.
	self pathComponents
		do: [:each | aStream nextPutAll: each]
		separatedBy: [aStream nextPut: self fileAccessor slashCharacter].! !


!DirectoryEntry methodsFor: 'testing' stamp: 'jmv 7/Nov/2026, 12:47:47 pm (UTC)'!
updateExists
	
	| pathName |
	(self fileAccessor isDriveSupported and: [self pathComponents isEmpty])
		 ifTrue: [^ exists := self fileAccessor drives includes: self ].

	self isRoot ifTrue: [ ^ exists := true ].
	
	pathName := self pathName.
	pathName = self fileAccessor slashCharacter asString ifTrue: [ ^ exists := true ].

	exists := self fileAccessor basicDirectoryExists: pathName.! !

!methodRemoval: FileIOAccessor #slashString stamp: 'jmv 7/Nov/2026, 12:48:15 pm (UTC)'!
FileIOAccessor removeSelector: #slashString!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
FileIOAccessor default instVarNamed: 'slash' put: nil!

