'From Cuis 4.2 of 25 July 2013 [latest update: #2737] on 19 April 2016 at 11:34:23.981295 am'!

!FmFileIOAccessor methodsFor: 'actions' stamp: 'jmv 4/19/2016 10:19'!
absolutePathComponentsFor: aString
	"Not complete, but in most cases it is OK"

	| tokens curDirPathComponents |
	tokens _ aString asPathTokens.

	aString isAbsolutePathName ifTrue: [ ^ tokens asArray ].

	curDirPathComponents _ FmDirectoryEntry currentDirectory pathComponents.
	aString = '.' ifTrue: [ ^ curDirPathComponents copy ].
	aString = '..' ifTrue:  [^ curDirPathComponents allButLast ].

	[ tokens notEmpty and: [ tokens first = '..' ]] whileTrue: [
		curDirPathComponents _ curDirPathComponents allButLast.
		tokens removeFirst ].

	^ Array streamContents: [ :strm |
		strm nextPutAll: curDirPathComponents.
		tokens do: [ :each |
			each = '.' ifFalse: [ strm nextPut: each ]]]! !

