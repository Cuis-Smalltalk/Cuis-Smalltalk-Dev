'From Cuis7.3 [latest update: #7070] on 16 March 2025 at 8:03:33 pm'!

!FileIOAccessor methodsFor: 'utilities' stamp: 'wbsl 3/16/2025 20:03:07'!
absolutePathComponentsFor: aString relativeTo: baseDirectory
	"aString is a relative path, and will be applied to baseDirectory
	See comment at #isAbsolutePathName"

	| baseDirectoryPathComponents tokens |

	baseDirectoryPathComponents := baseDirectory pathComponents.
	aString = '.' ifTrue: [ ^ baseDirectoryPathComponents copy ].
	aString = '..' ifTrue:  [^ baseDirectoryPathComponents allButLast ].

	tokens := aString asPathTokens.
	[ tokens notEmpty and: [ tokens first = '..' ]] whileTrue: [
		baseDirectoryPathComponents := baseDirectoryPathComponents allButLast.
		tokens removeFirst ].

	^ Array streamContents: [ :strm |
		baseDirectory drive ifNotNil: [ :drive |
			strm nextPut: drive ].
		strm nextPutAll: baseDirectoryPathComponents.
		tokens do: [ :each |
			each = '.' ifFalse: [ strm nextPut: each ]]].! !

