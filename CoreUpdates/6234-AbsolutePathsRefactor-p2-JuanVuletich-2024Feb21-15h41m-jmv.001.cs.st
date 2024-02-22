'From Cuis6.3 [latest update: #6231] on 21 February 2024 at 3:44:46 pm'!

!FileIOAccessor methodsFor: 'utilities' stamp: 'jmv 2/21/2024 15:19:59'!
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
		strm nextPutAll: baseDirectoryPathComponents.
		tokens do: [ :each |
			each = '.' ifFalse: [ strm nextPut: each ]]].! !

