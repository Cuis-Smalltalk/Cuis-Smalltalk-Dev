'From Cuis6.0 [latest update: #6153] on 24 December 2023 at 8:45:54 am'!

!FileIOAccessor methodsFor: 'actions' stamp: 'jmv 7/27/2023 17:06:06'!
absolutePathComponentsFor: aString
	"Not complete, but OK in most cases.
	See comment at #isAbsolutePathName"

	| tokens curDirPathComponents |

	"Basic tilde expansion. Consider $~ to mean user's home folder, if possible.
	See, for example, https://www.gnu.org/software/bash/manual/html_node/Tilde-Expansion.html"
	(aString size > 0 and: [aString first = $~]) ifTrue: [
		Smalltalk at: #OSProcess
			ifPresent: [ :osProcess |
				(osProcess thisOSProcess environmentAt: 'HOME')
					ifNotNil: [ :homeVariable |
						^self absolutePathComponentsFor: homeVariable, (aString withoutPrefix: '~')]
					ifNil: [ self error: self unresolvedHomeEnvironmentVariableDescription ]]
			ifAbsent: [
				self error: self tildeFolderRequiresOSProcessDescription ]].

	tokens := aString asPathTokens.

	aString isAbsolutePathName ifTrue: [ ^ tokens asArray ].

	curDirPathComponents := DirectoryEntry currentDirectory pathComponents.
	aString = '.' ifTrue: [ ^ curDirPathComponents copy ].
	aString = '..' ifTrue:  [^ curDirPathComponents allButLast ].

	[ tokens notEmpty and: [ tokens first = '..' ]] whileTrue: [
		curDirPathComponents := curDirPathComponents allButLast.
		tokens removeFirst ].

	^ Array streamContents: [ :strm |
		strm nextPutAll: curDirPathComponents.
		tokens do: [ :each |
			each = '.' ifFalse: [ strm nextPut: each ]]].! !

