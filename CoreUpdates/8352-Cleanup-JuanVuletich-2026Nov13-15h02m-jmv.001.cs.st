'From Cuis7.9 [latest update: #8351] on 13 November 2026 at 3:04:57 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 13/Nov/2026, 5:42:45 pm (UTC)'!
fileOutOn: aFileStream initializing: ignoredBool sortMethodsByCategory: byCategory
	"File a description of the receiver on aFileStream."

	| categories |

	aFileStream nextPut: $!!; nextChunkPut: self definitionPreambleWithoutStamp; newLine.
	aFileStream nextChunkPut: self definition.

	self organization
		putCommentOnFile: aFileStream
		forClass: self
		padForPointerScale: false.
	categories := byCategory
		ifTrue: [self organization categories ifNil: [#()]]
		ifFalse: [{ClassOrganizer allCategory}].
	categories do: [ :heading |
		self fileOutCategory: heading
			on: aFileStream ].! !

!methodRemoval: BasicClassOrganizer #fileOutCommentOn: stamp: 'jmv 13/Nov/2026, 6:02:41 pm (UTC)'!
BasicClassOrganizer removeSelector: #fileOutCommentOn:!
!methodRemoval: BasicClassOrganizer #putCommentOnFile:forClass: stamp: 'jmv 13/Nov/2026, 6:02:36 pm (UTC)'!
BasicClassOrganizer removeSelector: #putCommentOnFile:forClass:!
