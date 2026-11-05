'From Cuis7.9 [latest update: #8314] on 4 November 2026 at 4:16:49 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 4/Nov/2026, 7:13:43 pm (UTC)'!
fileOutChangedMessages: aSet on: aFileStream toFile: fileIndex
	"File a description of the messages of this class that have been
	changed (i.e., are entered into the argument, aSet) onto aFileStream.  If
	moveSource, is true, then set the method source pointer to the new file position.
	Note when this method is called with moveSource=true, it is condensing the
	.changes file, and should only write a preamble for every method."

	| org categories |

	org := self organization.
	categories := org categories.

	categories ifNotNil: [ categories do: [ :cat |  | sels |
		sels := (org listAtCategoryNamed: cat) select: [:sel | aSet includes: sel].
		sels do: [:sel |
			self printMethodChunk: sel withPreamble: true on: aFileStream moveSource: false toFile: fileIndex]]]! !


!BasicClassOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 4/Nov/2026, 7:08:40 pm (UTC)'!
fileOutCommentOn: aFileStream toFile: fileIndex
	"Copy the class comment to aFileStream.  If moveSource is true (as in compressChanges or compressSources, then update classComment to point to the new file."

	self hasClassComment ifTrue: [
		aFileStream newLine.
		RemoteString newString: self classComment
						onFileNumber: fileIndex toFile: aFileStream.
		Smalltalk userChangesAppendDo: [ :stream |
			stream newLine; nextChunkPut: self classComment ]].! !

!BasicClassOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 4/Nov/2026, 7:09:16 pm (UTC)'!
putCommentOnFile: aFileStream numbered: sourceIndex forClass: aClass
	"Store the comment about the class onto file, aFileStream."
	| header |
	self hasClassComment ifTrue: [
		aFileStream newLine; nextPut: $!!.
		header := String streamContents: [:strm | 
				strm nextPutAll: aClass name;
				nextPutAll: ' commentStamp: '.
				commentStamp ifNil: [commentStamp := '<historical>'].
				commentStamp storeOn: strm.
				strm nextPutAll: ' prior: '; nextPutAll: '0'].
		aFileStream nextChunkPut: header.
		aClass organization fileOutCommentOn: aFileStream toFile: sourceIndex.
		aFileStream newLine]! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 4/Nov/2026, 7:13:48 pm (UTC)'!
fileOutChangedMessages: aSet on: aFileStream 
	"File a description of the messages of the receiver that have been 
	changed (i.e., are entered into the argument, aSet) onto aFileStream."

	self fileOutChangedMessages: aSet
		on: aFileStream
		toFile: 0! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 4/Nov/2026, 7:09:33 pm (UTC)'!
fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex initializing: ignoredBool sortMethodsByCategory: byCategory
	"File a description of the receiver on aFileStream. If the boolean
	argument, moveSource, is true, then set the trailing bytes to the position
	of aFileStream and to fileIndex in order to indicate where to find the
	source code."

	| categories |

	aFileStream nextPut: $!!; nextChunkPut: self definitionPreambleWithoutStamp; newLine.
	aFileStream nextChunkPut: self definition.

	self organization
		putCommentOnFile: aFileStream
		numbered: fileIndex
		forClass: self.
	categories := byCategory
		ifTrue: [self organization categories ifNil: [#()]]
		ifFalse: [{ClassOrganizer allCategory}].
	categories do: [ :heading |
		self fileOutCategory: heading
			on: aFileStream
			moveSource: moveSource
			toFile: fileIndex ].! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 4/Nov/2026, 7:09:51 pm (UTC)'!
write: classes classCommentsOn: aStream

	classes do: [ :class |
		class organization classComment isEmpty ifFalse: [
			class organization
				putCommentOnFile: aStream
				numbered: 0
				forClass: class ]]! !


!ChangeSet methodsFor: 'private' stamp: 'jmv 4/Nov/2026, 7:09:57 pm (UTC)'!
fileOutClassDefinition: class on: stream
	"Write out class definition for the given class on the given stream, if the class definition was added or changed."

	(self atClass: class includes: #rename) ifTrue: [
		(ClassRenamedChange from: (self oldNameFor: class) to: class name stamp: nil) fileOutOn: stream.
		stream newLine ].

	(self atClass: class includes: #change) ifTrue: [ "fat definition only needed for changes"
		stream
			nextPut: $!!; nextChunkPut: class definitionPreambleWithoutStamp; newLine;
			nextChunkPut: (self fatDefForClass: class); newLine.
	] ifFalse: [
		(self atClass: class includes: #add) ifTrue: [ "use current definition for add"
			stream
				nextPut: $!!; nextChunkPut: class definitionPreambleWithoutStamp; newLine;
				nextChunkPut: class definition; newLine.
		].
	].

	(self atClass: class includes: #comment) ifTrue: [
		class theNonMetaClass organization putCommentOnFile: stream numbered: 0 forClass: class theNonMetaClass.
		stream newLine].! !

!methodRemoval: BasicClassOrganizer #putCommentOnFile:numbered:moveSource:forClass: stamp: 'jmv 4/Nov/2026, 7:10:02 pm (UTC)'!
BasicClassOrganizer removeSelector: #putCommentOnFile:numbered:moveSource:forClass:!
!methodRemoval: BasicClassOrganizer #fileOutCommentOn:moveSource:toFile: stamp: 'jmv 4/Nov/2026, 7:08:59 pm (UTC)'!
BasicClassOrganizer removeSelector: #fileOutCommentOn:moveSource:toFile:!
!methodRemoval: ClassDescription #fileOutChangedMessages:on:moveSource:toFile: stamp: 'jmv 4/Nov/2026, 7:13:59 pm (UTC)'!
ClassDescription removeSelector: #fileOutChangedMessages:on:moveSource:toFile:!
