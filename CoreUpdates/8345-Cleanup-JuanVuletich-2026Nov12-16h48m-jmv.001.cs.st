'From Cuis7.9 [latest update: #8342] on 12 November 2026 at 4:53:47 pm'!

!BasicClassOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 7:50:36 pm (UTC)'!
fileOutCommentOn: aFileStream padForPointerScale: doPad
	"Copy the class comment to aFileStream."

	self hasClassComment ifTrue: [
		aFileStream newLine.
		doPad ifTrue: [
			aFileStream padSpacesTo: SourceFiles pointerScaleForWriting ].
		aFileStream nextChunkPut: self classComment asPlainString.
		Smalltalk userChangesAppendDo: [ :stream |
			stream newLine; nextChunkPut: self classComment ]].! !

!BasicClassOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 7:51:07 pm (UTC)'!
putCommentOnFile: aFileStream forClass: aClass padForPointerScale: doPad
	"Store the comment about the class onto file, aFileStream."
	| header |
	self hasClassComment ifTrue: [
		aFileStream newLine; nextPut: $!!.
		header := String streamContents: [:strm | 
				strm nextPutAll: aClass name;
				nextPutAll: ' commentStamp: '.
				commentStamp ifNil: [commentStamp := '<historical>'].
				commentStamp storeOn: strm ].
		aFileStream nextChunkPut: header.
		aClass organization fileOutCommentOn: aFileStream padForPointerScale: doPad.
		aFileStream newLine ].! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 7:51:37 pm (UTC)'!
fileOutOnNewSources: aFileStream
	"File a description of the receiver on a new .sources file.
	Set the trailing bytes of each method to the position of aFileStream
	and to fileIndex in order to indicate where to find the source code."

	| categories |

	aFileStream nextPut: $!!; nextChunkPut: self definitionPreambleWithoutStamp; newLine.
	aFileStream nextChunkPut: self definition.

	self organization
		putCommentOnFile: aFileStream
		forClass: self
		padForPointerScale: true.
	categories := self organization categories ifNil: [#()].
	categories do: [ :heading |
		self fileOutCategory: heading
			onNewSources: aFileStream ].! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 12/Nov/2026, 7:51:49 pm (UTC)'!
write: classes classCommentsOn: aStream

	classes do: [ :class |
		class organization classComment isEmpty ifFalse: [
			class organization
				putCommentOnFile: aStream
				forClass: class
				padForPointerScale: false ]]! !


!ChangeSet methodsFor: 'private' stamp: 'jmv 12/Nov/2026, 7:52:05 pm (UTC)'!
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
		class theNonMetaClass organization
			putCommentOnFile: stream forClass: class theNonMetaClass padForPointerScale: false.
		stream newLine].! !

!methodRemoval: BasicClassOrganizer #putCommentOnFile:numbered:forClass: stamp: 'jmv 12/Nov/2026, 7:52:11 pm (UTC)'!
BasicClassOrganizer removeSelector: #putCommentOnFile:numbered:forClass:!
!methodRemoval: BasicClassOrganizer #fileOutCommentOn:toFile: stamp: 'jmv 12/Nov/2026, 7:52:18 pm (UTC)'!
BasicClassOrganizer removeSelector: #fileOutCommentOn:toFile:!
