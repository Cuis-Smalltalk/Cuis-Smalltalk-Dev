'From Cuis7.7 [latest update: #7900] on 3 May 2026 at 6:29:31 pm'!

!ClassRenamedChange methodsFor: 'printing' stamp: 'jmv 3/May/2026 18:25:15'!
command
	^ 'Smalltalk renameClassNamed: #', previousName, ' as: #', newName! !

!ClassRenamedChange methodsFor: 'fileIn/fileOut' stamp: 'jmv 3/May/2026 18:25:44'!
fileOutOn: aFileStream
	"File the receiver out on the given file stream"

	aFileStream nextPut: $!!.
	aFileStream nextChunkPut: 'classRenamed: #', previousName, ' as: #', newName.
	aFileStream newLine.
	
	aFileStream nextChunkPut: self command.
	aFileStream newLine.! !


!ChangeSet methodsFor: 'private' stamp: 'jmv 3/May/2026 18:18:04'!
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
		class theNonMetaClass organization putCommentOnFile: stream numbered: 0 moveSource: false forClass: class theNonMetaClass.
		stream newLine].! !

