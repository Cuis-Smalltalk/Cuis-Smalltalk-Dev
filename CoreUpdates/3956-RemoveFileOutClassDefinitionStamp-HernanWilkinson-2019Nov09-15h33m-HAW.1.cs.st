'From Cuis 5.0 [latest update: #3955] on 9 November 2019 at 3:37:56 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'HAW 11/9/2019 15:35:45'!
fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex
	"File a description of the receiver on aFileStream. If the boolean 
	argument, moveSource, is true, then set the trailing bytes to the position 
	of aFileStream and to fileIndex in order to indicate where to find the 
	source code."

	aFileStream nextPut: $!!; nextChunkPut: self definitionPreambleWithoutStamp; newLine.
	aFileStream nextChunkPut: self definition.

	self organization
		putCommentOnFile: aFileStream
		numbered: fileIndex
		moveSource: moveSource
		forClass: self.
	self organization categories do: [ :heading |
		self fileOutCategory: heading
			on: aFileStream
			moveSource: moveSource
			toFile: fileIndex]! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'HAW 11/9/2019 15:35:19'!
fileOutPSFor: class on: stream 
	"Write out removals and initialization for this class."

	| dict classRecord currentDef |
	classRecord _ changeRecords at: class name ifAbsent: [^ self].
	dict _ classRecord methodChangeTypes.
	((dict includesKey:  #initialize) and: [ class isMeta ]) ifTrue: [
		stream nextChunkPut: class soleInstance name, ' initialize'; newLine].
	((classRecord includesChangeType: #change)
			and: [(currentDef _ class definition) ~= (self fatDefForClass: class)]) ifTrue: [
		stream
			nextPut: $!!;
			nextChunkPut: class definitionPreambleWithoutStamp; newLine;
			nextChunkPut: currentDef; newLine].
	(classRecord includesChangeType: #reorganize) ifTrue: [
		class fileOutOrganizationOn: stream.
		stream newLine]! !

!ChangeSet methodsFor: 'private' stamp: 'HAW 11/9/2019 15:37:00'!
fileOutClassDefinition: class on: stream 
	"Write out class definition for the given class on the given stream, if the class definition was added or changed."

	(self atClass: class includes: #rename) ifTrue: [
		stream nextChunkPut: 'Smalltalk renameClassNamed: #', (self oldNameFor: class), ' as: #', class name; newLine].

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
		stream newLine].

! !

