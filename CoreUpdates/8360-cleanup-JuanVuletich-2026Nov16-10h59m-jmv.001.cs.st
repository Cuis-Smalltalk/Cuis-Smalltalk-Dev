'From Cuis7.9 [latest update: #8358] on 16 November 2026 at 11:03:08 am'!

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 14/Nov/2026, 12:04:51 pm (UTC)'!
writeMethodPreambleToChangesFile: file class: class category: catName withStamp: changeStamp

	class
		printCategoryChunk: catName
		on: file
		withStamp: changeStamp.
	self
		withPropertiesDo: [ :properties |
			properties hasAnyProperty ifTrue: [
				class printPropertiesChunk: properties on: file ]]
		ifSelector: [ :sel | nil ].
	file newLine.! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 14/Nov/2026, 12:06:35 pm (UTC)'!
writeToChangesFile: sourceStr class: class category: catName withStamp: changeStamp oldMethodOrNil: existingMethod

	"Store the source code for the receiver on the external .changes file.
	If no sources are available, i.e., SourceFile is nil, do nothing.
	Store a 4-byte source code pointer at the method end."

	| fileIndex file position libIndex |

	Smalltalk userChangesAppendDo: [ :stream |
		self writeMethodPreambleToChangesFile: stream class: class category: catName withStamp: changeStamp.
		stream nextChunkPut: sourceStr.
		stream nextChunkPut: ' ' ].

	fileIndex := 2.
	(SourceFiles notNil and: [(file := SourceFiles at: fileIndex) notNil]) ifTrue: [
		file setToEnd.
		self writeMethodPreambleToChangesFile: file class: class category: catName withStamp: changeStamp.
		file padSpacesTo: SourceFiles pointerScaleForWriting.
		position := file position.
		file nextChunkPut: sourceStr asPlainString.
		file nextChunkPut: ' '.
		"If we are doing a FileIn, don't flush for each method.
		This is a performance optimization, to make file in of large chunks of code faster.
		It means that the source code and the time stamp (and rest of preamble) won't
		be available for reading until the file in finishes.
		This is usually not a problem."
		InMidstOfFileinNotification signal ifFalse: [file flush].
		
		
		libIndex := 16rFFFFFFFE . "Changes"
		CodeHistory current addMethodVersion: self libraryIndex: libIndex positionIndex: position oldVersionOrNil: existingMethod.
		
		SourceFiles checkOKToAdd: sourceStr size at: position in: fileIndex ].! !

!methodRemoval: CompiledMethod #writeToChangesFile:withPreamble:oldMethodOrNil: stamp: 'jmv 16/Nov/2026, 2:00:25 pm (UTC)'!
CompiledMethod removeSelector: #writeToChangesFile:withPreamble:oldMethodOrNil:!
