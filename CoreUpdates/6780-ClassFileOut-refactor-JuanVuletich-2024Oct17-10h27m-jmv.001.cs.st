'From Cuis7.1 [latest update: #6779] on 17 October 2024 at 12:00:32 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 10/17/2024 11:55:56'!
fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex initializing: ignoredBool
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
		moveSource: moveSource
		forClass: self.
	categories := self organization categories.
	categories ifNotNil: [ categories do: [ :heading |
		self fileOutCategory: heading
			on: aFileStream
			moveSource: moveSource
			toFile: fileIndex]]! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 10/17/2024 11:58:16'!
fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex 
	"File a description of the receiver on aFileStream. If the boolean argument,
	moveSource, is true, then set the trailing bytes to the position of aFileStream and
	to fileIndex in order to indicate where to find the source code."

	^self fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex initializing: true! !


!Class methodsFor: 'fileIn/Out' stamp: 'jmv 10/17/2024 11:56:22'!
fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex initializing: aBool
	"File a description of the receiver on aFileStream. If the boolean argument,
	moveSource, is true, then set the trailing bytes to the position of aFileStream and
	to fileIndex in order to indicate where to find the source code."

	Transcript newLine; show: name.
	super
		fileOutOn: aFileStream
		moveSource: moveSource
		toFile: fileIndex
		initializing: nil.
	self class nonTrivial
		ifTrue: [
			aFileStream newLine; nextPutAll: '"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!!'; newLine; newLine.
			self class
				fileOutOn: aFileStream
				moveSource: moveSource
				toFile: fileIndex
				initializing: aBool]! !


!Metaclass methodsFor: 'fileIn/Out' stamp: 'jmv 10/17/2024 11:56:38'!
fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex initializing: aBool
	super
		fileOutOn: aFileStream
		moveSource: moveSource
		toFile: fileIndex
		initializing: nil.
	(aBool and:[moveSource not and: [self methodDict includesKey: #initialize]]) ifTrue: [
		aFileStream newLine.
		aFileStream newLine.
		aFileStream nextChunkPut: thisClass name , ' initialize'.
		aFileStream newLine ]! !

!methodRemoval: Metaclass #fileOutOn:moveSource:toFile: stamp: 'jmv 10/17/2024 11:58:25'!
Metaclass removeSelector: #fileOutOn:moveSource:toFile:!
!methodRemoval: Class #fileOutOn:moveSource:toFile: stamp: 'jmv 10/17/2024 11:58:27'!
Class removeSelector: #fileOutOn:moveSource:toFile:!
