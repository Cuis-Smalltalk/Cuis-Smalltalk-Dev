'From Cuis7.1 [latest update: #6779] on 17 October 2024 at 12:08:57 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 10/17/2024 12:08:33'!
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
		moveSource: moveSource
		forClass: self.
	categories := byCategory
		ifTrue: [self organization categories ifNil: [#()]]
		ifFalse: [{ClassOrganizer allCategory}].
	categories do: [ :heading |
		self fileOutCategory: heading
			on: aFileStream
			moveSource: moveSource
			toFile: fileIndex ].! !


!Class methodsFor: 'fileIn/Out' stamp: 'jmv 10/17/2024 12:04:46'!
fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex initializing: aBool sortMethodsByCategory: byCategory
	"File a description of the receiver on aFileStream. If the boolean argument,
	moveSource, is true, then set the trailing bytes to the position of aFileStream and
	to fileIndex in order to indicate where to find the source code."

	Transcript newLine; show: name.
	super
		fileOutOn: aFileStream
		moveSource: moveSource
		toFile: fileIndex
		initializing: nil
		sortMethodsByCategory: byCategory.
	self class nonTrivial
		ifTrue: [
			aFileStream newLine; nextPutAll: '"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!!'; newLine; newLine.
			self class
				fileOutOn: aFileStream
				moveSource: moveSource
				toFile: fileIndex
				initializing: aBool
				sortMethodsByCategory: byCategory ].! !


!Metaclass methodsFor: 'fileIn/Out' stamp: 'jmv 10/17/2024 12:04:56'!
fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex initializing: aBool sortMethodsByCategory: byCategory
	super
		fileOutOn: aFileStream
		moveSource: moveSource
		toFile: fileIndex
		initializing: nil
		sortMethodsByCategory: byCategory.
	(aBool and:[moveSource not and: [self methodDict includesKey: #initialize]]) ifTrue: [
		aFileStream newLine.
		aFileStream newLine.
		aFileStream nextChunkPut: thisClass name , ' initialize'.
		aFileStream newLine ]! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 10/17/2024 12:06:05'!
fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex 
	"File a description of the receiver on aFileStream. If the boolean argument,
	moveSource, is true, then set the trailing bytes to the position of aFileStream and
	to fileIndex in order to indicate where to find the source code."

	^self
		fileOutOn: aFileStream
		moveSource: moveSource
		toFile: fileIndex
		initializing: true
		sortMethodsByCategory: true.! !


!Class methodsFor: 'fileIn/Out' stamp: 'jmv 10/17/2024 12:03:12'!
fileOut
	"File a description of the receiver onto a new file whose base name is the name of the receiver.
	Method ordering is by method categories, and in each category, alphabetical."

	DirectoryEntry fileOutsDirectory // (self name, '.st') writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			fileStream timeStamp.
			self sharedPools size > 0 ifTrue: [
				self shouldFileOutPools
					ifTrue: [ self fileOutSharedPoolsOn: fileStream ]].
			self fileOutOn: fileStream moveSource: false toFile: 0 ]].! !


!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 10/17/2024 12:06:23'!
fileOutCategory: category on: aFileStream initializing: aBool
	"Store on the file associated with aFileStream, all the classes associated 
	with the category and any requested shared pools."

	| first poolSet tempClass classes |
	classes := (self superclassOrderIn: category).
	poolSet := Set new.
	classes do: [ :class |
		class sharedPools do: [:eachPool | poolSet add: eachPool]].
	poolSet notEmpty ifTrue: [
		tempClass := Class new.
		tempClass shouldFileOutPools ifTrue:
			[poolSet := poolSet select: [:aPool | tempClass shouldFileOutPool: (Smalltalk keyAtIdentityValue: aPool)].
			poolSet do: [:aPool | tempClass fileOutPool: aPool onFileStream: aFileStream]]].
	first := true.
	classes do: [ :class | 
		first
			ifTrue: [first := false]
			ifFalse: [aFileStream newLine; nextPut: Character newPage; newLine].
		class
			fileOutOn: aFileStream
			moveSource: false
			toFile: 0
			initializing: false
			sortMethodsByCategory: true ].
	aBool ifTrue: [classes do:[:cls| cls fileOutInitializerOn: aFileStream]].! !

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 10/17/2024 12:06:32'!
fileOutCategoryNoPoolsNoInit: category on: aFileStream
	"Store on the file associated with aFileStream, all the classes associated 
	with the category and any requested shared pools."

	| first classes |
	classes := self superclassOrderIn: category.
	first := true.
	classes do: [ :class | 
		first
			ifTrue: [first := false]
			ifFalse: [aFileStream newLine; nextPut: Character newPage; newLine].
		class
			fileOutOn: aFileStream
			moveSource: false
			toFile: 0
			initializing: false
			sortMethodsByCategory: true ].! !

!methodRemoval: Metaclass #fileOutOn:moveSource:toFile:initializing: stamp: 'jmv 10/17/2024 12:06:51'!
Metaclass removeSelector: #fileOutOn:moveSource:toFile:initializing:!
!methodRemoval: Class #fileOutOn:moveSource:toFile:initializing: stamp: 'jmv 10/17/2024 12:06:51'!
Class removeSelector: #fileOutOn:moveSource:toFile:initializing:!
!methodRemoval: ClassDescription #fileOutOn:moveSource:toFile:initializing: stamp: 'jmv 10/17/2024 12:06:56'!
ClassDescription removeSelector: #fileOutOn:moveSource:toFile:initializing:!
