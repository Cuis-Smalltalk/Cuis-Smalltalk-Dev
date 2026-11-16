'From Cuis7.9 [latest update: #8342] on 12 November 2026 at 4:48:05 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 7:35:47 pm (UTC)'!
fileOutCategory: aSymbol on: aFileStream
	"File a description of the receiver's category, aString, onto aFileStream."

	| selectors |

	aFileStream newLine.
	selectors := (aSymbol isNil or: [aSymbol == ClassOrganizer allCategory])
				ifTrue: [ self organization allMethodSelectors ]
				ifFalse: [ self organization listAtCategoryNamed: aSymbol ].

	"Overridden to preserve author stamps in sources file regardless"
	selectors do: [ :sel |
		self printMethodChunk: sel
			withPreamble: true
			on: aFileStream ].! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 7:36:40 pm (UTC)'!
fileOutOn: aFileStream initializing: ignoredBool sortMethodsByCategory: byCategory
	"File a description of the receiver on aFileStream."

	| categories |

	aFileStream nextPut: $!!; nextChunkPut: self definitionPreambleWithoutStamp; newLine.
	aFileStream nextChunkPut: self definition.

	self organization
		putCommentOnFile: aFileStream
		forClass: self.
	categories := byCategory
		ifTrue: [self organization categories ifNil: [#()]]
		ifFalse: [{ClassOrganizer allCategory}].
	categories do: [ :heading |
		self fileOutCategory: heading
			on: aFileStream ].! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 7:46:28 pm (UTC)'!
printMethodChunk: selector withPreamble: doPreamble on: outStream
	"Copy the source code for the method associated with selector onto the fileStream."

	| compiledMethod |
	compiledMethod := self methodDict at: selector ifAbsent: [
		outStream nextPutAll: selector; newLine.
		outStream tab; nextPutAll: '** ERROR  -  THIS METHOD IS MISSING ** '; newLine; newLine.
		outStream nextPutAll: '  '.
		^ self ].

	self printMethodChunkFor: compiledMethod withPreamble: doPreamble on: outStream padForPointerScale: false.! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 7:46:04 pm (UTC)'!
printMethodChunkFor: compiledMethod withPreamble: doPreamble on: outStream padForPointerScale: doPad
	"Copy the source code for the method associated with selector onto the fileStream."

	| oldPos newPos sourceFile methodCode methodCodeText codeStyler |

	doPreamble ifTrue: [
		self
			printCategoryChunk: (self organization categoryOfElement: compiledMethod selector)
			on: outStream
			withStamp: compiledMethod authorAndStampUTC.
		compiledMethod
			withPropertiesDo: [ :properties |
				properties hasAnyProperty ifTrue: [
					self printPropertiesChunk: properties on: outStream ]]
			ifSelector: [ :sel | nil ].
		outStream newLine ].

	((compiledMethod fileIndex = 0
		or: [(SourceFiles at: compiledMethod fileIndex) == nil])
		or: [(oldPos := compiledMethod filePosition) = 0])
			ifTrue: [
				"The source code is not accessible.  We must decompile..."
				outStream nextChunkPut: compiledMethod decompileString.
				newPos := 0 ]
			ifFalse: [
				sourceFile := SourceFiles at: compiledMethod fileIndex.
				sourceFile position: oldPos.
				"Copy the method chunk"
				doPad ifTrue: [
					outStream padSpacesTo: SourceFiles pointerScaleForWriting ].
				"Copy method chunk"
				methodCode := sourceFile nextChunk.
				(methodCode isEmpty or: [ methodCode isSeparators ]) ifTrue: [
					"This is very unlikely, but possible source file is corrupted."
					methodCode := compiledMethod decompileString ].
				"Convert method code to use ANSI assignments"
				(Preferences at: #fileOutANSIassignment) ifTrue: [
					codeStyler := SHTextStylerST80 new.
					codeStyler classOrMetaClass: compiledMethod methodClass.
					methodCodeText := methodCode asText.
					codeStyler instVarNamed: 'formattedText' put: methodCodeText.
					codeStyler getReady.
					codeStyler parseText.
					codeStyler replaceStringForRangesWithType: #assignment with: ':=' offset: 0.
					methodCode := methodCodeText string ].
				newPos := outStream position.
				outStream nextChunkPut: methodCode ].
	doPreamble ifTrue: [ outStream nextChunkPut: ' ' ].
	outStream newLine.
	^newPos! !


!Class methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 7:37:52 pm (UTC)'!
fileOutOn: aFileStream initializing: aBool sortMethodsByCategory: byCategory
	"File a description of the receiver on aFileStream."

	Transcript newLine; show: name.
	super
		fileOutOn: aFileStream
		initializing: nil
		sortMethodsByCategory: byCategory.
	self class nonTrivial
		ifTrue: [
			aFileStream newLine; nextPutAll: '"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!!'; newLine; newLine.
			self class
				fileOutOn: aFileStream
				initializing: aBool
				sortMethodsByCategory: byCategory ].! !


!Metaclass methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 7:37:38 pm (UTC)'!
fileOutOn: aFileStream initializing: aBool sortMethodsByCategory: byCategory
	super
		fileOutOn: aFileStream
		initializing: nil
		sortMethodsByCategory: byCategory.
	aBool ifTrue: [
		self fileOutInitializerOn: aFileStream ].! !


!BasicClassOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 7:32:38 pm (UTC)'!
fileOutCommentOn: aFileStream
	"Copy the class comment to aFileStream."

	self hasClassComment ifTrue: [
		aFileStream newLine.
		aFileStream nextChunkPut: self classComment asPlainString.
		Smalltalk userChangesAppendDo: [ :stream |
			stream newLine; nextChunkPut: self classComment ]].! !

!BasicClassOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 7:32:59 pm (UTC)'!
putCommentOnFile: aFileStream forClass: aClass
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
		aClass organization fileOutCommentOn: aFileStream.
		aFileStream newLine ].! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 7:35:53 pm (UTC)'!
fileOutCategory: catName
	"FileOut the named category"

	DirectoryEntry fileOutsDirQuery: (self name , '-' , catName , '.st') writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self fileOutCategory: catName on: fileStream ].! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 7:46:21 pm (UTC)'!
fileOutCategory: aSymbol onNewSources: aFileStream
	"File a description of the receiver's category, aString, onto a new .sources file.
	Set the trailing bytes of each method to the position of aFileStream
	and to fileIndex in order to indicate where to find the source code."

	| selectors newPos |

	aFileStream newLine.
	selectors := (aSymbol isNil or: [aSymbol == ClassOrganizer allCategory])
				ifTrue: [ self organization allMethodSelectors ]
				ifFalse: [ self organization listAtCategoryNamed: aSymbol ].

	selectors do: [ :sel |
		self methodDict at: sel
			ifPresent: [ :compiledMethod |
				newPos := self printMethodChunkFor: compiledMethod withPreamble: true on: aFileStream padForPointerScale: true.
				newPos > 0 ifTrue: [
					"Set the new method source pointer"
					compiledMethod setSourcePosition: newPos inFile: 1 ]]].! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 7:40:55 pm (UTC)'!
fileOutChangedMessages: aSet on: aFileStream
	"File a description of the messages of this class that have been
	changed (i.e., are entered into the argument, aSet) onto aFileStream."

	| org categories |

	org := self organization.
	categories := org categories.

	categories ifNotNil: [ categories do: [ :cat |  | sels |
		sels := (org listAtCategoryNamed: cat) select: [ :sel | aSet includes: sel ].
		sels do: [ :sel |
			self printMethodChunk: sel withPreamble: true on: aFileStream ]]].! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 7:35:00 pm (UTC)'!
fileOutMethod: selector
	"Write source code of a single method on a file.  Make up a name for the file."

	| nameBody |
	(selector == #Comment) ifTrue: [^ self inform: 'Sorry, cannot file out class comment in isolation.'].
	(self includesSelector: selector) ifFalse: [^ self error: 'Selector ', selector asPlainString, ' not found'].
	nameBody := self name , '-' , (selector copyReplaceAll: ':' with: '').
	DirectoryEntry fileOutsDirQuery: (nameBody asFileName, '.st') writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self printMethodChunk: selector withPreamble: true on: fileStream ].! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 7:36:47 pm (UTC)'!
fileOutOn: aFileStream
	"File a description of the receiver on aFileStream."

	self
		fileOutOn: aFileStream
		initializing: true
		sortMethodsByCategory: true.! !


!Class methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 7:36:51 pm (UTC)'!
fileOut
	"File a description of the receiver onto a new file whose base name is the name of the receiver.
	Method ordering is by method categories, and in each category, alphabetical."

	DirectoryEntry fileOutsDirQuery: (self name, '.st') writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self sharedPools size > 0 ifTrue: [
			self shouldFileOutPools
				ifTrue: [ self fileOutSharedPoolsOn: fileStream ]].
		self fileOutOn: fileStream
			initializing: true
			sortMethodsByCategory: true ].! !

!Class methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 7:36:57 pm (UTC)'!
fileOutAlphabetically
	"File a description of the receiver onto a new file whose base name is the name of the receiver.
	Method ordering is alphabetical, igoring categories."

	DirectoryEntry fileOutsDirQuery: (self name, '.st') writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self sharedPools size > 0 ifTrue: [
			self shouldFileOutPools
				ifTrue: [ self fileOutSharedPoolsOn: fileStream ]].
		self fileOutOn: fileStream initializing: true sortMethodsByCategory: false ].! !

!Class methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 7:37:01 pm (UTC)'!
fileOutHierarchy
	"File a description of the receiver onto a new file whose base name is the name of the receiver.
	Morph fileOutHierarchy
	"
	DirectoryEntry fileOutsDirQuery: (self name, '-hierarchy.st') writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self sharedPools size > 0 ifTrue: [
			self shouldFileOutPools
				ifTrue: [ self fileOutSharedPoolsOn: fileStream ]].
		self withAllSubclassesDo: [ :each |
			each fileOutOn: fileStream
				initializing: true
				sortMethodsByCategory: true ]].! !


!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 7:38:07 pm (UTC)'!
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
			initializing: false
			sortMethodsByCategory: true ].
	aBool ifTrue: [classes do: [ :cls | cls fileOutInitializerOn: aFileStream ]].! !

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 7:38:11 pm (UTC)'!
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
			initializing: false
			sortMethodsByCategory: true ].! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 12/Nov/2026, 7:35:05 pm (UTC)'!
writeMethod: methodReference on: aStream
	methodReference actualClass
		printMethodChunk: methodReference methodSymbol
		withPreamble: true
		on: aStream.! !


!CodeProvider methodsFor: 'message list menu' stamp: 'jmv 12/Nov/2026, 7:35:10 pm (UTC)'!
fileOutMethod

	self selectedMessageName ifNotNil: [
		^self selectedClassOrMetaClass fileOutMethod: self selectedMessageName].

	self messageList ifNotNil: [ :theMethods |
		DirectoryEntry fileOutsDirQuery: 'methods.st' writeStreamDo: [ :fileStream |
			fileStream writeSystemTimeStamp.
			theMethods do: [ :methodRef |
				methodRef actualClass
					printMethodChunk: methodRef methodSymbol
					withPreamble: true
					on: fileStream ]]].! !


!MethodSet methodsFor: 'message functions' stamp: 'jmv 12/Nov/2026, 7:35:14 pm (UTC)'!
fileOutAllMethods

	DirectoryEntry fileOutsDirQuery: 'methods.st' writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self messageList do: [ :methodRef |
			methodRef actualClass
				printMethodChunk: methodRef methodSymbol
				withPreamble: true
				on: fileStream ]].! !


!ChangeList methodsFor: 'menu actions' stamp: 'jmv 12/Nov/2026, 7:35:21 pm (UTC)'!
fileOutCurrentVersionsOfSelections

	DirectoryEntry fileOutsDirQuery: 'CurrentVersions.st' writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self currentVersionsOfSelections do: [ :methodRef |
			methodRef actualClass
				printMethodChunk: methodRef methodSymbol
				withPreamble: true
				on: fileStream ]].! !

!methodRemoval: Metaclass #fileOutOn:toFile:initializing:sortMethodsByCategory: stamp: 'jmv 12/Nov/2026, 7:38:30 pm (UTC)'!
Metaclass removeSelector: #fileOutOn:toFile:initializing:sortMethodsByCategory:!
!methodRemoval: Class #fileOutOn:toFile:initializing:sortMethodsByCategory: stamp: 'jmv 12/Nov/2026, 7:38:31 pm (UTC)'!
Class removeSelector: #fileOutOn:toFile:initializing:sortMethodsByCategory:!
!methodRemoval: ClassDescription #printMethodChunkFor:withPreamble:on:toFile: stamp: 'jmv 12/Nov/2026, 7:46:33 pm (UTC)'!
ClassDescription removeSelector: #printMethodChunkFor:withPreamble:on:toFile:!
!methodRemoval: ClassDescription #fileOutChangedMessages:on:toFile: stamp: 'jmv 12/Nov/2026, 7:41:00 pm (UTC)'!
ClassDescription removeSelector: #fileOutChangedMessages:on:toFile:!
!methodRemoval: ClassDescription #printMethodChunk:withPreamble:on:toFile: stamp: 'jmv 12/Nov/2026, 7:35:29 pm (UTC)'!
ClassDescription removeSelector: #printMethodChunk:withPreamble:on:toFile:!
!methodRemoval: ClassDescription #fileOutCategory:on:toFile: stamp: 'jmv 12/Nov/2026, 7:39:18 pm (UTC)'!
ClassDescription removeSelector: #fileOutCategory:on:toFile:!
!methodRemoval: ClassDescription #fileOutOn:toFile:initializing:sortMethodsByCategory: stamp: 'jmv 12/Nov/2026, 7:38:32 pm (UTC)'!
ClassDescription removeSelector: #fileOutOn:toFile:initializing:sortMethodsByCategory:!
