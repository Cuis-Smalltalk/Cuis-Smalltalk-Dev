'From Cuis7.9 [latest update: #8341] on 12 November 2026 at 2:10:58 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 4:55:15 pm (UTC)'!
fileOutCategory: aSymbol on: aFileStream toFile: fileIndex 
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
			on: aFileStream
			toFile: fileIndex ].! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 4:57:48 pm (UTC)'!
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
				newPos := self printMethodChunkFor: compiledMethod withPreamble: true on: aFileStream toFile: 1.
				newPos > 0 ifTrue: [
					"Set the new method source pointer"
					compiledMethod setSourcePosition: newPos inFile: 1 ]]].! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 5:08:06 pm (UTC)'!
fileOutOn: aFileStream toFile: fileIndex initializing: ignoredBool sortMethodsByCategory: byCategory
	"File a description of the receiver on aFileStream."

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
			toFile: fileIndex ].! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 5:07:44 pm (UTC)'!
fileOutOnNewSources: aFileStream
	"File a description of the receiver on a new .sources file.
	Set the trailing bytes of each method to the position of aFileStream
	and to fileIndex in order to indicate where to find the source code."

	| categories |

	aFileStream nextPut: $!!; nextChunkPut: self definitionPreambleWithoutStamp; newLine.
	aFileStream nextChunkPut: self definition.

	self organization
		putCommentOnFile: aFileStream
		numbered: 1
		forClass: self.
	categories := self organization categories ifNil: [#()].
	categories do: [ :heading |
		self fileOutCategory: heading
			onNewSources: aFileStream ].! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 5:08:19 pm (UTC)'!
printMethodChunk: selector withPreamble: doPreamble on: outStream toFile: fileIndex
	"Copy the source code for the method associated with selector onto the fileStream."

	| compiledMethod |
	compiledMethod := self methodDict at: selector ifAbsent: [
		outStream nextPutAll: selector; newLine.
		outStream tab; nextPutAll: '** ERROR  -  THIS METHOD IS MISSING ** '; newLine; newLine.
		outStream nextPutAll: '  '.
		^ self ].

	self printMethodChunkFor: compiledMethod withPreamble: doPreamble on: outStream toFile: fileIndex.! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 5:00:12 pm (UTC)'!
printMethodChunkFor: compiledMethod withPreamble: doPreamble on: outStream toFile: fileIndex
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
				fileIndex = 0 ifFalse: [
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


!Class methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 5:01:12 pm (UTC)'!
fileOutOn: aFileStream toFile: fileIndex initializing: aBool sortMethodsByCategory: byCategory
	"File a description of the receiver on aFileStream."

	Transcript newLine; show: name.
	super
		fileOutOn: aFileStream
		toFile: fileIndex
		initializing: nil
		sortMethodsByCategory: byCategory.
	self class nonTrivial
		ifTrue: [
			aFileStream newLine; nextPutAll: '"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!!'; newLine; newLine.
			self class
				fileOutOn: aFileStream
				toFile: fileIndex
				initializing: aBool
				sortMethodsByCategory: byCategory ].! !

!Class methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 5:08:34 pm (UTC)'!
fileOutOnNewSources: aFileStream
	"File a description of the receiver on a new .sources file.
	Set the trailing bytes of each method to the position of aFileStream
	and to fileIndex in order to indicate where to find the source code."

	Transcript newLine; show: name.
	super fileOutOnNewSources: aFileStream.
	self class nonTrivial
		ifTrue: [
			aFileStream newLine; nextPutAll: '"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!!'; newLine; newLine.
			self class fileOutOnNewSources: aFileStream ].! !


!Metaclass methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 3:37:09 pm (UTC)'!
fileOutOn: aFileStream toFile: fileIndex initializing: aBool sortMethodsByCategory: byCategory
	super
		fileOutOn: aFileStream
		toFile: fileIndex
		initializing: nil
		sortMethodsByCategory: byCategory.
	aBool ifTrue: [
		self fileOutInitializerOn: aFileStream ].! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 2:10:56 pm (UTC)'!
fileOutCategory: catName
	"FileOut the named category"

	DirectoryEntry fileOutsDirQuery: (self name , '-' , catName , '.st') writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self fileOutCategory: catName on: fileStream toFile: 0 ].! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 4:58:41 pm (UTC)'!
fileOutChangedMessages: aSet on: aFileStream toFile: fileIndex
	"File a description of the messages of this class that have been
	changed (i.e., are entered into the argument, aSet) onto aFileStream."

	| org categories |

	org := self organization.
	categories := org categories.

	categories ifNotNil: [ categories do: [ :cat |  | sels |
		sels := (org listAtCategoryNamed: cat) select: [ :sel | aSet includes: sel ].
		sels do: [ :sel |
			self printMethodChunk: sel withPreamble: true on: aFileStream toFile: fileIndex ]]].! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 2:11:55 pm (UTC)'!
fileOutMethod: selector
	"Write source code of a single method on a file.  Make up a name for the file."

	| nameBody |
	(selector == #Comment) ifTrue: [^ self inform: 'Sorry, cannot file out class comment in isolation.'].
	(self includesSelector: selector) ifFalse: [^ self error: 'Selector ', selector asPlainString, ' not found'].
	nameBody := self name , '-' , (selector copyReplaceAll: ':' with: '').
	DirectoryEntry fileOutsDirQuery: (nameBody asFileName, '.st') writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self printMethodChunk: selector withPreamble: true
			on: fileStream toFile: 0 ].! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 2:34:52 pm (UTC)'!
fileOutOn: aFileStream
	"File a description of the receiver on aFileStream."

	self
		fileOutOn: aFileStream
		toFile: 0
		initializing: true
		sortMethodsByCategory: true.! !


!Class methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 2:35:37 pm (UTC)'!
fileOut
	"File a description of the receiver onto a new file whose base name is the name of the receiver.
	Method ordering is by method categories, and in each category, alphabetical."

	DirectoryEntry fileOutsDirQuery: (self name, '.st') writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self sharedPools size > 0 ifTrue: [
			self shouldFileOutPools
				ifTrue: [ self fileOutSharedPoolsOn: fileStream ]].
		self fileOutOn: fileStream
			toFile: 0 
			initializing: true
			sortMethodsByCategory: true ].! !

!Class methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 2:13:09 pm (UTC)'!
fileOutAlphabetically
	"File a description of the receiver onto a new file whose base name is the name of the receiver.
	Method ordering is alphabetical, igoring categories."

	DirectoryEntry fileOutsDirQuery: (self name, '.st') writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self sharedPools size > 0 ifTrue: [
			self shouldFileOutPools
				ifTrue: [ self fileOutSharedPoolsOn: fileStream ]].
		self fileOutOn: fileStream toFile: 0 initializing: true sortMethodsByCategory: false ].! !

!Class methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 2:35:51 pm (UTC)'!
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
				toFile: 0
				initializing: true
				sortMethodsByCategory: true ]].! !


!BasicClassOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 5:07:02 pm (UTC)'!
fileOutCommentOn: aFileStream toFile: fileIndex
	"Copy the class comment to aFileStream."

	self hasClassComment ifTrue: [
		aFileStream newLine.
		RemoteString newString: self classComment
						onFileNumber: fileIndex toFile: aFileStream.
		Smalltalk userChangesAppendDo: [ :stream |
			stream newLine; nextChunkPut: self classComment ]].! !


!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 3:35:54 pm (UTC)'!
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
			toFile: 0
			initializing: false
			sortMethodsByCategory: true ].
	aBool ifTrue: [classes do: [ :cls | cls fileOutInitializerOn: aFileStream ]].! !

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 2:13:44 pm (UTC)'!
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
			toFile: 0
			initializing: false
			sortMethodsByCategory: true ].! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 12/Nov/2026, 4:01:26 pm (UTC)'!
condenseSources: newVersionString
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseSources"

	| classCount oldChanges oldChangesLocalName oldChangesPathName newChangesPathName newSourcesName |
	SourceFileVersionString := newVersionString.

	"Write all sources with fileIndex 1"
	newSourcesName := self defaultSourcesName.
	newSourcesName asFullFileEntry writeStreamDo: [ :newSources|
		newSources writeSystemTimeStamp.
		'Condensing Sources File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: self classNames size
			during: [ :barBlock |
				classCount := 0.
				self allClassesDo: [ :class |
					barBlock value: (classCount := classCount + 1).
					class fileOutOnNewSources: newSources ]]].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e historyPointer: 0 ] ].
	CodeHistory initialize.

	"Make a new empty changes file"
	oldChanges := SourceFiles at: 2.
	oldChangesPathName := oldChanges name.
	oldChangesLocalName := oldChanges localName.
	self closeSourceFiles.
	oldChangesPathName ifNotNil: [
		(oldChangesPathName, '.old') asFullFileEntry delete.
	oldChangesPathName asFullFileEntry rename: oldChangesLocalName, '.old' ].
	newChangesPathName := self defaultChangesName.
	newChangesPathName asFullFileEntry writeStreamDo: [ :stream |
		stream writeSystemTimeStamp ].
	LastQuitLogPosition := 0.

	self openSourceFiles.! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 12/Nov/2026, 2:13:52 pm (UTC)'!
writeMethod: methodReference on: aStream
	methodReference actualClass
		printMethodChunk: methodReference methodSymbol
		withPreamble: true
		on: aStream
		toFile: 0.! !


!CodeProvider methodsFor: 'message list menu' stamp: 'jmv 12/Nov/2026, 2:13:57 pm (UTC)'!
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
					on: fileStream
					toFile: 0 ]]].! !


!MethodSet methodsFor: 'message functions' stamp: 'jmv 12/Nov/2026, 2:14:02 pm (UTC)'!
fileOutAllMethods

	DirectoryEntry fileOutsDirQuery: 'methods.st' writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self messageList do: [ :methodRef |
			methodRef actualClass
				printMethodChunk: methodRef methodSymbol
				withPreamble: true
				on: fileStream
				toFile: 0 ]].! !


!ChangeList methodsFor: 'menu actions' stamp: 'jmv 12/Nov/2026, 2:14:06 pm (UTC)'!
fileOutCurrentVersionsOfSelections

	DirectoryEntry fileOutsDirQuery: 'CurrentVersions.st' writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self currentVersionsOfSelections do: [ :methodRef |
			methodRef actualClass
				printMethodChunk: methodRef methodSymbol
				withPreamble: true
				on: fileStream
				toFile: 0 ]].! !

!methodRemoval: Metaclass #fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory: stamp: 'jmv 12/Nov/2026, 5:10:34 pm (UTC)'!
Metaclass removeSelector: #fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory:!
!methodRemoval: Class #fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory: stamp: 'jmv 12/Nov/2026, 5:10:34 pm (UTC)'!
Class removeSelector: #fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory:!
!methodRemoval: ClassDescription #fileOutCategory:on:moveSource:toFile: stamp: 'jmv 12/Nov/2026, 5:10:34 pm (UTC)'!
ClassDescription removeSelector: #fileOutCategory:on:moveSource:toFile:!
!methodRemoval: ClassDescription #fileOutOn:moveSource:toFile: stamp: 'jmv 12/Nov/2026, 5:10:34 pm (UTC)'!
ClassDescription removeSelector: #fileOutOn:moveSource:toFile:!
!methodRemoval: ClassDescription #fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory: stamp: 'jmv 12/Nov/2026, 5:10:34 pm (UTC)'!
ClassDescription removeSelector: #fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory:!
!methodRemoval: ClassDescription #printMethodChunk:withPreamble:on:moveSource:toFile: stamp: 'jmv 12/Nov/2026, 5:10:34 pm (UTC)'!
ClassDescription removeSelector: #printMethodChunk:withPreamble:on:moveSource:toFile:!
