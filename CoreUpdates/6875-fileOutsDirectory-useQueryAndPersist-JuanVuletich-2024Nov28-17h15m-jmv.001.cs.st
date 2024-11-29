'From Cuis7.1 [latest update: #6872] on 28 November 2024 at 5:18:09 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 11/28/2024 16:50:45'!
fileOutCategory: catName
	"FileOut the named category"

	DirectoryEntry fileOutsDirQuery: (self name , '-' , catName , '.st') writeStreamDo: [ :fileStream |
		fileStream timeStamp.
		self fileOutCategory: catName on: fileStream moveSource: false toFile: 0 ].! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 11/28/2024 16:49:43'!
fileOutMethod: selector
	"Write source code of a single method on a file.  Make up a name for the file."

	| nameBody |
	(selector == #Comment) ifTrue: [^ self inform: 'Sorry, cannot file out class comment in isolation.'].
	(self includesSelector: selector) ifFalse: [^ self error: 'Selector ', selector asPlainString, ' not found'].
	nameBody := self name , '-' , (selector copyReplaceAll: ':' with: '').
	DirectoryEntry fileOutsDirQuery: (nameBody asFileName, '.st') writeStreamDo: [ :fileStream |
		fileStream timeStamp.
		self printMethodChunk: selector withPreamble: true
			on: fileStream moveSource: false toFile: 0 ].! !


!Class methodsFor: 'fileIn/Out' stamp: 'jmv 11/28/2024 16:51:03'!
fileOut
	"File a description of the receiver onto a new file whose base name is the name of the receiver.
	Method ordering is by method categories, and in each category, alphabetical."

	DirectoryEntry fileOutsDirQuery: (self name, '.st') writeStreamDo: [ :fileStream |
		fileStream timeStamp.
		self sharedPools size > 0 ifTrue: [
			self shouldFileOutPools
				ifTrue: [ self fileOutSharedPoolsOn: fileStream ]].
		self fileOutOn: fileStream moveSource: false toFile: 0 ].! !

!Class methodsFor: 'fileIn/Out' stamp: 'jmv 11/28/2024 16:51:20'!
fileOutAlphabetically
	"File a description of the receiver onto a new file whose base name is the name of the receiver.
	Method ordering is alphabetical, igoring categories."

	DirectoryEntry fileOutsDirQuery: (self name, '.st') writeStreamDo: [ :fileStream |
		fileStream timeStamp.
		self sharedPools size > 0 ifTrue: [
			self shouldFileOutPools
				ifTrue: [ self fileOutSharedPoolsOn: fileStream ]].
		self fileOutOn: fileStream moveSource: false toFile: 0 initializing: true sortMethodsByCategory: false ].! !

!Class methodsFor: 'fileIn/Out' stamp: 'jmv 11/28/2024 16:53:15'!
fileOutHierarchy
	"File a description of the receiver onto a new file whose base name is the name of the receiver.
	Morph fileOutHierarchy
	"
	DirectoryEntry fileOutsDirQuery: (self name, '-hierarchy.st') writeStreamDo: [ :fileStream |
		fileStream timeStamp.
		self sharedPools size > 0 ifTrue: [
			self shouldFileOutPools
				ifTrue: [ self fileOutSharedPoolsOn: fileStream ]].
		self withAllSubclassesDo: [ :each |
			each fileOutOn: fileStream moveSource: false toFile: 0 ]].! !


!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 11/28/2024 16:51:55'!
fileOutAllCategories
	"
	Cursor write showWhile: [
		SystemOrganization fileOutAllCategories ]
	"
	DirectoryEntry fileOutsDirQuery: 'Cuis-AllCode.st' writeStreamDo: [ :fileStream |
		self categories do: [ :category |
			self fileOutCategoryNoPoolsNoInit: category on: fileStream ]].! !

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 11/28/2024 16:52:07'!
fileOutCategory: aCategoryRoot
	"FileOut all the classes in the named system category."

	DirectoryEntry fileOutsDirQuery: (aCategoryRoot asFileName , '.st') writeStreamDo: [ :fileStream |
		(self withSubCategoriesOf: aCategoryRoot) do: [ :category | 
			self fileOutCategory: category on: fileStream initializing: true ]].! !


!InstructionPrinter class methodsFor: 'printing' stamp: 'jmv 11/28/2024 16:52:32'!
printClass: class 
	"Create a file whose name is the argument followed by '.bytes'. Store on 
	the file the symbolic form of the compiled methods of the class."

	DirectoryEntry fileOutsDirQuery: (class name , '.bytes') writeStreamDo: [ :fileStream |
		class selectorsDo: [ :sel | 
			fileStream newLine; nextPutAll: sel; newLine.
			(self on: (class compiledMethodAt: sel)) printInstructionsOn: fileStream ]].

	"
	InstructionPrinter printClass: Parser.
	"! !


!CodeProvider methodsFor: 'message list menu' stamp: 'jmv 11/28/2024 16:54:23'!
fileOutMessage
	"Put a description of the selected message on a file"

	self selectedMessageName ifNotNil: [
		^self selectedClassOrMetaClass fileOutMethod: self selectedMessageName].

	self messageList ifNotNil: [ :theMethods |
		DirectoryEntry fileOutsDirQuery: 'methods.st' writeStreamDo: [ :fileStream |
			fileStream timeStamp.
			theMethods do: [ :methodRef |
				methodRef actualClass
					printMethodChunk: methodRef methodSymbol
					withPreamble: true
					on: fileStream
					moveSource: false
					toFile: 0 ]]].! !


!ChangeList methodsFor: 'menu actions' stamp: 'jmv 11/28/2024 16:55:46'!
fileOutCurrentVersionsOfSelections

	DirectoryEntry fileOutsDirQuery: 'CurrentVersions.st' writeStreamDo: [ :fileStream |
		fileStream timeStamp.
		self currentVersionsOfSelections do: [ :methodRef |
			methodRef actualClass
				printMethodChunk: methodRef methodSymbol
				withPreamble: true
				on: fileStream
				moveSource: false
				toFile: 0 ]].! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 11/28/2024 16:55:35'!
fileOutSelections

	DirectoryEntry fileOutsDirQuery: 'SelectedChanges.st' writeStreamDo: [ :fileStream |
		fileStream timeStamp.
		listSelections with: changeList do: [ :selected :item |
			selected ifTrue: [ item fileOutOn: fileStream ]]].! !


!CodeFile methodsFor: 'fileIn/fileOut' stamp: 'jmv 11/28/2024 16:58:18'!
fileOut
		DirectoryEntry fileOutsDirQuery: 'filename.st' writeStreamDo: [ :fileStream |
			sourceSystem isEmpty
				ifFalse: [ fileStream nextChunkPut: sourceSystem printString; newLine ].
			self fileOutOn: fileStream.
			fileStream newLine; newLine.
			classes do: [ :cls |
				cls needsInitialize
					ifTrue: [ fileStream newLine; nextChunkPut: cls name,' initialize']].
			fileStream newLine ].! !


!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 11/28/2024 16:59:47'!
fileOut

	DirectoryEntry fileOutsDirQuery: (self name, '.st') writeStreamDo: [ :fileStream |		
		self fileOutOn: fileStream.
		self needsInitialize ifTrue: [
			fileStream newLine; nextChunkPut: self name,' initialize' ]].! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 11/28/2024 16:59:59'!
fileOutCategory: categoryName

	DirectoryEntry fileOutsDirQuery: (self name,'-',categoryName,'.st') writeStreamDo: [ :fileStream |
		self fileOutMethods: (self organization listAtCategoryNamed: categoryName) on: fileStream ].! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 11/28/2024 17:00:14'!
fileOutMethod: selector

	DirectoryEntry fileOutsDirQuery: (name,'-', selector asFileName, '.st') writeStreamDo: [ :fileStream |
		self fileOutMethods: (Array with: selector) on: fileStream ].! !

