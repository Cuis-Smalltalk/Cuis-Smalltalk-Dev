'From Cuis6.0 [latest update: #6122] on 15 December 2023 at 4:35:47 pm'!

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 12/15/2023 14:54:42'!
fileOutCurrentVersionsOfSelections

	self request: 'Enter file name' initialAnswer: 'CurrentVersions.st' do: [ :filename |
		DirectoryEntry fileOutsDirectory // filename writeStreamDo: [ :stream |
			stream timeStamp.
			self currentVersionsOfSelections print do: [ :methodRef |
				methodRef actualClass
					printMethodChunk: methodRef methodSymbol
					withPreamble: true
					on: stream
					moveSource: false
					toFile: 0 ]]].! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 12/15/2023 14:55:03'!
fileOutSelections

	self request: 'Enter file name' initialAnswer: 'SelectedChanges.st' do: [ :filename |
		DirectoryEntry fileOutsDirectory // filename writeStreamDo: [ :stream |
			stream timeStamp.
			listSelections with: changeList do: [ :selected :item |
				selected ifTrue: [ item fileOutOn: stream ]]]].! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/15/2023 15:16:41'!
fileOutCategory: catName
	"FileOut the named category"

	DirectoryEntry fileOutsDirectory // (self name , '-' , catName , '.st') writeStreamDo: [ :fileStream |
		fileStream timeStamp.
		self fileOutCategory: catName on: fileStream moveSource: false toFile: 0 ]! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/15/2023 15:16:46'!
fileOutMethod: selector
	"Write source code of a single method on a file.  Make up a name for the file."

	| nameBody |
	(selector == #Comment) ifTrue: [^ self inform: 'Sorry, cannot file out class comment in isolation.'].
	(self includesSelector: selector) ifFalse: [^ self error: 'Selector ', selector asPlainString, ' not found'].
	nameBody := self name , '-' , (selector copyReplaceAll: ':' with: '').
	DirectoryEntry fileOutsDirectory // (nameBody asFileName, '.st') writeStreamDo: [ :fileStream |
		fileStream timeStamp.
		self printMethodChunk: selector withPreamble: true
			on: fileStream moveSource: false toFile: 0 ]! !


!Class methodsFor: 'fileIn/Out' stamp: 'jmv 12/15/2023 15:16:50'!
fileOut
	"File a description of the receiver onto a new file whose base name is the name of the receiver."

	DirectoryEntry fileOutsDirectory // (self name, '.st') writeStreamDo: [ :stream |
		stream timeStamp.
		self sharedPools size > 0 ifTrue: [
			self shouldFileOutPools
				ifTrue: [ self fileOutSharedPoolsOn: stream ]].
		self fileOutOn: stream moveSource: false toFile: 0 ]! !

!Class methodsFor: 'fileIn/Out' stamp: 'jmv 12/15/2023 15:16:54'!
fileOutHierarchy
	"File a description of the receiver onto a new file whose base name is the name of the receiver.
	Morph fileOutHierarchy
	"

	DirectoryEntry fileOutsDirectory // (self name, '-hierarcy.st') writeStreamDo: [ :stream |
		stream timeStamp.
		self sharedPools size > 0 ifTrue: [
			self shouldFileOutPools
				ifTrue: [ self fileOutSharedPoolsOn: stream ]].
		self withAllSubclassesDo: [ :each |
			each fileOutOn: stream moveSource: false toFile: 0 ]].! !


!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 12/15/2023 14:55:12'!
fileOutAllCategories
	"
	Cursor write showWhile: [
		SystemOrganization fileOutAllCategories ]
	"
	DirectoryEntry fileOutsDirectory // 'Cuis-AllCode.st' writeStreamDo: [ :stream |
		self categories do: [ :category |
			self fileOutCategoryNoPoolsNoInit: category on: stream ]]! !

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 12/15/2023 15:16:58'!
fileOutCategory: aCategoryRoot
	"FileOut all the classes in the named system category."

	DirectoryEntry fileOutsDirectory // (aCategoryRoot asFileName , '.st') writeStreamDo: [ :fileStream |
		(self withSubCategoriesOf: aCategoryRoot) do: [ :category | 
			self fileOutCategory: category on: fileStream initializing: true ]]! !


!InstructionPrinter class methodsFor: 'printing' stamp: 'jmv 12/15/2023 15:17:19'!
printClass: class 
	"Create a file whose name is the argument followed by '.bytes'. Store on 
	the file the symbolic form of the compiled methods of the class."

	DirectoryEntry fileOutsDirectory // (class name , '.bytes') writeStreamDo: [ :file |
		class selectorsDo: [ :sel | 
			file newLine; nextPutAll: sel; newLine.
			(self on: (class compiledMethodAt: sel)) printInstructionsOn: file ]].

	"
	InstructionPrinter printClass: Parser.
	"! !


!CodeFile methodsFor: 'fileIn/fileOut' stamp: 'jmv 12/15/2023 15:11:21'!
fileOut
	self request: 'Enter the file name' initialAnswer: '' do: [ :aString |
		DirectoryEntry fileOutsDirectory // aString writeStreamDo: [ :stream | 
			sourceSystem isEmpty
				ifFalse: [ stream nextChunkPut: sourceSystem printString; newLine ].
			self fileOutOn: stream.
			stream newLine; newLine.
			classes do: [ :cls |
				cls needsInitialize
					ifTrue: [ stream newLine; nextChunkPut: cls name,' initialize']].
			stream newLine ]]! !


!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 12/15/2023 15:27:50'!
fileOut

	DirectoryEntry fileOutsDirectory // (self name, '.st') writeStreamDo: [ :stream |
		self fileOutOn: stream.
		self needsInitialize ifTrue: [
			stream newLine; nextChunkPut: self name,' initialize' ]]! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 12/15/2023 15:27:54'!
fileOutCategory: categoryName

	DirectoryEntry fileOutsDirectory // (self name,'-',categoryName,'.st') writeStreamDo: [ :f |
		self fileOutMethods: (self organization listAtCategoryNamed: categoryName) on: f ]! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 12/15/2023 15:28:00'!
fileOutMethod: selector

	DirectoryEntry fileOutsDirectory // (name,'-', selector asFileName, '.st') writeStreamDo: [ :f |
		self fileOutMethods: (Array with: selector) on: f ]! !

