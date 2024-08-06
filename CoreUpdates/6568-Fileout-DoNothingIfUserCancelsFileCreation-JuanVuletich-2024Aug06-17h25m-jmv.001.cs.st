'From Cuis7.1 [latest update: #6566] on 6 August 2024 at 5:28:00 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 8/6/2024 17:10:18'!
fileOutCategory: catName
	"FileOut the named category"

	DirectoryEntry fileOutsDirectory // (self name , '-' , catName , '.st') writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			fileStream timeStamp.
			self fileOutCategory: catName on: fileStream moveSource: false toFile: 0 ]].! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 8/6/2024 17:10:34'!
fileOutMethod: selector
	"Write source code of a single method on a file.  Make up a name for the file."

	| nameBody |
	(selector == #Comment) ifTrue: [^ self inform: 'Sorry, cannot file out class comment in isolation.'].
	(self includesSelector: selector) ifFalse: [^ self error: 'Selector ', selector asPlainString, ' not found'].
	nameBody := self name , '-' , (selector copyReplaceAll: ':' with: '').
	DirectoryEntry fileOutsDirectory // (nameBody asFileName, '.st') writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			fileStream timeStamp.
			self printMethodChunk: selector withPreamble: true
				on: fileStream moveSource: false toFile: 0 ]].! !


!Class methodsFor: 'fileIn/Out' stamp: 'jmv 8/6/2024 17:11:01'!
fileOut
	"File a description of the receiver onto a new file whose base name is the name of the receiver."

	DirectoryEntry fileOutsDirectory // (self name, '.st') writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			fileStream timeStamp.
			self sharedPools size > 0 ifTrue: [
				self shouldFileOutPools
					ifTrue: [ self fileOutSharedPoolsOn: fileStream ]].
			self fileOutOn: fileStream moveSource: false toFile: 0 ]].! !

!Class methodsFor: 'fileIn/Out' stamp: 'jmv 8/6/2024 17:11:42'!
fileOutHierarchy
	"File a description of the receiver onto a new file whose base name is the name of the receiver.
	Morph fileOutHierarchy
	"

	DirectoryEntry fileOutsDirectory // (self name, '-hierarcy.st') writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			fileStream timeStamp.
			self sharedPools size > 0 ifTrue: [
				self shouldFileOutPools
					ifTrue: [ self fileOutSharedPoolsOn: fileStream ]].
			self withAllSubclassesDo: [ :each |
				each fileOutOn: fileStream moveSource: false toFile: 0 ]]].! !


!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 8/6/2024 17:13:30'!
fileOutAllCategories
	"
	Cursor write showWhile: [
		SystemOrganization fileOutAllCategories ]
	"
	DirectoryEntry fileOutsDirectory // 'Cuis-AllCode.st' writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			self categories do: [ :category |
				self fileOutCategoryNoPoolsNoInit: category on: fileStream ]]].! !

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 8/6/2024 17:13:54'!
fileOutCategory: aCategoryRoot
	"FileOut all the classes in the named system category."

	DirectoryEntry fileOutsDirectory // (aCategoryRoot asFileName , '.st') writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			(self withSubCategoriesOf: aCategoryRoot) do: [ :category | 
				self fileOutCategory: category on: fileStream initializing: true ]]].! !


!InstructionPrinter class methodsFor: 'printing' stamp: 'jmv 8/6/2024 17:21:17'!
printClass: class 
	"Create a file whose name is the argument followed by '.bytes'. Store on 
	the file the symbolic form of the compiled methods of the class."

	DirectoryEntry fileOutsDirectory // (class name , '.bytes') writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			class selectorsDo: [ :sel | 
				fileStream newLine; nextPutAll: sel; newLine.
				(self on: (class compiledMethodAt: sel)) printInstructionsOn: fileStream ]]].

	"
	InstructionPrinter printClass: Parser.
	"! !


!CodeProvider methodsFor: 'message list menu' stamp: 'jmv 8/6/2024 17:20:43'!
fileOutMessage
	"Put a description of the selected message on a file"

	self selectedMessageName ifNotNil:
		[^self selectedClassOrMetaClass fileOutMethod: self selectedMessageName].
	self messageList ifNotNil:
		[:theMethods|
		self request: 'Enter file name'
			initialAnswer: 'methods.st'
			do: [ :filename |
				DirectoryEntry fileOutsDirectory // filename writeStreamDo:
					[ :fileStream |
					fileStream ifNotNil: [
						fileStream timeStamp.
						theMethods do:
							[ :methodRef |
							methodRef actualClass
								printMethodChunk: methodRef methodSymbol
								withPreamble: true
								on: fileStream
								moveSource: false
								toFile: 0 ]]]]].! !


!ChangeList methodsFor: 'menu actions' stamp: 'jmv 8/6/2024 17:20:23'!
fileOutCurrentVersionsOfSelections

	self request: 'Enter file name' initialAnswer: 'CurrentVersions.st' do: [ :filename |
		DirectoryEntry fileOutsDirectory // filename writeStreamDo: [ :fileStream |
			fileStream ifNotNil: [
				fileStream timeStamp.
				self currentVersionsOfSelections do: [ :methodRef |
					methodRef actualClass
						printMethodChunk: methodRef methodSymbol
						withPreamble: true
						on: fileStream
						moveSource: false
						toFile: 0 ]]]].! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 8/6/2024 17:20:02'!
fileOutSelections

	self request: 'Enter file name' initialAnswer: 'SelectedChanges.st' do: [ :filename |
		DirectoryEntry fileOutsDirectory // filename writeStreamDo: [ :fileStream |
			fileStream ifNotNil: [
				fileStream timeStamp.
				listSelections with: changeList do: [ :selected :item |
					selected ifTrue: [ item fileOutOn: fileStream ]]]]].! !


!FileList methodsFor: 'private' stamp: 'jmv 8/6/2024 17:15:20'!
put: aText
	"Private - put the supplied text onto the file"

	| type |
	brevityState == #fullFile ifTrue: [
		acceptedContentsCache := aText.
		directory // fileName writeStreamDo: [ :fileStream |
			fileStream ifNotNil: [
				fileStream nextPutAll: acceptedContentsCache asPlainString ]].
		directory invalidateChildren.
		self updateFileList.
		^ true  "accepted"].
	listIndex = 0 ifTrue: [
		self inform: 'No fileName is selected'.
		^ false  "failed"].
	type := 'These'.
	brevityState = #briefFile ifTrue: [type := 'Abbreviated'].
	brevityState = #briefHex ifTrue: [type := 'Abbreviated'].
	brevityState = #fullHex ifTrue: [type := 'Hexadecimal'].
	brevityState = #FileList ifTrue: [type := 'Directory'].
	self inform: ('{1} contents cannot
meaningfully be saved at present.' format:{type}).
	^ false  "failed"! !


!SystemWindow methodsFor: 'user interface' stamp: 'jmv 8/6/2024 17:16:06'!
saveContentsTo: aFileEntry
	"Saves the contents to the given filename"

	self hasSaveAs ifFalse: [^self].
	aFileEntry writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			fileStream nextPutAll: model contentsForFileSaving.
			self 
				setProperty: #lastFilenameUsedToSave toValue: aFileEntry pathName;
				notifyUserWith: 'Contents saved to', String newLineString, aFileEntry pathName ]].! !


!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 8/6/2024 17:16:22'!
saveWorldInFile
	"Save the world's submorphs, model, and stepList in a file.  "

	self request: 'File name for this morph?' do: [ :fileName |
		fileName isEmpty ifTrue: [^ self].  "abort"
		"Save only model, stepList, submorphs in this world"
		myWorld submorphsDo: [ :m | m allMorphsDo: [ :subM | subM prepareToBeSaved ]].	"Amen"
		(fileName, '.morph') asFileEntry writeStreamDo: [ :fileStream |
			fileStream ifNotNil: [
				fileStream fileOutObject: myWorld ]]].! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 8/6/2024 17:16:46'!
fileOut
	"File out the receiver, to a file whose name is a function of the  
	change-set name and either of the date & time or chosen to have a  
	unique numeric tag, depending on the preference  
	'changeSetVersionNumbers'"
	| folder slips nameToUse |
	folder := DirectoryEntry savedChangeSetsDirectory.
	nameToUse := self name.
	nameToUse := nameToUse copyReplaceAll: 'AuthorName' with: Utilities authorName asUnaccented asCamelCase.
	nameToUse := (Preferences at: #changeSetVersionNumbers)
				ifTrue: [
					folder
						nextNameFor: nameToUse coda: '-', Utilities authorInitials
						extension: 'cs.st' ]
				ifFalse: [ (nameToUse , '.' , Utilities dateTimeSuffix , '.cs.st') asFileName ].

	folder // nameToUse writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			fileStream timeStamp.
			self fileOutPreambleOn: fileStream.
			self fileOutOn: fileStream.
			self fileOutPostscriptOn: fileStream ]].

	self hasUnsavedChanges: false.
	(Preferences at: #checkForSlips) 		ifFalse: [^ self].
	slips := self checkForSlips.
	(slips notEmpty
			and: [(PopUpMenu withCaption: 'Methods in this fileOut have halts
or references to the Transcript
or other ''slips'' in them.
Would you like to browse them?' chooseFrom: 'Ignore\Browse slips')
					= 2])
		ifTrue: [ Smalltalk browseMessageList: slips name: 'Possible slips in ' , name ]! !


!CodeFile methodsFor: 'fileIn/fileOut' stamp: 'jmv 8/6/2024 17:17:43'!
fileOut
	self request: 'Enter the file name' initialAnswer: '' do: [ :aString |
		DirectoryEntry fileOutsDirectory // aString writeStreamDo: [ :fileStream |
			fileStream ifNotNil: [
				sourceSystem isEmpty
					ifFalse: [ fileStream nextChunkPut: sourceSystem printString; newLine ].
				self fileOutOn: fileStream.
				fileStream newLine; newLine.
				classes do: [ :cls |
					cls needsInitialize
						ifTrue: [ fileStream newLine; nextChunkPut: cls name,' initialize']].
				fileStream newLine ]]].! !


!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 8/6/2024 17:18:24'!
fileOut

	DirectoryEntry fileOutsDirectory // (self name, '.st') writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			self fileOutOn: fileStream.
			self needsInitialize ifTrue: [
				fileStream newLine; nextChunkPut: self name,' initialize' ]]].! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 8/6/2024 17:18:43'!
fileOutCategory: categoryName

	DirectoryEntry fileOutsDirectory // (self name,'-',categoryName,'.st') writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			self fileOutMethods: (self organization listAtCategoryNamed: categoryName) on: fileStream ]].! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 8/6/2024 17:19:02'!
fileOutMethod: selector

	DirectoryEntry fileOutsDirectory // (name,'-', selector asFileName, '.st') writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			self fileOutMethods: (Array with: selector) on: fileStream ]].! !


!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 8/6/2024 17:19:21'!
printSpaceAnalysis	
	"
	SpaceTally new printSpaceAnalysis
	"

	DirectoryEntry logsDirectory // 'STspace.text' writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			self printSpaceAnalysis: 1 on: fileStream ]].! !

