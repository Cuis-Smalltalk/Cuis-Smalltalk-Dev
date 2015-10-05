'From Cuis 4.2 of 25 July 2013 [latest update: #2517] on 3 October 2015 at 8:09:13.858745 pm'!

!FileList methodsFor: 'file list' stamp: 'jmv 10/3/2015 19:01'!
selectedFileEntry
	"Answer the selected file, in the form of a FileEntry. For the various stream-reading services."

	^self directory ifNotNil: [ :dir | (dir fullNameFor: fileName) asFileEntry ]! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 10/3/2015 19:21'!
fileOutCategory: catName
	"FileOut the named category"

	(FmDirectoryEntry smalltalkImageDirectory pathName, '/', self name , '-' , catName , '.st') asFileEntry writeStream: [ :fileStream |
		fileStream timeStamp.
		self fileOutCategory: catName on: fileStream moveSource: false toFile: 0 ]! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 10/3/2015 19:22'!
fileOutMethod: selector
	"Write source code of a single method on a file.  Make up a name for the file."

	| nameBody |
	(selector == #Comment) ifTrue: [^ self inform: 'Sorry, cannot file out class comment in isolation.'].
	(self includesSelector: selector) ifFalse: [^ self error: 'Selector ', selector asString, ' not found'].
	nameBody _ self name , '-' , (selector copyReplaceAll: ':' with: '').
	(FmDirectoryEntry smalltalkImageDirectory pathName, '/', nameBody , '.st') asFileEntry writeStream: [ :fileStream |
		fileStream timeStamp.
		self printMethodChunk: selector withPreamble: true
			on: fileStream moveSource: false toFile: 0 ]! !


!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 10/3/2015 19:45'!
fileOutCategory: category
	"FileOut all the classes in the named system category."

	(FmDirectoryEntry smalltalkImageDirectory pathName, '/', category asFileName , '.st') asFileEntry writeStream: [ :fileStream |
		self fileOutCategory: category on: fileStream initializing: true ]! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 10/3/2015 19:37'!
condenseSources	
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseSources"

	| classCount dir newVersionString oldChangesName newChangesName newSourcesName |
	newVersionString _ FillInTheBlankMorph request: 'Please name the new sources file' initialAnswer: SourceFileVersionString.
	newVersionString ifNil: [^ self].
	newVersionString = SourceFileVersionString ifTrue: [
		^ self error: 'The new source file must not be the same as the old.'].
	SourceFileVersionString _ newVersionString.

	"Write all sources with fileIndex 1"
	newSourcesName _ self defaultSourcesName.
	newSourcesName asFileEntry writeStream: [ :f |
		f timeStamp.
		'Condensing Sources File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: Smalltalk classNames size
			during: [ :bar |
				classCount _ 0.
				Smalltalk allClassesDo: [ :class |
					bar value: (classCount _ classCount + 1).
					class fileOutOn: f moveSource: true toFile: 1]]].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	"Make a new empty changes file"
	oldChangesName _ self currentChangesName.
	self closeSourceFiles.
	oldChangesName ifNotNil: [
		dir _ FileDirectory smalltalkImageDirectory.
		dir rename: oldChangesName toBe: oldChangesName, '.old' ].
	newChangesName _ self defaultChangesName.
	newChangesName writeStream: [ :stream |
		stream timeStamp ].
	LastQuitLogPosition _ 0.

	self openSourceFiles.
	self inform: 'Source files have been rewritten!!
 
Check that all is well, and then save/quit.
 
Otherwise, remove new sources/changes,
replace them with the former ones, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 10/3/2015 19:13'!
saveAs: newName andQuit: aBoolean clearAllClassState: clearAllStateFlag
	"Save the image  under a new name."

	| newChangesName |
	self currentChangesName ifNotNil: [ :oldChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName _ self fullNameForChangesNamed: newName.
		FmFileIOAccessor copy: oldChangesName to: newChangesName ].

	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: aBoolean
		clearAllClassState: clearAllStateFlag! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 10/3/2015 19:42'!
externalizeSources   
	"Write the sources and changes streams onto external files."
	"
	Smalltalk externalizeSources
	"

	| sourcesName changesName |

	sourcesName _ self defaultSourcesName.
	sourcesName asFileEntry writeStream: [ :stream |
		stream nextPutAll: SourceFiles first originalContents ].
	SourceFiles at: 1 put: (FileStream readOnlyFileNamed: sourcesName).

	changesName _ self defaultChangesName.
	changesName  asFileEntry writeStream: [ :stream |
		stream nextPutAll: SourceFiles last contents ].
	SourceFiles at: 2 put: (FileStream oldFileNamed: changesName).

	self inform: 'Sources successfully externalized'! !


!FileDoesNotExistException class methodsFor: 'examples' stamp: 'jmv 10/3/2015 20:01'!
example
	"
	FileDoesNotExistException example
	"

	| result |
	result _ [ 'error42.log' asFileEntry textContents ]
		on: FileDoesNotExistException
		do: [:ex | 'No error log'].
	Transcript show: result; newLine! !


!UnhandledError methodsFor: 'priv handling' stamp: 'jmv 10/3/2015 19:48'!
runtimeDefaultAction
	"Dump the stack trace to a log file, then exit the program (image)."

	('error', Utilities dateTimeSuffix, FileDirectory dot, 'log') asFileName asFileEntry writeStream: [ :file |
		Smalltalk timeStamp: file.
		(thisContext sender stackOfSize: 20) do: [ :ctx | file newLine. ctx printOn: file ]].

	Smalltalk snapshot: false andQuit: true clearAllClassState: false! !


!Form methodsFor: 'fileIn/Out' stamp: 'jmv 10/3/2015 19:14'!
writeOnFileNamed: fileName 
	"Saves the receiver on the file fileName in the format:
		fileCode, depth, extent, offset, bits."

	fileName asFileEntry forceWriteStream: [ :file |
		file binary.
		file nextPut: 2.  "file code = 2"
		self writeOn: file ].

"
 | f |
[(f _ Form fromUser) boundingBox area>25] whileTrue:
	[f writeOnFileNamed: 'test.form'.
	(Form fromFileNamed: 'test.form') display].
"! !


!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 10/3/2015 19:05'!
putForm: aForm onFileNamed: filename
	"Store the given form on a file of the given name."

	filename asFileEntry forceWriteStream: [ :stream |
		(self onBinaryStream: stream binary) nextPutImage: aForm ]! !


!InstructionPrinter class methodsFor: 'printing' stamp: 'jmv 10/3/2015 19:27'!
printClass: class 
	"Create a file whose name is the argument followed by '.bytes'. Store on 
	the file the symbolic form of the compiled methods of the class."

	(FmDirectoryEntry smalltalkImageDirectory pathName, '/', class name , '.bytes') asFileEntry writeStream: [ :file |
		class selectorsDo: [ :sel | 
			file newLine; nextPutAll: sel; newLine.
			(self on: (class compiledMethodAt: sel)) printInstructionsOn: file ]].

	"
	InstructionPrinter printClass: Parser.
	"! !


!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 10/3/2015 19:27'!
fileOutCategory: categoryName

	(self name,'-',categoryName,'.st') asFileEntry writeStream: [ :f |
		self fileOutMethods: (self organization listAtCategoryNamed: categoryName) on: f ]! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 10/3/2015 19:28'!
fileOutMethod: selector

	(self name,'-', selector, '.st') asFileEntry writeStream: [ :f |
		self fileOutMethods: (Array with: selector) on: f ]! !


!SmartRefStream methodsFor: 'class changed shape' stamp: 'jmv 10/3/2015 19:58'!
writeClassRenameMethod: sel was: oldName fromInstVars: oldList 
	"The class coming is unknown.  Ask the user for the existing class it maps to.  If got one, write a method, and restart the obj fileIn.  If none, write a dummy method and get the user to complete it later.  "

	| tell choice newName answ code oldVer newList newVer instSel |
	self flag: #bobconv.
	tell := 'Reading an instance of ' , oldName 
				, '.
Which modern class should it translate to?'.
	answ := (PopUpMenu 
				labels: 'Let me type the name now
Let me think about it
Let me find a conversion file on the disk') 
					startUpWithCaption: tell.
	answ = 1 
		ifTrue: [
			tell := 'Name of the modern class {1} should translate to:' format: {oldName}.
			choice := FillInTheBlankMorph request: tell.	"class name"
			choice size = 0 
				ifTrue: [answ := 'conversion method needed']
				ifFalse: 
					[newName := choice.
					answ := Smalltalk at: newName asSymbol
								ifAbsent: ['conversion method needed'].
					answ class == String 
						ifFalse: [renamed at: oldName asSymbol put: answ name]]].
	answ = 3 | (answ = 0) 
		ifTrue: [
			byteStream close.
			^'conversion method needed'].
	answ = 2 ifTrue: [answ := 'conversion method needed'].
	answ = 'conversion method needed' 
		ifTrue: [
			byteStream close.
			newName := 'PutNewClassHere'].
	answ class == String 
		ifFalse: 
			[oldVer := self versionSymbol: (structures at: oldName).
			newList := (Array with: answ classVersion) , answ allInstVarNames.
			newVer := self versionSymbol: newList.
			instSel := 'convert' , oldVer , ':' , newVer , ':'].
	code := WriteStream on: (String new: 500).
	code
		nextPutAll: sel;
		newLine.
	answ class == String 
		ifFalse: [
			code
				newLine;
				tab;
				nextPutAll: 'reshaped at: #' , oldName , ' put: #' , instSel , '.'.
			code
				newLine;
				tab;
				tab;
				nextPutAll: '"Be sure to define that conversion method in class ' 
							, answ name , '"'].
	code
		newLine;
		tab;
		nextPutAll: '^ ' , newName.	"Return new class"
	self class compile: code contents classified: 'conversion'.
	newName = 'PutNewClassHere' 
		ifTrue: [
			self 
				inform: 'Please complete the following method and 
then read-in the object file again.'.
			Smalltalk browseAllImplementorsOf: sel asSymbol].
	self flag: #violateBasicLayerPrinciples.
	"SmartRefStream should not refer to UI!!!!!!!!!! (sd)"

	"The class version number only needs to change under one specific circumstance.  That is when the first letters of the instance variables have stayed the same, but their meaning has changed.  A conversion method is needed, but this system does not know it.  
	If this is true for class Foo, define classVersion in Foo class.  
	Beware of previous object fileouts already written after the change in meaning, but before bumping the version number.  They have the old (wrong) version number, say 2.  If this is true, your method must be able to test the data and successfully read files that say version 2 but are really 3."
	^answ! !


!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 10/3/2015 19:47'!
saveWorldInFile
	"Save the world's submorphs, model, and stepList in a file.  "

	| fileName |
	fileName _ FillInTheBlankMorph request: 'File name for this morph?'.
	fileName isEmpty ifTrue: [^ self].  "abort"

	"Save only model, stepList, submorphs in this world"
	myWorld submorphsDo: [ :m |
		m allMorphsDo: [ :subM | subM prepareToBeSaved ]].	"Amen"

	(fileName, '.morph') asFileEntry writeStream: [ :fileStream |
		fileStream fileOutObject: myWorld ]! !

!methodRemoval: DataStream class #forceNewFileNamed:!
DataStream class removeSelector: #forceNewFileNamed:!
!methodRemoval: DataStream class #newFileNamed:!
DataStream class removeSelector: #newFileNamed:!
!methodRemoval: DataStream class #oldFileNamed:!
DataStream class removeSelector: #oldFileNamed:!
!methodRemoval: DataStream #close!
DataStream removeSelector: #close!
!methodRemoval: StandardFileMenu class #oldFileStreamFrom:!
StandardFileMenu class removeSelector: #oldFileStreamFrom:!
!methodRemoval: FileDirectory #copyFileWithoutOverwriteConfirmationNamed:toFileNamed:!
FileDirectory removeSelector: #copyFileWithoutOverwriteConfirmationNamed:toFileNamed:!
!methodRemoval: FileDirectory #forceNewFileNamed:!
FileDirectory removeSelector: #forceNewFileNamed:!
!methodRemoval: Class class #fileOutPool:!
Class class removeSelector: #fileOutPool:!
