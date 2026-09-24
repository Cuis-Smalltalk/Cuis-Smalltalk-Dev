'From Cuis7.9 [latest update: #8235] on 24 September 2026 at 5:12:03 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 23/Sep/2026, 5:25:37 pm (UTC)'!
classComment: aString stamp: aStamp
	"Store the comment, aString or Text or RemoteString, associated with the class we are organizing.  Empty string gets stored only if had a non-empty one before."

	| ptr header oldCommentRemoteStr |
	aString isRemote ifTrue: [
		SystemChangeNotifier uniqueInstance classCommented: self.
		self organization classComment: aString stamp: aStamp.
		^self ].

	oldCommentRemoteStr := self organization commentRemoteStr.
	(aString isEmpty) & (oldCommentRemoteStr == nil) ifTrue: [
		self organization classComment: nil.
		^self ].
		"never had a class comment, no need to write empty string out"

	ptr := nil.
	oldCommentRemoteStr ifNotNil: [
		ptr := oldCommentRemoteStr historyPointer.
		ptr := ptr >= 16rFC000000
			ifTrue: [ ptr - 16rFC000000 + 16r1000000 ]].
	
	SourceFiles ifNotNil: [ | file |
		(file := SourceFiles at: 2) ifNotNil: [
			file setToEnd; newLine; nextPut: $!!.	"directly"
			header := String streamContents: [:strm | strm nextPutAll: self name;
				nextPutAll: ' commentStamp: '.
				aStamp storeOn: strm.
				ptr ifNotNil: [
					strm nextPutAll: ' prior: '; nextPutAll: ptr printString ]].
			file nextChunkPut: header]].
	self organization classComment: (RemoteString newString: aString onFileNumber: 2) stamp: aStamp.

	Smalltalk userChangesAppendDo: [ :stream |
		stream newLine; nextPut: $!!.	"directly"
		header := String streamContents: [:strm | strm nextPutAll: self name;
			nextPutAll: ' commentStamp: '.
			aStamp storeOn: strm.
			ptr ifNotNil: [
				strm nextPutAll: ' prior: '; nextPutAll: ptr printString ]].
		stream nextChunkPut: header.
		stream newLine; nextChunkPut: aString ].

	SystemChangeNotifier uniqueInstance classCommented: self.! !


!ClassDescription class methodsFor: 'utilities' stamp: 'jmv 23/Sep/2026, 5:49:01 pm (UTC)'!
printCategoryChunk: className category: category on: aFileStream withStamp: changeStamp priorMethod: priorMethod
	"Print a method category preamble.  This must have a category name.
	It may have an author/date stamp, and it may have a prior source link.
	Always include a stamp, even if it is empty."

	| ptr |
	aFileStream newLine; nextPut: $!!.
	aFileStream nextChunkPut: (String streamContents: [ :strm |
		strm nextPutAll: className; nextPutAll: ' methodsFor: '; print: category asString.
		strm nextPutAll: ' stamp: '; print: (changeStamp ifNil: ['']).
		priorMethod notNil ifTrue: [
			ptr := priorMethod historyPointer.
			ptr >= 16rFC000000 ifTrue: [
				strm nextPutAll: ' prior: '; print: ptr - 16rFC000000 + 16r1000000 ]].
		]).! !


!CompiledMethod class methodsFor: 'source code management aux' stamp: 'jmv 22/Sep/2026, 7:16:13 pm (UTC)'!
priorReferenceFrom: tokens
	"also review selectors matching 'prior:' "
	^ self field: #prior: from: tokens ifAbsentOrNil: nil! !


!VersionsBrowser methodsFor: 'init & update' stamp: 'jmv 22/Sep/2026, 5:20:23 pm (UTC)'!
scanVersionsOf: method class: class meta: meta category: category selector: selector
	| position stamp priorMethodRef prevPos prevFileIndex preamble methodProperties libNameOrNil tokens sourceFilesCopy |
	selectorOfMethod := selector.
	currentCompiledMethod := method.
	classOfMethod := meta ifTrue: [class class] ifFalse: [class].
	changeList := OrderedCollection new.
	list := OrderedCollection new.
	addedChange ifNotNil: [ :change |
		self addItem: change text: ('{1} (in {2})' format: { change stamp. change fileName }) ].
	listIndex := 0.
	position := method filePosition.
	sourceFilesCopy := SourceFiles collect:
		[:x | x ifNotNil: [ x name asFullFileEntry readStream ]].
	method fileIndex = 0 ifTrue: [^ nil].
	file := sourceFilesCopy at: method fileIndex.
	[position notNil & file notNil] whileTrue: [
		"Skip back filler"
		file position: position.
		file backChunk. "Skip back blank space."
		"Get preamble"
		preamble := file backChunk.
		methodProperties := nil.
		libNameOrNil := nil.
		(preamble beginsWith: 'methodProperties:') ifTrue: [
			methodProperties := Compiler evaluate: (preamble copyFrom: 'methodProperties:' size+1 to: preamble size).
			methodProperties do: [ :assoc |
				assoc key == #libraryName ifTrue: [ libNameOrNil := assoc value ]].
			file backChunk. "Skip back blank space."
			preamble := file backChunk ].
		"Preamble is likely a linked method preamble, if we're in
			a changes file (not the sources file).  Try to parse it
			for prior source position and file index"
		prevFileIndex := nil.
		prevPos := nil.
		stamp := ''.
		(preamble includesSubString: 'methodsFor:')
			ifTrue: [
				tokens := Scanner new scanTokens: preamble.
				stamp := CompiledMethod authorAndStampLocalTZFrom: tokens.
				priorMethodRef := CompiledMethod priorReferenceFrom: tokens.
				priorMethodRef ifNotNil: [
					priorMethodRef = 0 ifFalse: [
						priorMethodRef := priorMethodRef + 16rFC000000 - 16r1000000 ].
					prevFileIndex := sourceFilesCopy fileIndexFromHistoryPointer: priorMethodRef.
					prevPos := sourceFilesCopy filePositionFromHistoryPointer: priorMethodRef ]].
 		self addItem:
				(MethodDefinitionChange new
					file: file position: position
					class: class name category: category meta: meta stamp: stamp
					properties: methodProperties
					prior: priorMethodRef
					selector: selectorOfMethod)
			text: stamp , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector
					", (libNameOrNil ifNil: [''] ifNotNil: [' - [libraryName: ', libNameOrNil, ']'])."
					, (methodProperties ifNil: [''] ifNotNil: [' [methodProperties: ', methodProperties printString, '] ']).
		position := prevPos.
		prevPos notNil ifTrue: [
			file := sourceFilesCopy at: prevFileIndex]].
	sourceFilesCopy do: [:x | x notNil ifTrue: [x close]].
	self clearSelections! !


!ClassCommentVersionsBrowser methodsFor: 'basic function' stamp: 'jmv 23/Sep/2026, 5:46:26 pm (UTC)'!
scanVersionsOf: class
	"Scan for all past versions of the class comment of the given class"

	| oldCommentRemoteStr sourceFilesCopy position prevPos stamp preamble tokens prevFileIndex |

	classOfMethod := class.
	oldCommentRemoteStr := class  organization commentRemoteStr.
	currentCompiledMethod := oldCommentRemoteStr.
	selectorOfMethod := #Comment.
	changeList := OrderedCollection new.
	list := OrderedCollection new.
	listIndex := 0.
	oldCommentRemoteStr ifNil: [
		^ nil].

	sourceFilesCopy := SourceFiles collect: [ :x | x ifNotNil: [x name asFullFileEntry readStream]].
	position := oldCommentRemoteStr position.
	file := sourceFilesCopy at: oldCommentRemoteStr sourceFileNumber.
	[ position notNil & file notNil]  whileTrue: [
		"Skip back filler"
		file position: position.
		file backChunk. "Skip back blank space."
		"Get preamble"
		preamble := file backChunk.
		(preamble beginsWith: 'methodProperties:') ifTrue: [
			file backChunk. "Skip back blank space."
			preamble := file backChunk ].
		prevPos := nil.
		stamp := ''.
		(preamble includesSubString: 'commentStamp:')
			ifTrue: [
				tokens := Scanner new scanTokens: preamble.
				stamp := CompiledMethod field: #commentStamp: from: tokens ifAbsentOrNil: [ '' ].
				(CompiledMethod priorReferenceFrom: tokens) ifNotNil: [ :priorRef | | priorMethodRef |
					priorMethodRef := priorRef.
					priorMethodRef = 0 ifFalse: [
						priorMethodRef := priorMethodRef + 16rFC000000 - 16r1000000 ].
					prevFileIndex := sourceFilesCopy fileIndexFromHistoryPointer: priorMethodRef.
					prevPos := sourceFilesCopy filePositionFromHistoryPointer: priorMethodRef ]]
			ifFalse: [
				"The stamp get lost, maybe after a condenseChanges"
				stamp := '<historical>'].
 		self addItem:
				(ClassCommentChange new file: file position: position
						class: class name stamp: stamp)
			text: stamp , ' ' , class name , ' class comment'. 
		prevPos = 0 ifTrue: [ prevPos := nil ].
		position := prevPos.
		prevPos notNil ifTrue: [ file := sourceFilesCopy at: prevFileIndex ]].
	sourceFilesCopy do: [ :x | x notNil ifTrue: [ x close ]].
	self clearSelections.! !

!methodRemoval: RemoteString #sourcePointer stamp: 'jmv 24/Sep/2026, 8:11:12 pm (UTC)'!
RemoteString removeSelector: #sourcePointer!
!methodRemoval: CuisSourceFileArray #sourcePointerFromFileIndex:andPosition: stamp: 'jmv 24/Sep/2026, 8:11:20 pm (UTC)'!
CuisSourceFileArray removeSelector: #sourcePointerFromFileIndex:andPosition:!
!methodRemoval: CuisSourceFileArray #fileIndexFromSourcePointer: stamp: 'jmv 24/Sep/2026, 8:11:20 pm (UTC)'!
CuisSourceFileArray removeSelector: #fileIndexFromSourcePointer:!
!methodRemoval: CuisSourceFileArray #filePositionFromSourcePointer: stamp: 'jmv 24/Sep/2026, 8:11:20 pm (UTC)'!
CuisSourceFileArray removeSelector: #filePositionFromSourcePointer:!
!methodRemoval: CompiledMethod #sourcePointer stamp: 'jmv 24/Sep/2026, 8:11:12 pm (UTC)'!
CompiledMethod removeSelector: #sourcePointer!
