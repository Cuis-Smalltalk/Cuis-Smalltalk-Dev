'From Cuis7.9 [latest update: #8123] on 31 July 2026 at 2:58:00 pm'!

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 31/Jul/2026, 5:50:45 pm (UTC)'!
getPreamble
	| file preamble |
	self fileIndex = 0 ifTrue: [^ String new].  "no source pointer for this method"
	file := SourceFiles at: self fileIndex.
	file ifNil: [^ ''].  "sources file not available"
	"file does not exist happens in secure mode"
	[
		file name asFullFileEntry readStreamDo: [ :stream | | p |
			preamble := ''.
			p := 0 max: self filePosition.
			p < stream size ifTrue: [
				"It appears that on Linux systems, immediately after adding a
				new method, the OS file cache may not be updated, so contents
				may still not be accessible. Protect against that."
				stream position: p.
				stream backChunk. "Skip back blank space."
				"Find and answer preamble chunk."
				preamble := stream backChunk.
				(preamble beginsWith: 'methodProperties:') ifTrue: [
					stream backChunk. "Skip back blank space."
					preamble := stream backChunk ]]]
	] on: FileDoesNotExistException do: [ :ex | preamble := '' ].
	^ preamble! !


!VersionsBrowser methodsFor: 'init & update' stamp: 'jmv 31/Jul/2026, 5:52:28 pm (UTC)'!
scanVersionsOf: method class: class meta: meta category: category selector: selector
	| position stamp priorMethodRef prevPos prevFileIndex libraryName preamble tokens sourceFilesCopy |
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
		(preamble beginsWith: 'methodProperties:') ifTrue: [
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
				stamp := CompiledMethod stampFrom: tokens.
				priorMethodRef := CompiledMethod priorReferenceFrom: tokens.
				priorMethodRef ifNotNil: [
					prevFileIndex := sourceFilesCopy fileIndexFromSourcePointer: priorMethodRef.
					prevPos := sourceFilesCopy filePositionFromSourcePointer: priorMethodRef ].
				libraryName := CompiledMethod libraryNameFrom: tokens ].
 		self addItem:
				(MethodDefinitionChange new
					file: file position: position
					class: class name category: category meta: meta stamp: stamp
					prior: priorMethodRef libraryName: libraryName)
			text: stamp , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector.
		position := prevPos.
		prevPos notNil ifTrue: [
			file := sourceFilesCopy at: prevFileIndex]].
	sourceFilesCopy do: [:x | x notNil ifTrue: [x close]].
	self clearSelections! !


!ClassCommentVersionsBrowser methodsFor: 'basic function' stamp: 'jmv 31/Jul/2026, 5:53:20 pm (UTC)'!
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
	oldCommentRemoteStr ifNil:[^ nil] ifNotNil: [oldCommentRemoteStr sourcePointer].

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
				(CompiledMethod priorReferenceFrom: tokens) ifNotNil: [ :priorRef |
					prevFileIndex := sourceFilesCopy fileIndexFromSourcePointer: priorRef.
					prevPos := sourceFilesCopy filePositionFromSourcePointer: priorRef ]]
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

