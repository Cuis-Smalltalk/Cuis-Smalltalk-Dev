'From Cuis7.9 [latest update: #8133] on 5 August 2026 at 3:57:47 pm'!

!ChangeList methodsFor: 'scanning' stamp: 'jmv 5/Aug/2026, 6:55:59 pm (UTC)'!
scanCategory: category class: class meta: meta stamp: stamp prior: priorVersionPosition
	"Read methods following a method category preamble in a chunk file.
	See #methods, #methodsFor:*, scanCategory:*"

	| methodProperties libNameOrNil itemPosition methodSource |
	[
		methodProperties := nil.
		libNameOrNil := nil.
		file skipSeparators.
		(file peekForString: '!!methodProperties:') ifTrue: [
			methodProperties := Compiler evaluate: file nextChunk.
				methodProperties do: [ :assoc |
					assoc key == #libraryName ifTrue: [ libNameOrNil := assoc value ]]].
		file skipSeparators.
		itemPosition := file position.
		methodSource := file nextChunk.
		methodSource = '' ifTrue: [ ^self ]. "done when double terminators"
			self
				addItem: (MethodDefinitionChange new
					file: file position: itemPosition
					class: class category: category meta: meta stamp: stamp
					properties: methodProperties
					prior: priorVersionPosition)
				text: 'method: ' , class , (meta ifTrue: [' class '] ifFalse: [' '])
					, (((Smalltalk at: class ifAbsent: [Object class]) parserClass selectorFrom: methodSource) ifNil: [''])
					, (stamp isEmpty ifTrue: [''] ifFalse: [' - ' , stamp])
					", (libNameOrNil ifNil: [''] ifNotNil: [' - [libraryName: ', libNameOrNil, ']'])"
					, (methodProperties ifNil: [''] ifNotNil: [' [methodProperties: ', methodProperties printString, '] '])
		] repeat.
! !


!VersionsBrowser methodsFor: 'init & update' stamp: 'jmv 5/Aug/2026, 6:56:49 pm (UTC)'!
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
				stamp := CompiledMethod stampFrom: tokens.
				priorMethodRef := CompiledMethod priorReferenceFrom: tokens.
				priorMethodRef ifNotNil: [
					prevFileIndex := sourceFilesCopy fileIndexFromSourcePointer: priorMethodRef.
					prevPos := sourceFilesCopy filePositionFromSourcePointer: priorMethodRef ]].
 		self addItem:
				(MethodDefinitionChange new
					file: file position: position
					class: class name category: category meta: meta stamp: stamp
					properties: methodProperties
					prior: priorMethodRef)
			text: stamp , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector
					", (libNameOrNil ifNil: [''] ifNotNil: [' - [libraryName: ', libNameOrNil, ']'])."
					, (methodProperties ifNil: [''] ifNotNil: [' [methodProperties: ', methodProperties printString, '] ']).
		position := prevPos.
		prevPos notNil ifTrue: [
			file := sourceFilesCopy at: prevFileIndex]].
	sourceFilesCopy do: [:x | x notNil ifTrue: [x close]].
	self clearSelections! !

