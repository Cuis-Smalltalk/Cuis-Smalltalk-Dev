'From Cuis7.9 [latest update: #8241] on 29 September 2026 at 3:22:59 pm'!
!classDefinition: #MethodDefinitionChange category: #'Tools-Changes'!
ChangeListElement subclass: #MethodDefinitionChange
	instanceVariableNames: 'file position class category meta stamp prior properties methodSelector '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ChangeList methodsFor: 'scanning' stamp: 'jmv 29/Sep/2026, 6:19:08 pm (UTC)'!
scanCategory: category class: class meta: meta stamp: stamp
	"Read methods following a method category preamble in a chunk file.
	See #methods, #methodsFor:*, scanCategory:*"

	| methodProperties libNameOrNil itemPosition methodSource selector |
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
		selector := ((Smalltalk at: class ifAbsent: [Object class]) parserClass selectorFrom: methodSource) ifNil: [''].
		self
			addItem: (MethodDefinitionChange new
				file: file position: itemPosition
				class: class category: category meta: meta stamp: stamp
				properties: methodProperties
				selector: selector)
			text: 'method: ' , class , (meta ifTrue: [' class '] ifFalse: [' '])
				, selector
				, (stamp isEmpty ifTrue: [''] ifFalse: [' - ' , stamp])
				", (libNameOrNil ifNil: [''] ifNotNil: [' - [libraryName: ', libNameOrNil, ']'])"
				, (methodProperties ifNil: [''] ifNotNil: [' [methodProperties: ', methodProperties printString, '] '])
	] repeat.! !


!MethodDefinitionChange methodsFor: 'initialization' stamp: 'jmv 29/Sep/2026, 6:16:57 pm (UTC)'!
file: aFile position: aPosition class: aClassName category: aClassCategory meta: isMeta stamp: aStamp properties: anArrayOrNil selector: selector

	file := aFile.
	position := aPosition.
	class := aClassName.
	category := aClassCategory.
	meta := isMeta.
	stamp := aStamp.
	properties := anArrayOrNil.
	methodSelector := selector.! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 29/Sep/2026, 6:19:24 pm (UTC)'!
scanMethodDefinition: tokens

	| stamp className |
	
	className := tokens first.
	stamp := CompiledMethod authorAndStampLocalTZFrom: tokens.
	
	tokens second == #methodsFor: ifTrue: [
		self scanCategory: tokens third class: className meta: false stamp: stamp.
		^self ].

	tokens third == #methodsFor: ifTrue: [
		self scanCategory: tokens fourth class: className meta: true stamp: stamp.
		^self ].
	
	self error: 'Unsupported method definition' 
! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 29/Sep/2026, 6:19:37 pm (UTC)'!
scanMethodDefinitionNoCats: tokens

	| className |
	
	className := tokens first.
	
	tokens second == #methods ifTrue: [
		self scanCategory: ClassOrganizer defaultMethodCategory class: className meta: false stamp: ''.
		^self ].

	(tokens second == #class and: [ tokens third == #methods]) ifTrue: [
		self scanCategory: ClassOrganizer defaultMethodCategory class: className meta: true stamp: ''.
		^self ].
	
	self error: 'Unsupported method definition' 
! !


!MethodVersionsBrowser methodsFor: 'init & update' stamp: 'jmv 29/Sep/2026, 6:18:38 pm (UTC)'!
scanVersionsOf: method class: class meta: meta category: category selector: selector
	| position stamp priorMethodRef prevPos prevFileIndex preamble methodProperties libNameOrNil tokens sourceFilesCopy sourceFile |
	selectorOfMethod := selector.
	currentCompiledMethod := method.
	classOfMethod := meta ifTrue: [class class] ifFalse: [class].
	changeList := OrderedCollection new.
	list := OrderedCollection new.
	listIndex := 0.



	position := method filePosition.
	sourceFilesCopy := SourceFiles collect:
		[:x | x ifNotNil: [ x name asFullFileEntry readStream ]].
	method fileIndex = 0 ifTrue: [^ nil].
	sourceFile := sourceFilesCopy at: method fileIndex.
	[position notNil & sourceFile notNil] whileTrue: [
		"Skip back filler"
		sourceFile position: position.
		sourceFile backChunk. "Skip back blank space."
		"Get preamble"
		preamble := sourceFile backChunk.
		methodProperties := nil.
		libNameOrNil := nil.
		(preamble beginsWith: 'methodProperties:') ifTrue: [
			methodProperties := Compiler evaluate: (preamble copyFrom: 'methodProperties:' size+1 to: preamble size).
			methodProperties do: [ :assoc |
				assoc key == #libraryName ifTrue: [ libNameOrNil := assoc value ]].
			sourceFile backChunk. "Skip back blank space."
			preamble := sourceFile backChunk ].
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
					file: sourceFile position: position
					class: class name category: category meta: meta stamp: stamp
					properties: methodProperties
					selector: selectorOfMethod)
			text: stamp , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector
					", (libNameOrNil ifNil: [''] ifNotNil: [' - [libraryName: ', libNameOrNil, ']'])."
					, (methodProperties ifNil: [''] ifNotNil: [' [methodProperties: ', methodProperties printString, '] ']).
		
		
		
		position := prevPos.
		prevPos notNil ifTrue: [
			sourceFile := sourceFilesCopy at: prevFileIndex]].
	sourceFilesCopy do: [:x | x notNil ifTrue: [x close]].! !

!methodRemoval: MethodDefinitionChange #file:position:class:category:meta:stamp:properties:prior:selector: stamp: 'jmv 29/Sep/2026, 6:19:51 pm (UTC)'!
MethodDefinitionChange removeSelector: #file:position:class:category:meta:stamp:properties:prior:selector:!
!methodRemoval: ChangeList #scanCategory:class:meta:stamp:prior: stamp: 'jmv 29/Sep/2026, 6:19:43 pm (UTC)'!
ChangeList removeSelector: #scanCategory:class:meta:stamp:prior:!
!classDefinition: #MethodDefinitionChange category: #'Tools-Changes'!
ChangeListElement subclass: #MethodDefinitionChange
	instanceVariableNames: 'file position class category meta stamp properties methodSelector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
