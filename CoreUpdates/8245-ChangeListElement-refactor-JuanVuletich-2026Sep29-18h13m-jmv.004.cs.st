'From Cuis7.9 [latest update: #8244] on 29 September 2026 at 6:35:08 pm'!
!classDefinition: #ClassCommentChange category: #'Tools-Changes'!
ChangeListElement subclass: #ClassCommentChange
	instanceVariableNames: 'file position class stamp string '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
!classDefinition: #ClassDefinitionChange category: #'Tools-Changes'!
ChangeListElement subclass: #ClassDefinitionChange
	instanceVariableNames: 'file position class category meta stamp string '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
!classDefinition: #MethodDefinitionChange category: #'Tools-Changes'!
ChangeListElement subclass: #MethodDefinitionChange
	instanceVariableNames: 'file position class category meta stamp properties methodSelector string '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
!classDefinition: #PreambleRecord category: #'Tools-Changes'!
ChangeListElement subclass: #PreambleRecord
	instanceVariableNames: 'file position stamp string '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
!classDefinition: #UnspecificRecord category: #'Tools-Changes'!
ChangeListElement subclass: #UnspecificRecord
	instanceVariableNames: 'file position stamp string '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ClassCommentChange methodsFor: 'initialization' stamp: 'jmv 29/Sep/2026, 9:16:23 pm (UTC)'!
class: aClassName stamp: aStamp string: aString

	class := aClassName.
	stamp := aStamp.
	string := aString.! !


!ClassDefinitionChange methodsFor: 'initialization' stamp: 'jmv 29/Sep/2026, 9:19:04 pm (UTC)'!
class: aClassName category: aClassCategory meta: isMeta stamp: aStamp string: aString

	class := aClassName.
	category := aClassCategory.
	meta := isMeta.
	stamp := aStamp.
	string := aString.! !


!MethodDefinitionChange methodsFor: 'initialization' stamp: 'jmv 29/Sep/2026, 9:23:12 pm (UTC)'!
class: aClassName category: aClassCategory meta: isMeta stamp: aStamp properties: anArrayOrNil selector: selector string: aString

	class := aClassName.
	category := aClassCategory.
	meta := isMeta.
	stamp := aStamp.
	properties := anArrayOrNil.
	methodSelector := selector.
	string := aString.! !


!PreambleRecord methodsFor: 'initialization' stamp: 'jmv 29/Sep/2026, 9:33:11 pm (UTC)'!
string: aString
	
	string := aString.! !


!UnspecificRecord methodsFor: 'initialization' stamp: 'jmv 29/Sep/2026, 9:33:15 pm (UTC)'!
string: aString
	
	string := aString.! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 29/Sep/2026, 9:29:12 pm (UTC)'!
classDefinitionRecordFrom: tokens
	| classDefinition isMeta itemPosition className record fullClassName category stamp |

	itemPosition := file position.
	fullClassName := tokens second.
	isMeta := fullClassName includesSubString: ' class'.
	className := isMeta ifTrue: [fullClassName substrings first] ifFalse: [fullClassName].
	category := CompiledMethod classCategoryFrom: tokens.
	stamp := CompiledMethod authorAndStampLocalTZFrom: tokens.
	classDefinition := file nextChunk withCuisLineEndings.

	record := ClassDefinitionChange new
		class: className asSymbol
		category: category
		meta: isMeta
		stamp: stamp
		string: classDefinition.

	^record! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 29/Sep/2026, 9:33:28 pm (UTC)'!
scanAndIgnore: item in: position

	| record |
	record := PreambleRecord new string: item.
	self 
		addItem: record 
		text: ('preamble: ' , item contractTo: 160).! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 29/Sep/2026, 9:29:34 pm (UTC)'!
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
		methodSource := file nextChunk withCuisLineEndings.
		methodSource = '' ifTrue: [ ^self ]. "done when double terminators"
		selector := ((Smalltalk at: class ifAbsent: [Object class]) parserClass selectorFrom: methodSource) ifNil: [''].
		self
			addItem: (MethodDefinitionChange new
				class: class category: category meta: meta stamp: stamp
				properties: methodProperties
				selector: selector
				string: methodSource)
			text: 'method: ' , class , (meta ifTrue: [' class '] ifFalse: [' '])
				, selector
				, (stamp isEmpty ifTrue: [''] ifFalse: [' - ' , stamp])
				", (libNameOrNil ifNil: [''] ifNotNil: [' - [libraryName: ', libNameOrNil, ']'])"
				, (methodProperties ifNil: [''] ifNotNil: [' [methodProperties: ', methodProperties printString, '] '])
	] repeat.! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 29/Sep/2026, 8:32:26 pm (UTC)'!
scanClassComment: tokens

	| className stamp record string |

	className := tokens first.
	stamp := tokens third.
	string := file nextChunk withCuisLineEndings.
	record := ClassCommentChange new
			class: className
			stamp: stamp
			string: string.
	self 
		addItem: record
		text: 'class comment for ' , className, (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp]).! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 29/Sep/2026, 9:33:42 pm (UTC)'!
scanDoIt
	
	| itemPosition command |
	itemPosition := file position.
	command := file nextChunk.
	command notEmpty ifTrue: [
		self
			addItem: (UnspecificRecord new string: command)
			text: 'do it: ' , (command contractTo: 160)].! !


!MethodVersionsBrowser methodsFor: 'init & update' stamp: 'jmv 29/Sep/2026, 9:29:58 pm (UTC)'!
scanVersionsOf: method class: class meta: meta category: category selector: selector
	| methodSource position stamp priorMethodRef prevPos prevFileIndex preamble methodProperties libNameOrNil tokens sourceFilesCopy sourceFile |
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
		"Read source"
		sourceFile position: position.
		methodSource := sourceFile nextChunk withCuisLineEndings.
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
					class: class name category: category meta: meta stamp: stamp
					properties: methodProperties
					selector: selectorOfMethod
					string: methodSource)
			text: stamp , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector
					", (libNameOrNil ifNil: [''] ifNotNil: [' - [libraryName: ', libNameOrNil, ']'])."
					, (methodProperties ifNil: [''] ifNotNil: [' [methodProperties: ', methodProperties printString, '] ']).
		
		
		
		position := prevPos.
		prevPos notNil ifTrue: [
			sourceFile := sourceFilesCopy at: prevFileIndex]].
	sourceFilesCopy do: [:x | x notNil ifTrue: [x close]].! !


!ChangeListElement methodsFor: 'accessing' stamp: 'jmv 29/Sep/2026, 9:27:53 pm (UTC)'!
text
	"In Cuis, all source code is plain Strings
	ehhhhh"
	^self string! !


!ClassCommentChange methodsFor: 'accessing' stamp: 'jmv 29/Sep/2026, 8:25:52 pm (UTC)'!
string
	^string! !


!ClassDefinitionChange methodsFor: 'accessing' stamp: 'jmv 29/Sep/2026, 9:22:06 pm (UTC)'!
string
	^string! !


!MethodDefinitionChange methodsFor: 'accessing' stamp: 'jmv 29/Sep/2026, 9:28:10 pm (UTC)'!
string
	^string! !


!PreambleRecord methodsFor: 'accessing' stamp: 'jmv 29/Sep/2026, 9:34:01 pm (UTC)'!
string
	^string! !


!UnspecificRecord methodsFor: 'accessing' stamp: 'jmv 29/Sep/2026, 9:34:03 pm (UTC)'!
string
	^string! !

!methodRemoval: UnspecificRecord #file:position: stamp: 'jmv 29/Sep/2026, 9:34:17 pm (UTC)'!
UnspecificRecord removeSelector: #file:position:!
!methodRemoval: PreambleRecord #file:position: stamp: 'jmv 29/Sep/2026, 9:34:17 pm (UTC)'!
PreambleRecord removeSelector: #file:position:!
!methodRemoval: MethodDefinitionChange #file:position:class:category:meta:stamp:properties:selector: stamp: 'jmv 29/Sep/2026, 9:27:10 pm (UTC)'!
MethodDefinitionChange removeSelector: #file:position:class:category:meta:stamp:properties:selector:!
!methodRemoval: ClassDefinitionChange #file:position:class:category:meta:stamp: stamp: 'jmv 29/Sep/2026, 9:22:10 pm (UTC)'!
ClassDefinitionChange removeSelector: #file:position:class:category:meta:stamp:!
!methodRemoval: ClassCommentChange #file:position:class:stamp: stamp: 'jmv 29/Sep/2026, 9:17:40 pm (UTC)'!
ClassCommentChange removeSelector: #file:position:class:stamp:!
!classDefinition: #ClassCommentChange category: #'Tools-Changes'!
ChangeListElement subclass: #ClassCommentChange
	instanceVariableNames: 'class stamp string'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
!classDefinition: #ClassDefinitionChange category: #'Tools-Changes'!
ChangeListElement subclass: #ClassDefinitionChange
	instanceVariableNames: 'class category meta stamp string'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
!classDefinition: #MethodDefinitionChange category: #'Tools-Changes'!
ChangeListElement subclass: #MethodDefinitionChange
	instanceVariableNames: 'class category meta stamp properties methodSelector string'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
!classDefinition: #PreambleRecord category: #'Tools-Changes'!
ChangeListElement subclass: #PreambleRecord
	instanceVariableNames: 'stamp string'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
!classDefinition: #UnspecificRecord category: #'Tools-Changes'!
ChangeListElement subclass: #UnspecificRecord
	instanceVariableNames: 'stamp string'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
