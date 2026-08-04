'From Cuis7.9 [latest update: #8127] on 3 August 2026 at 2:46:26 pm'!
!classDefinition: #MethodDefinitionChange category: #'Tools-Changes'!
ChangeListElement subclass: #MethodDefinitionChange
	instanceVariableNames: 'file position class category meta stamp prior libraryName properties '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!Array methodsFor: 'printing' stamp: 'jmv 3/Aug/2026, 5:24:55 pm (UTC)'!
storePropertiesOn: aStream
	"Convenient polymorphism with AdditionalMethodState."

	self storeAsBraceFormOn: aStream! !


!CompiledMethod methodsFor: 'accessing' stamp: 'jmv 31/Jul/2026, 8:32:29 pm (UTC)'!
propertiesAsArray
	"Answer an Array with all the method properties."

	^self 
		withPropertiesDo: [ :properties | properties propertiesAsArray ] 
		ifSelector: [ :selector | #() ]! !


!AdditionalMethodState methodsFor: 'converting' stamp: 'jmv 31/Jul/2026, 8:28:34 pm (UTC)'!
asArray
	"Include both Pragmas and Associations (method properties)"
	^Array streamContents: [ :strm |
		1 to: self basicSize do: [:i |
			strm nextPut: (self basicAt: i) ]].
! !

!AdditionalMethodState methodsFor: 'converting' stamp: 'jmv 31/Jul/2026, 8:31:10 pm (UTC)'!
propertiesAsArray
	"Include only the properties. Skip Pragmas."

	| element "<Association|Pragma>" |
	^Array streamContents: [ :strm |
		1 to: self basicSize do: [:i |
			(element := self basicAt: i) isPragma ifFalse: [
				strm nextPut: element ]]].
! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 31/Jul/2026, 9:03:35 pm (UTC)'!
scanCategory: category class: class meta: meta stamp: stamp prior: priorVersionPosition
	"Read methods following a method category preamble in a chunk file.
	See #methods, #methodsFor:*, scanCategory:*"

	| methodProperties libNameOrNil itemPosition methodSource |
	[
		methodProperties := nil.
		libNameOrNil := nil.
		file skipSeparators.
		(file peekFor: $!!) ifTrue: [ | keyword additionalChunk |
			keyword := file nextKeyword.
			keyword = '' ifTrue: [ ^self ]. "done when double terminators"
			additionalChunk := file nextChunk.
			keyword = 'methodProperties:' ifTrue: [
				methodProperties := Compiler evaluate: additionalChunk.
				methodProperties do: [ :assoc |
					assoc key == #libraryName ifTrue: [ libNameOrNil := assoc value ]]]].
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
					, (libNameOrNil ifNil: [''] ifNotNil: [' - [libraryName: ', libNameOrNil, ']'])
					", (' [methodProperties: ', methodProperties printString, '] ')"
					, (stamp isEmpty ifTrue: [''] ifFalse: [' - ' , stamp])
		] repeat.
! !


!MethodDefinitionChange methodsFor: 'accessing' stamp: 'jmv 31/Jul/2026, 8:44:25 pm (UTC)'!
properties
	^properties! !

!MethodDefinitionChange methodsFor: 'initialization' stamp: 'jmv 31/Jul/2026, 6:38:43 pm (UTC)'!
file: aFile position: aPosition class: aClassName category: aClassCategory meta: isMeta stamp: aStamp properties: anArrayOrNil prior: aPrior

	file := aFile.
	position := aPosition.
	class := aClassName.
	category := aClassCategory.
	meta := isMeta.
	stamp := aStamp.
	properties := anArrayOrNil.
	prior := aPrior.! !


!AdditionalMethodState methodsFor: 'properties' stamp: 'jmv 3/Aug/2026, 5:33:39 pm (UTC)'!
propertyKeysAndValuesDo: aBlock
	"Enumerate the receiver.
	Include only the properties. Skip Pragmas."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		(propertyOrPragma := self basicAt: i) isPragma ifFalse: [
			aBlock value: propertyOrPragma key value: propertyOrPragma value]]! !

!AdditionalMethodState methodsFor: 'printing' stamp: 'jmv 3/Aug/2026, 5:25:09 pm (UTC)'!
storePropertiesOn: aStream
	"Store as an Array."

	| isFirst |
	aStream nextPut: ${.
	isFirst := true.
	self propertiesDo: [ :assoc |
		isFirst ifTrue: [isFirst := false] ifFalse: [ aStream nextPut: $.; newLine; tab ].
		assoc storeOn: aStream ].
	aStream nextPut: $}.
! !


!Decompiler class methodsFor: 'testing' stamp: 'jmv 3/Aug/2026, 5:36:28 pm (UTC)'!
recompileAllTest
	"[Decompiler recompileAllTest]"
	"decompile every method and compile it back; if the decompiler is correct then the system should keep running.  :)"
	
	Smalltalk allBehaviorsDo: [ :behavior |
		Utilities informUser: (behavior printString) during: [
			behavior selectors do: [ :sel |
				| existingMethod decompiled ast compiled |
				decompiled := (Decompiler new decompile: sel in: behavior) decompileString.
				ast := Compiler new compile: decompiled in: behavior notifying: nil ifFail: [ self error: 'failed' ].
				existingMethod := behavior compiledMethodAt: sel.
				compiled := ast generate: existingMethod trailer.
				existingMethod propertiesAsArray do: [ :assoc |
					compiled propertyValueAt: assoc key put: assoc value ].
				behavior addSelectorSilently: sel withMethod: compiled ] ] ].
! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 31/Jul/2026, 6:24:12 pm (UTC)'!
scanMethodDefinition: tokens

	| stamp className priorVersionPosition |
	
	className := tokens first.
	stamp := CompiledMethod stampFrom: tokens.
	priorVersionPosition := CompiledMethod priorReferenceFrom: tokens.
	
	tokens second == #methodsFor: ifTrue: [
		self scanCategory: tokens third class: className meta: false stamp: stamp prior: priorVersionPosition.
		^self ].

	tokens third == #methodsFor: ifTrue: [
		self scanCategory: tokens fourth class: className meta: true stamp: stamp prior: priorVersionPosition.
		^self ].
	
	self error: 'Unsupported method definition' 
! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 31/Jul/2026, 6:24:26 pm (UTC)'!
scanMethodDefinitionNoCats: tokens

	| className |
	
	className := tokens first.
	
	tokens second == #methods ifTrue: [
		self scanCategory: ClassOrganizer defaultMethodCategory class: className meta: false stamp: '' prior: nil.
		^self ].

	(tokens second == #class and: [ tokens third == #methods]) ifTrue: [
		self scanCategory: ClassOrganizer defaultMethodCategory class: className meta: true stamp: '' prior: nil.
		^self ].
	
	self error: 'Unsupported method definition' 
! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 31/Jul/2026, 8:59:06 pm (UTC)'!
removeUpToDate
	"Remove all up to date version of entries from the receiver"

	| newChangeList newList |
	newChangeList := OrderedCollection new.
	newList := OrderedCollection new.
	changeList with: list do: [ :chRec :strNstamp | | keep cls name |
		keep := chRec isClassDeletion not or: [ chRec changeClass notNil ]. "If a class deletion, and class already gone, don't keep it"
		keep ifTrue: [
			(cls := chRec changeClass) ifNotNil: [ | sel str |
				str := chRec string.
				sel := chRec methodSelector.
				keep := chRec isMethodDeletion
					ifTrue: [cls includesSelector: sel]
					ifFalse: [
						((cls sourceCodeAt: sel ifAbsent: nil)  = str and: [
							chRec isMethodDefinition and: [
							(cls includesSelector: sel) and: [
							chRec properties isNil or: chRec properties = (cls compiledMethodAt: sel) propertiesAsArray ]]])
						not ]]].
		(chRec isClassComment and: [
				name := chRec changeClassName.
				Smalltalk includesKey: name]) ifTrue: [
			cls := Smalltalk at: name.
			keep := cls organization classComment ~= chRec text ].
		(chRec isClassDefinition and: [
				name := chRec changeClassName.
				Smalltalk includesKey: name]) ifTrue: [
			cls := Smalltalk at: name.
			chRec isMetaClassChange ifTrue: [ cls := cls class ].
			keep := cls definition ~= chRec text ].
		keep ifTrue: [
			newChangeList add: chRec.
			newList add: strNstamp]].
	newChangeList size < changeList size ifTrue: [
		changeList := newChangeList.
		list := newList.
		listIndex := 0.
		self clearSelections ].
	self changed: #list! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 31/Jul/2026, 8:50:43 pm (UTC)'!
selectEquivalentMethods
	"Selects all method definitions for which there is already an equivalent method in the current image, 
	(meaning that the difference is cosmetic and not in behavior)"
	1 to: changeList size do: [ :i | 
		| change class |
		change := changeList at: i.
		listSelections at: i put:
			((change isMethodDefinition and: [
				(class := change changeClass) notNil]) and: [
					(class includesSelector: change methodSelector) and: [
						| cmWithNode |
						cmWithNode := [class basicCompile: change string notifying: nil trailer: class defaultMethodTrailer ifFail: nil] 
							on: SyntaxErrorNotification do: [ :ex | ex return ].
						(cmWithNode notNil and: [
							| current inChange currentProperties |
							current := (class compiledMethodAt: change methodSelector) copyWithTrailerBytes: #(0).
							currentProperties := current propertiesAsArray.
							inChange := cmWithNode method copyWithTrailerBytes: #(0).
							current = inChange or: [
								| currentCmWithNode |
								currentCmWithNode := [class basicCompile: (class decompilerClass new decompile: change methodSelector in: class) decompileString
										notifying: nil trailer: class defaultMethodTrailer ifFail: nil] on: SyntaxErrorNotification do: [ :ex | ex return ].
								(currentCmWithNode notNil and: [
									current := currentCmWithNode method copyWithTrailerBytes: #(0).
									current = inChange and: [
										(change properties isNil or: [ change properties = currentProperties ])]])
							]
						])
					]]
			)].
	self changed: #allSelections.
	self changed: #annotation! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 31/Jul/2026, 8:51:40 pm (UTC)'!
selectUnchangedMethods
	"Selects all method definitions for which there is already a method in the current image, whose source is exactly the same.  9/18/96 sw"

	| change class |
	1 to: changeList size do: [ :i | 
		change := changeList at: i.
		listSelections at: i put:
			((change isMethodDefinition and:
				[(class := change changeClass) notNil]) and:
					[(class includesSelector: change methodSelector) and:
						[change string = (class sourceCodeAt: change methodSelector) asPlainString and:
							[change properties isNil or: [ change properties = 
								(class compiledMethodAt: change methodSelector) propertiesAsArray]]]])].
	self changed: #allSelections.
	self changed: #annotation.! !


!VersionsBrowser methodsFor: 'init & update' stamp: 'jmv 31/Jul/2026, 8:52:20 pm (UTC)'!
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
					, (libNameOrNil ifNil: [''] ifNotNil: [' - [libraryName: ', libNameOrNil, ']']).
					", (' [methodProperties: ', methodProperties printString, '] ')."
		position := prevPos.
		prevPos notNil ifTrue: [
			file := sourceFilesCopy at: prevFileIndex]].
	sourceFilesCopy do: [:x | x notNil ifTrue: [x close]].
	self clearSelections! !


!MethodDefinitionChange methodsFor: 'accessing' stamp: 'jmv 3/Aug/2026, 5:20:57 pm (UTC)'!
fileOutOn: aFileStream
	"File the receiver out on the given file stream"

	self changeClass printCategoryChunk: category on: aFileStream
		withStamp: stamp priorMethod: nil.
	self changeClass printPropertiesChunk: properties on: aFileStream.
	aFileStream newLine.
	aFileStream nextChunkPut: self string.
	aFileStream nextChunkPut: ' '; newLine.
	aFileStream newLine.! !

!MethodDefinitionChange methodsFor: 'initialization' stamp: 'jmv 3/Aug/2026, 5:15:34 pm (UTC)'!
fileIn
	"File the receiver in.  Apply the change.  Define the method."

	self changeClass ifNotNil: [ :methodClass |
		methodClass
			compile: self text
			classified: category
			withStamp: stamp
			methodProperties: properties
			notifying: nil ].! !

!methodRemoval: MethodDefinitionChange #file:position:class:category:meta:stamp:prior:libraryName: stamp: 'jmv 3/Aug/2026, 5:43:14 pm (UTC)'!
MethodDefinitionChange removeSelector: #file:position:class:category:meta:stamp:prior:libraryName:!
!methodRemoval: MethodDefinitionChange #libraryName stamp: 'jmv 3/Aug/2026, 5:43:17 pm (UTC)'!
MethodDefinitionChange removeSelector: #libraryName!
!methodRemoval: ChangeList #scanCategory:class:meta:stamp:prior:libraryName: stamp: 'jmv 3/Aug/2026, 5:42:55 pm (UTC)'!
ChangeList removeSelector: #scanCategory:class:meta:stamp:prior:libraryName:!
!methodRemoval: CompiledMethod class #libraryNameFrom: stamp: 'jmv 3/Aug/2026, 5:42:58 pm (UTC)'!
CompiledMethod class removeSelector: #libraryNameFrom:!
!methodRemoval: ClassDescription #compile:classified:withStamp:libraryName:notifying: stamp: 'jmv 3/Aug/2026, 5:43:06 pm (UTC)'!
ClassDescription removeSelector: #compile:classified:withStamp:libraryName:notifying:!
!methodRemoval: ClassDescription #compile:classified:withStamp:libraryName:notifying:logSource: stamp: 'jmv 3/Aug/2026, 5:43:08 pm (UTC)'!
ClassDescription removeSelector: #compile:classified:withStamp:libraryName:notifying:logSource:!
!methodRemoval: ClassDescription #printCategoryChunk:on:withStamp:libraryName:priorMethod: stamp: 'jmv 3/Aug/2026, 5:43:01 pm (UTC)'!
ClassDescription removeSelector: #printCategoryChunk:on:withStamp:libraryName:priorMethod:!

!AdditionalMethodState reorganize!
('accessing' at: at:ifAbsent: keysAndValuesDo: method: pragmas removeKey:ifAbsent: selector selector: setMethod:)
('converting' asArray propertiesAsArray)
('properties' propertiesDo: propertyKeysAndValuesDo: propertyOrPragmaAt: propertyValueAt: propertyValueAt:ifAbsent: removeKey:)
('copying' copyWith: copyWithout:)
('literals' allLiteralsDo:)
('testing' analogousCodeTo: hasAtLeastTheSamePropertiesAs: hasLiteralSuchThat: includesKey: isEmpty notEmpty)
('printing' printOn: storePropertiesOn:)
!

!classDefinition: #MethodDefinitionChange category: #'Tools-Changes'!
ChangeListElement subclass: #MethodDefinitionChange
	instanceVariableNames: 'file position class category meta stamp prior properties'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
