'From Cuis7.9 [latest update: #8180] on 14 September 2026 at 1:50:16 pm'!

!CompiledMethod class methodsFor: 'services' stamp: 'jmv 14/Sep/2026, 4:35:12 pm (UTC)'!
authorAndStampLocalTZ: authorAndStamp
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file.
	Answer the empty string if no time stamp is available.
	Format the date in the timestamp with the preferred style.
	(Object compiledMethodAt: #printString) authorAndStampAsSaved
	(Object compiledMethodAt: #printString) authorAndStamp
	"
	authorAndStamp isEmpty ifTrue: [ ^authorAndStamp ].
	^String streamContents: [ :strm | 
		self authorAndStamp: authorAndStamp partsDo: [ :authorInitials :dateAndTime |
			strm nextPutAll: authorInitials.
			strm space.
			dateAndTime printNiceLocalTimeZoneOn: strm ]]! !

!CompiledMethod class methodsFor: 'services' stamp: 'jmv 14/Sep/2026, 4:42:34 pm (UTC)'!
authorAndStampLocalTZFrom: tokens
	^self authorAndStampLocalTZ: (self stampFrom: tokens)! !

!CompiledMethod class methodsFor: 'services' stamp: 'jmv 14/Sep/2026, 4:35:47 pm (UTC)'!
authorAndStampUTC: authorAndStamp
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file.
	Answer the empty string if no time stamp is available.
	Format the date in the timestamp with the preferred style.
	(Object compiledMethodAt: #printString) authorAndStampAsSaved
	(Object compiledMethodAt: #printString) authorAndStamp
	"
	authorAndStamp isEmpty ifTrue: [ ^authorAndStamp ].
	^String streamContents: [ :strm | 
		self authorAndStamp: authorAndStamp partsDo: [ :authorInitials :dateAndTime |
			strm nextPutAll: authorInitials.
			strm space.
			dateAndTime asUTC printNiceOn: strm ]]! !


!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 14/Sep/2026, 4:36:47 pm (UTC)'!
authorAndStampLocalTZ
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file.
	Answer the empty string if no time stamp is available.
	Format the date in the timestamp with the preferred style.
	(Object compiledMethodAt: #printString) authorAndStampAsSaved
	(Object compiledMethodAt: #printString) authorAndStamp
	"
	^ self class authorAndStampLocalTZ: self authorAndStampAsSaved! !

!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 14/Sep/2026, 4:36:26 pm (UTC)'!
authorAndStampUTC
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file.
	Answer the empty string if no time stamp is available.
	Format the date in the timestamp with the preferred style.
	(Object compiledMethodAt: #printString) authorAndStampAsSaved
	(Object compiledMethodAt: #printString) authorAndStamp
	"
	^ self class authorAndStampUTC: self authorAndStampAsSaved! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 14/Sep/2026, 4:42:44 pm (UTC)'!
classDefinitionRecordFrom: tokens
	| classDefinition isMeta itemPosition className record fullClassName category stamp |

	itemPosition := file position.
	fullClassName := tokens second.
	isMeta := fullClassName includesSubString: ' class'.
	className := isMeta ifTrue: [fullClassName substrings first] ifFalse: [fullClassName].
	category := CompiledMethod classCategoryFrom: tokens.
	stamp := CompiledMethod authorAndStampLocalTZFrom: tokens.
	classDefinition := file nextChunk.

	record := ClassDefinitionChange new
		file: file
		position: itemPosition
		class: className asSymbol
		category: category
		meta: isMeta
		stamp: stamp.

	^record! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 14/Sep/2026, 4:43:04 pm (UTC)'!
scanClassRemoval: tokens

	| packageOrBaseImageNameOrNil removeType classDefinition className description record stamp |
	removeType := tokens first.
	className := tokens second.
	packageOrBaseImageNameOrNil := (tokens size > 3 and: [ tokens third = #restrictedTo: ])
		ifTrue: [ tokens fourth ].

	stamp := CompiledMethod authorAndStampLocalTZFrom: tokens.
	classDefinition := file nextChunk.

	record := ClassDeletionChange
			className: className
			definition: classDefinition
			appliesOnlyTo: packageOrBaseImageNameOrNil
			stamp: stamp.

	description := packageOrBaseImageNameOrNil notNil
			ifTrue: ['clase move out (to some package or BaseImage): ']
			ifFalse: ['class removal: '].

	self
		addItem: record
		text: description, className, '; ', stamp.! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 14/Sep/2026, 4:43:11 pm (UTC)'!
scanClassRenamed: tokens

	| record stamp newName previousName preamble |

	preamble := tokens first.
	previousName := tokens second.
	newName := tokens fourth.
	stamp := CompiledMethod authorAndStampLocalTZFrom: tokens.
	file nextChunk.
	
	record := ClassRenamedChange from: previousName to: newName stamp: stamp.
			
	self 
		addItem: record
		text: preamble, ' ', previousName, ' -> ', newName, '; ', stamp ! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 14/Sep/2026, 4:43:16 pm (UTC)'!
scanMethodDefinition: tokens

	| stamp className priorVersionPosition |
	
	className := tokens first.
	stamp := CompiledMethod authorAndStampLocalTZFrom: tokens.
	priorVersionPosition := CompiledMethod priorReferenceFrom: tokens.
	
	tokens second == #methodsFor: ifTrue: [
		self scanCategory: tokens third class: className meta: false stamp: stamp prior: priorVersionPosition.
		^self ].

	tokens third == #methodsFor: ifTrue: [
		self scanCategory: tokens fourth class: className meta: true stamp: stamp prior: priorVersionPosition.
		^self ].
	
	self error: 'Unsupported method definition' 
! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 14/Sep/2026, 4:43:22 pm (UTC)'!
scanMethodRemoval: tokens

	| packageOrBaseImageNameOrNil removeType isMeta sourceCode methodReference className description record selector stamp |
	removeType := tokens first.
	className := tokens second.
	packageOrBaseImageNameOrNil := (tokens size > 4 and: [ tokens fourth = #restrictedTo: ])
		ifTrue: [ tokens fifth ].

	sourceCode := file nextChunk.
	isMeta := tokens third == #class.	
	selector := isMeta ifTrue: [ tokens fourth ] ifFalse: [ tokens third ].
	stamp := CompiledMethod authorAndStampLocalTZFrom: tokens.

	methodReference := MethodReference new
		setClassSymbol: className
		classIsMeta: isMeta
		methodSymbol: selector
		stringVersion: className, (isMeta ifTrue: [' class '] ifFalse: [' ']), selector,'; ', stamp.

	record := MethodDeletionChange
		methodReference: methodReference
		appliesOnlyTo: packageOrBaseImageNameOrNil
		source: sourceCode
		stamp: stamp.

	description := packageOrBaseImageNameOrNil notNil
		ifTrue: ['method move out (to some package or BaseImage): ']
		ifFalse: ['method removal: '].

	self
		addItem: record
		text: description, methodReference stringVersion.! !


!VersionsBrowser methodsFor: 'init & update' stamp: 'jmv 14/Sep/2026, 4:43:30 pm (UTC)'!
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

!methodRemoval: ChangeList #timeStampString stamp: 'jmv 14/Sep/2026, 4:47:38 pm (UTC)'!
ChangeList removeSelector: #timeStampString!
