'From Cuis7.9 [latest update: #8259] on 2 October 2026 at 11:26:06 am'!
!classDefinition: #CodeHistory category: #'System-CodeHistory'!
Object subclass: #CodeHistory
	instanceVariableNames: 'itemsData count removals'
	classVariableNames: 'Current'
	poolDictionaries: ''
	category: 'System-CodeHistory'!

!CodeHistory commentStamp: 'jmv 24/Sep/2026, 8:54:10 pm (UTC)' prior: 0!
Usually there's only one instance of me.

itemsData, count:
	A collection of entries holding a point in history of a CompiledMethod or ClassDefinition.
	libraryIndex:
		States where the actual method / class definition resides.
			16r00000000 : 	No Source
			16rFFFFFFFF : 	Sources file
			16rFFFFFFFE : 	Changes file
			Any other : 		Library number in Library Registry
	positionIndex: 
		Position of source code or of header chunk. Pensar. 0 if No Source.
	prevItem:
		Index of previous entry for this Method or Class. 0 if none.
	nextItem:
		Index of next entry for this Method or Class. 0 if none.

removals:
	A Dictionary with keys:
		MethodReference
	The value is the index to the item for the removal.

Existing methods have a #historyPointer that point to the item of their definition (source code). But deleted methods can't. removals is used for finding the removal (chained to the previous definition) when re-adding a removed method.!


!CodeHistory methodsFor: 'initialization' stamp: 'jmv 23/Sep/2026, 1:13:00 pm (UTC)'!
initialize
	itemsData := WordArray new: 0.
	count := 0.
	removals := Dictionary new.! !

!CodeHistory methodsFor: 'initialization' stamp: 'jmv 25/Sep/2026, 6:56:42 pm (UTC)'!
initializeNotifications
	"
	CodeHistory initialize
	"

	"Avoid double registration"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.
	SystemChangeNotifier uniqueInstance
		"when: #classAdded send: #classAdded:inCategory: to: self;
		when: #classCommented send: #classCommented: to: self;
		when: #classDefinitionChanged send: #classDefinitionChangedFrom:to:oldClassDefinition: to: self;
		when: #classRecategorized send: #classRecategorized:from:to: to: self;
		when: #classRemoved send: #classRemoved:fromCategory: to: self;
		when: #aboutToRenameClass send: #aboutToRenameClass:from:to:inCategory: to: self;
		when: #classReorganized send: #classReorganized: to: self;"
		when: #methodAddedInProtocolTimeStamp send: #methodAdded:selector:inProtocol:class:methodTimeStamp: to: self;
		when: #methodChanged send: #methodChangedFrom:to:selector:inClass: to: self;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: self;
		"when: #selectorRecategorized send: #selectorRecategorized:from:to:inClass: to: self;"
		yourself.! !

!CodeHistory methodsFor: 'add / update' stamp: 'jmv 24/Sep/2026, 8:50:59 pm (UTC)'!
addLibraryIndex: libraryIndex positionIndex: positionIndex prevItem: prevItem nextItem: nextItem

	count * 4 = itemsData size ifTrue: [
		itemsData := itemsData grownTo: count + (count // 10 max: 1000) * 4 ].

	itemsData at: count*4+1 put: libraryIndex.
	itemsData at: count*4+2 put: positionIndex.
	itemsData at: count*4+3 put: prevItem.
	itemsData at: count*4+4 put: nextItem.
	count := count + 1.
	^count! !

!CodeHistory methodsFor: 'add / update' stamp: 'jmv 23/Sep/2026, 1:59:49 pm (UTC)'!
at: index updateNextItem: aNumber
	itemsData at: index-1*4+4 put: aNumber.! !

!CodeHistory methodsFor: 'add / update' stamp: 'jmv 25/Sep/2026, 6:50:41 pm (UTC)'!
at: index updatePrevItem: aNumber
	itemsData at: index-1*4+3 put: aNumber.! !

!CodeHistory methodsFor: 'querying' stamp: 'jmv 23/Sep/2026, 2:42:46 pm (UTC)'!
libraryIndexAt: index
	^itemsData at: index-1*4+1! !

!CodeHistory methodsFor: 'querying' stamp: 'jmv 30/Sep/2026, 8:59:39 pm (UTC)'!
nextItemIndexAt: index
	^itemsData at: index-1*4+4! !

!CodeHistory methodsFor: 'querying' stamp: 'jmv 23/Sep/2026, 2:42:53 pm (UTC)'!
positionIndexAt: index
	^itemsData at: index-1*4+2! !

!CodeHistory methodsFor: 'querying' stamp: 'jmv 30/Sep/2026, 8:59:28 pm (UTC)'!
prevItemIndexAt: index
	^itemsData at: index-1*4+3! !

!CodeHistory methodsFor: 'services' stamp: 'jmv 2/Oct/2026, 2:22:39 pm (UTC)'!
methodSourceAndMetadataFor: historyPointer into: aBlock
	"Evaluate a block with information for method stored at historyPointer (that points to source code)
		- method source code
		- method stamp string as saved (nil if not present)
		- method properties (nil if not present)
		- previousHistoryPointer (0 if not present)
		- nextHistoryPointer (0 if not present)
	
	CodeHistory current methodSourceAndMetadataFor: (Object >> #=) historyPointer
		into: [ :methodSource :stamp :methodProperties :prev :next |
			'-----' print.
			methodSource print.
			stamp print.
			methodProperties print.
			prev print.
			next print ]
	"

	| fileIndex position preamble tokens methodSource methodPropertiesString stampString prevHistoryPointer nextHistoryPointer |

	methodSource := nil.
	methodPropertiesString := ''.
	stampString := nil.
	historyPointer < 16rFC000000
		ifTrue: [
			fileIndex := self libraryIndexAt: historyPointer.
			fileIndex = 16rFFFFFFFF ifTrue: [ fileIndex := 1 ]. "Sources"
			"Maybe at some point no CompiledMethod will point to .changes. Only CodeHistory and .sources"
			fileIndex = 16rFFFFFFFE ifTrue: [ fileIndex := 2 ]. "Changes"
			position := self positionIndexAt: historyPointer.
			prevHistoryPointer := self prevItemIndexAt: historyPointer.
			nextHistoryPointer := self nextItemIndexAt: historyPointer ]
		ifFalse: [
			fileIndex := self class fileIndexFromHistoryPointer: historyPointer.
			position := self class filePositionFromHistoryPointer: historyPointer.
			prevHistoryPointer := 0.
			nextHistoryPointer := 0 ].
	
	"Not if historyPointer points to a method removal. In that case, methodSource will be nil."
	(fileIndex = 0 and: [position = 0]) ifFalse: [
		 (SourceFiles at: fileIndex) name asFullFileEntry readStreamDo: [ :sourceFile |
			"Read source"
			sourceFile position: position.
			methodSource := sourceFile nextChunk withCuisLineEndings.
			"Point to source. Find Preamble"
			sourceFile position: position.
			sourceFile backChunk. "Skip back blank space filler."
			"Get preamble (and Method Properties if present)"
			preamble := sourceFile backChunk.
			(preamble beginsWith: 'methodProperties:') ifTrue: [
				methodPropertiesString := (preamble copyFrom: 1 + 'methodProperties:' size to: preamble size).
				sourceFile backChunk. "Skip back blank space."
				preamble := sourceFile backChunk ]].

		"Preamble is likely a linked method preamble, if we're in
			a changes file (not the sources file).  Try to parse it
			for prior source position and file index"
		(preamble includesSubString: 'methodsFor:')
			ifTrue: [
				tokens := Scanner new scanTokens: preamble.
				stampString := CompiledMethod stampFrom: tokens.
				"At some point, #prior: pointer in changes file will no longer be used."
				prevHistoryPointer = 0 ifTrue: [ 	"CodeHistory takes precedence"
					prevHistoryPointer := (CompiledMethod priorReferenceFrom: tokens) ifNil: [ 0 ].
					prevHistoryPointer = 0 ifFalse: [ prevHistoryPointer := prevHistoryPointer + 16rFC000000 - 16r1000000 ]]].
	].

	aBlock valueWithArguments: {methodSource. stampString. methodPropertiesString. prevHistoryPointer. nextHistoryPointer}.! !

!CodeHistory methodsFor: 'system change notifications' stamp: 'jmv 1/Oct/2026, 6:43:41 pm (UTC)'!
methodAdded: newMethod selector: aSymbol inProtocol: aCategoryName class: aClass methodTimeStamp: aTimeStampString
	| newItem prevItem fileIndex posIndex libIndex |
	
	"newMethod (a new version of oldMethod) was just installed and saved to the changes file.
	Add it to the CodeHistory."
	
	"If re-added, take the entry for the removal. We don't have the old method, but its entry
	in CodeHistory was stored in removals. Retrieve it and remove it from removals."
	prevItem := removals removeKey: newMethod asMethodReference ifAbsent: [0]. 
	
	"Point to changes file / position"
	libIndex := 0.
	fileIndex := newMethod fileIndex.
	posIndex := newMethod filePosition.
	fileIndex = 1 ifTrue: [ libIndex := 16rFFFFFFFF ]. "Sources"
	fileIndex = 2 ifTrue: [ libIndex := 16rFFFFFFFE ]. "Changes"
	
	"Add new entry to CodeHistory"
	newItem := self addLibraryIndex: libIndex positionIndex: posIndex prevItem: prevItem nextItem: 0.

	"If method removal already in CodeHistory, update it"
	prevItem = 0 ifFalse: [
		self at: prevItem updateNextItem: newItem ].

	"Make method point to the entry just added"
	newMethod historyPointer: newItem.! !

!CodeHistory methodsFor: 'system change notifications' stamp: 'jmv 1/Oct/2026, 6:41:10 pm (UTC)'!
methodChangedFrom: oldMethod to: newMethod selector: aSymbol inClass: aClass
	| newItem prevItem fileIndex libIndex posIndex |
	
	"newMethod (a new version of oldMethod) was just installed and saved to the changes file.
	Add it to the CodeHistory."
	
	"Point to changes file / position"
	libIndex := 0.
	fileIndex := newMethod fileIndex.
	posIndex := newMethod filePosition.
	fileIndex = 1 ifTrue: [ libIndex := 16rFFFFFFFF ]. "Sources"
	fileIndex = 2 ifTrue: [ libIndex := 16rFFFFFFFE ]. "Changes"
	
	"Add new entry to CodeHistory"
	prevItem := oldMethod historyPointer.
	newItem := self addLibraryIndex: libIndex positionIndex: posIndex prevItem: prevItem nextItem: 0.

	"If method already in CodeHistory, update its previous entry to point to new entry"
	(prevItem > 0 and: [
	prevItem < 16rFC000000]) ifTrue: [
		self at: prevItem updateNextItem: newItem ].

	"Make method point to the entry just added"
	newMethod historyPointer: newItem.! !

!CodeHistory methodsFor: 'system change notifications' stamp: 'jmv 1/Oct/2026, 6:46:09 pm (UTC)'!
methodRemoved: oldMethod selector: aSymbol inProtocol: protocol class: aClass
	| newItem prevItem libIndex posIndex |
	
	"oldMethod was just removed. Add an entry to the CodeHistory."

	"Add en entry in CodeHistory for the removal"
	libIndex := 0.
	posIndex := 0.
	prevItem := oldMethod historyPointer.
	newItem := self addLibraryIndex: libIndex positionIndex: posIndex prevItem: prevItem nextItem: 0.

	"If oldMethod was already in CodeHistory, update its previous entry"
	(prevItem > 0 and: [
	prevItem < 16rFC000000]) ifTrue: [
		self at: prevItem updateNextItem: newItem ].

	"Store the new item in removals.
	This will let us keep the chain of items in case the method is added again."
	removals at: oldMethod asMethodReference put: newItem.! !


!CodeHistory class methodsFor: 'accessing' stamp: 'jmv 23/Sep/2026, 1:57:14 pm (UTC)'!
current
	^Current! !

!CodeHistory class methodsFor: 'history pointer conversion' stamp: 'jmv 2/Oct/2026, 1:25:57 pm (UTC)'!
fileIndexFromHistoryPointer: anInteger
	"Return the index of the source file which contains the source chunk addressed by anInteger"
	"No source pointer => no fileIndex"
	
	anInteger = 0 ifTrue: [ ^0 ].
	anInteger >= 16rFC000000 ifFalse: [
		self error: 'history pointer does not encode .sources or .changes file index'].
	^(anInteger - 16rFC000000 anyMask: 16r2000000)
		ifTrue: [2]
		ifFalse: [1]! !

!CodeHistory class methodsFor: 'history pointer conversion' stamp: 'jmv 2/Oct/2026, 1:26:01 pm (UTC)'!
filePositionFromHistoryPointer: anInteger
	"Return the position of the source chunk addressed by anInteger"

	| unscaledPosition |
	"No source pointer => no filePosition"
	anInteger = 0 ifTrue: [ ^0 ].
	anInteger >= 16rFC000000 ifFalse: [
		self error: 'history pointer does not encode file position in .sources or .changes'].
	unscaledPosition := (anInteger - 16rFC000000) bitAnd: 16r1FFFFFF.
	^unscaledPosition * SourceFiles pointerScaleForWriting! !

!CodeHistory class methodsFor: 'history pointer conversion' stamp: 'jmv 2/Oct/2026, 1:26:05 pm (UTC)'!
historyPointerFromFileIndex: index andPosition: position
	"Return a sourcePointer encoding the given file index and position"
	| answer changesFlag |
	((index between: 1 and: 2) and: [position >= 0])
		ifFalse: [self error: 'invalid source code pointer'].
	changesFlag := index = 2 ifTrue: [ 16r2000000 ] ifFalse: [ 0 ].
	answer := (position // SourceFiles pointerScaleForWriting bitOr: changesFlag) + 16rFC000000.
	^answer! !

!CodeHistory class methodsFor: 'class initialization' stamp: 'jmv 30/Sep/2026, 8:54:55 pm (UTC)'!
initialize
	Current := self new.
	Current initializeNotifications.! !
