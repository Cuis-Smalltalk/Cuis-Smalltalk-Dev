'From Cuis7.9 [latest update: #8378] on 17 November 2026 at 3:06:35 pm'!

!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'jmv 17/Nov/2026, 6:05:21 pm (UTC)'!
addAndClassifySelector: selector withMethod: compiledMethod inCategory: category methodTimeStamp: aTimeStampString
	"Code was just saved and compiled. Regular interactive use in a browser, FileIn, etc."

	| priorMethodOrNil priorCategoryOrNil |
	priorMethodOrNil := self compiledMethodAt: selector ifAbsent: nil.
	priorCategoryOrNil := self whichCategoryIncludesSelector: selector.
	self addSelectorSilently: selector withMethod: compiledMethod.
	SystemChangeNotifier uniqueInstance doSilently: [self organization classify: selector under: category].
	priorMethodOrNil
		ifNil: [
			SystemChangeNotifier uniqueInstance
				methodAdded: compiledMethod
				selector: selector
				inProtocol: category
				class: self
				methodTimeStamp: aTimeStampString ]
		ifNotNil: [
			SystemChangeNotifier uniqueInstance
				methodChangedFrom: priorMethodOrNil
				to: compiledMethod
				selector: selector
				inClass: self.
			category = priorCategoryOrNil ifFalse: [
				SystemChangeNotifier uniqueInstance
					selectorRecategorized: selector
					from: priorCategoryOrNil
					to: category
					inClass: self ]].! !


!CompiledMethod methodsFor: 'accessing' stamp: 'jmv 17/Nov/2026, 6:00:55 pm (UTC)'!
historyPointer
	"The last four bytes of a CompiledMethod encode access to source code and method history.
	0 means no source code and no method history.
	If value is in [16rFC000000 .. 16rFDFFFFFF] source code is in .sources file.
		Substract 16rFC000000 and multiply * 32 for position in file
	If value is in [16rFE000000 .. 16rFFFFFFFF] source code is in .changes file.
		Substract 16rFE000000 and multiply * 32 for position in file
	If value is below that, it is an index in CodeHistory."

	^ self uint32At: self size - 3 bigEndian: false! !

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 17/Nov/2026, 5:59:50 pm (UTC)'!
getSourceFromFile
	"Read the source code from .sources or .changes file."

	| fileIndex position theFile answer |
	(fileIndex := self fileIndex) = 0 ifTrue: [^ nil].
	(position := self filePosition) = 0 ifTrue: [^ nil].
	theFile := SourceFiles at: self fileIndex.
	theFile position: position.
	answer := theFile nextChunk.
	^answer isEmpty ifTrue: [nil] ifFalse: [answer]! !


!CodeHistory methodsFor: 'add / update' stamp: 'jmv 16/Nov/2026, 6:16:02 pm (UTC)'!
addMethodVersion: newMethod libraryIndex: libIndex positionIndex: posIndex oldVersionOrNil: oldMethod
	"A new method (possibly a new version of oldMethod) was saved to .changes file and is about to be installed.
	Add it to CodeHistory.
	newMethod may be
	- entirely new (oldMethod is nil)
	- replacement for an existing method: oldMethod
	- re-addition of a previously existing and removed method (oldMethod is nil)
	Note: newMethod is still not installed, the old method, if existing is still installed."

	| newItem prevItem |
	
	"If re-added, take the entry for the removal. We don't have the old method, but its entry
	in CodeHistory was stored in removals. Retrieve it and remove it from removals."
	prevItem := oldMethod
		ifNotNil: [ oldMethod historyPointer ]
		ifNil: [ removals removeKey: newMethod asMethodReference ifAbsent: [0] ].
	
	"Add new entry to CodeHistory"
	newItem := self addLibraryIndex: libIndex positionIndex: posIndex prevItem: prevItem nextItem: 0.

	"If method already in CodeHistory, update its previous entry to point to new entry as nextItem"
	(prevItem > 0 and: [
	prevItem < 16rFC000000]) ifTrue: [
		self at: prevItem updateNextItem: newItem ].

	"Make method point to the entry just added"
	newMethod historyPointer: newItem.! !

