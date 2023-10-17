'From Cuis 6.0 [latest update: #6047] on 17 October 2023 at 5:21:15 pm'!

!VersionsBrowserWindow class methodsFor: 'services' stamp: 'jmv 10/17/2023 17:12:11'!
browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector extraChangeRecord: aChangeRecord
	^ self browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector lostMethodPointer: nil extraChangeRecord: aChangeRecord! !

!VersionsBrowserWindow class methodsFor: 'services' stamp: 'jmv 10/17/2023 17:13:33'!
browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector lostMethodPointer: sourcePointer extraChangeRecord: aChangeRecord
	| changeList browser |
	changeList := (browser := VersionsBrowser new)
		scanVersionsOf: method class: class meta: meta
		category: msgCategory selector: selector.
	changeList ifNil: [ self inform: 'No versions available'. ^nil ].

	aChangeRecord ifNotNil: [
		changeList addedChangeRecord: aChangeRecord ].
	VersionsBrowserWindow open: changeList label: 'Recent versions of ', selector.

	^browser! !


!VersionsBrowser methodsFor: 'init & update' stamp: 'jmv 10/17/2023 17:07:27'!
scanVersionsOf: method class: class meta: meta category: category selector: selector
	| position stamp prevPos prevFileIndex preamble tokens sourceFilesCopy |
	selectorOfMethod := selector.
	currentCompiledMethod := method.
	classOfMethod := meta ifTrue: [class class] ifFalse: [class].
	changeList := OrderedCollection new.
	list := OrderedCollection new.
	addedChangeRecord ifNotNil: [ :change |
		self addItem: change text: ('{1} (in {2})' format: { change stamp. change fileName }) ].
	listIndex := 0.
	position := method filePosition.
	sourceFilesCopy := SourceFiles collect:
		[:x | x ifNotNil: [ x name asFileEntry readStream ]].
	method fileIndex = 0 ifTrue: [^ nil].
	file := sourceFilesCopy at: method fileIndex.
	[position notNil & file notNil] whileTrue: [
		"Skip back filler"
		file position: position.
		file backChunk.
		"Get preamble"
		preamble := file backChunk.
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
				(CompiledMethod priorReferenceFrom: tokens) ifNotNil: [ :priorMethodRef |
					prevFileIndex := sourceFilesCopy fileIndexFromSourcePointer: priorMethodRef.
					prevPos := sourceFilesCopy filePositionFromSourcePointer: priorMethodRef ]].
 		self addItem:
				(ChangeRecord new file: file position: position type: #method
						class: class name category: category meta: meta stamp: stamp)
			text: stamp , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector.
		position := prevPos.
		prevPos notNil ifTrue: [
			file := sourceFilesCopy at: prevFileIndex]].
	sourceFilesCopy do: [:x | x notNil ifTrue: [x close]].
	self clearSelections! !


!ChangeListWindow methodsFor: 'menu commands' stamp: 'jmv 10/17/2023 17:12:38'!
browseVersions

	| method class browser firstChange |
	method := model currentCompiledMethod.
	method isNil ifTrue: [
		^ VersionsBrowserWindow
			browseCommentOf: model selectedClass ].
	firstChange := model xtraVersion.
	firstChange ifNil: [ ^self ].
	class := method methodClass.
	browser := VersionsBrowserWindow
		browseVersionsOf: method
		class: class
		meta: class isMeta
		category: (class organization categoryOfElement: method selector)
		selector: method selector
		extraChangeRecord: firstChange.
	^browser! !


!VersionsBrowserWindow class methodsFor: 'services' stamp: 'jmv 10/17/2023 17:11:58'!
browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector lostMethodPointer: sourcePointer 
	^ self browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector lostMethodPointer: nil extraChangeRecord: nil! !

!methodRemoval: VersionsBrowser #addedChangeRecord stamp: 'jmv 10/17/2023 17:18:46'!
VersionsBrowser removeSelector: #addedChangeRecord!
