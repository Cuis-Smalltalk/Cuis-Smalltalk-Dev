'From Cuis 6.0 [latest update: #5207] on 31 May 2022 at 2:38:12 pm'!


!VersionsBrowser methodsFor: 'init & update' stamp: 'jmv 5/31/2022 14:37:36'!
scanVersionsOf: method class: class meta: meta category: category selector: selector
	| position stamp prevPos prevFileIndex preamble tokens sourceFilesCopy |
	selectorOfMethod _ selector.
	currentCompiledMethod _ method.
	classOfMethod _ meta ifTrue: [class class] ifFalse: [class].
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	self addedChangeRecord ifNotNil: [ :change |
		self addItem: change text: ('{1} (in {2})' format: { change stamp. change fileName }) ].
	listIndex _ 0.
	position _ method filePosition.
	sourceFilesCopy _ SourceFiles collect:
		[:x | x ifNotNil: [ x name asFileEntry readStream ]].
	method fileIndex = 0 ifTrue: [^ nil].
	file _ sourceFilesCopy at: method fileIndex.
	[position notNil & file notNil] whileTrue: [
		"Skip back filler"
		file position: position.
		file backChunk.
		"Get preamble"
		preamble _ file backChunk.
		"Preamble is likely a linked method preamble, if we're in
			a changes file (not the sources file).  Try to parse it
			for prior source position and file index"
		prevFileIndex _ nil.
		prevPos _ nil.
		stamp _ ''.
		(preamble includesSubString: 'methodsFor:')
			ifTrue: [
				tokens _ Scanner new scanTokens: preamble.
				stamp _ CompiledMethod stampFrom: tokens.
				(CompiledMethod priorReferenceFrom: tokens) ifNotNil: [ :priorMethodRef |
					prevFileIndex _ sourceFilesCopy fileIndexFromSourcePointer: priorMethodRef.
					prevPos _ sourceFilesCopy filePositionFromSourcePointer: priorMethodRef ]].
 		self addItem:
				(ChangeRecord new file: file position: position type: #method
						class: class name category: category meta: meta stamp: stamp)
			text: stamp , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector.
		position _ prevPos.
		prevPos notNil ifTrue: [
			file _ sourceFilesCopy at: prevFileIndex]].
	sourceFilesCopy do: [:x | x notNil ifTrue: [x close]].
	self clearSelections! !


!ClassCommentVersionsBrowser methodsFor: 'basic function' stamp: 'jmv 5/31/2022 14:37:31'!
scanVersionsOf: class
	"Scan for all past versions of the class comment of the given class"

	| oldCommentRemoteStr sourceFilesCopy position prevPos stamp preamble tokens prevFileIndex |

	classOfMethod _ class.
	oldCommentRemoteStr _ class  organization commentRemoteStr.
	currentCompiledMethod _ oldCommentRemoteStr.
	selectorOfMethod _ #Comment.
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	listIndex _ 0.
	oldCommentRemoteStr ifNil:[^ nil] ifNotNil: [oldCommentRemoteStr sourcePointer].

	sourceFilesCopy _ SourceFiles collect: [ :x | x ifNotNil: [x name asFileEntry readStream]].
	position _ oldCommentRemoteStr position.
	file _ sourceFilesCopy at: oldCommentRemoteStr sourceFileNumber.
	[ position notNil & file notNil]  whileTrue: [
		"Skip back filler"
		file position: position.
		file backChunk.
		"Get preamble"
		preamble _ file backChunk.
		prevPos _ nil.
		stamp _ ''.
		(preamble includesSubString: 'commentStamp:')
			ifTrue: [
				tokens _ Scanner new scanTokens: preamble.
				stamp _ CompiledMethod field: #commentStamp: from: tokens ifAbsentOrNil: [ '' ].
				(CompiledMethod priorReferenceFrom: tokens) ifNotNil: [ :priorRef |
					prevFileIndex _ sourceFilesCopy fileIndexFromSourcePointer: priorRef.
					prevPos _ sourceFilesCopy filePositionFromSourcePointer: priorRef ]]
			ifFalse: [
				"The stamp get lost, maybe after a condenseChanges"
				stamp _ '<historical>'].
 		self addItem:
				(ChangeRecord new file: file position: position type: #classComment
						class: class name category: nil meta: class isMeta stamp: stamp)
			text: stamp , ' ' , class name , ' class comment'. 
		prevPos = 0 ifTrue: [ prevPos _ nil ].
		position _ prevPos.
		prevPos notNil ifTrue: [ file _ sourceFilesCopy at: prevFileIndex ]].
	sourceFilesCopy do: [ :x | x notNil ifTrue: [ x close ]].
	self clearSelections! !

