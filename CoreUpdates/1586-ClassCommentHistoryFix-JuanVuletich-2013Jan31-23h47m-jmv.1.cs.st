'From Cuis 4.1 of 12 December 2012 [latest update: #1584] on 31 January 2013 at 11:48:02 pm'!

!ClassCommentVersionsBrowser methodsFor: 'basic function' stamp: 'jmv 1/31/2013 23:44'!
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

	sourceFilesCopy _ SourceFiles collect: [ :x | x ifNotNil: [x readOnlyCopy]].
	position _ oldCommentRemoteStr position.
	file _ sourceFilesCopy at: oldCommentRemoteStr sourceFileNumber.
	[ position notNil & file notNil]  whileTrue: [
		file position: (0 max: position-150).  " Skip back to before the preamble"
		[file position < (position-1)]  "then pick it up from the front"
			whileTrue: [
				preamble _ file nextChunk.
				file skipSeparators			"Skip any padding"
				].

		prevPos _ nil.
		stamp _ ''.
		(preamble findString: 'commentStamp:' startingAt: 1) > 0
			ifTrue: [
				tokens _ Smalltalk actualScannerClass new scanTokens: preamble.
				(tokens at: tokens size-3) = #commentStamp: ifTrue: [
					"New format gives change stamp and unified prior pointer"
					stamp _ tokens at: tokens size-2.
					prevPos _ tokens last.
					prevFileIndex _ sourceFilesCopy fileIndexFromSourcePointer: prevPos.
					prevPos _ sourceFilesCopy filePositionFromSourcePointer: prevPos]]
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


!CuisSourceFileArray methodsFor: 'sourcePointer conversion' stamp: 'jmv 1/31/2013 23:41'!
filePositionFromSourcePointer: anInteger
	"Return the position of the source chunk addressed by anInteger"

	| unscaledPosition |
	"No source pointer => no filePosition"
	anInteger = 0 ifTrue: [ ^0 ].
	unscaledPosition _ (anInteger - 16r1000000) bitAnd: 16r1FFFFFF.
	^unscaledPosition * pointerScale! !

