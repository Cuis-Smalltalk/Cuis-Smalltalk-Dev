'From Cuis 5.0 of 7 November 2016 [latest update: #3722] on 2 May 2019 at 9:37:49 am'!

!TextEditor methodsFor: 'accessing' stamp: 'jmv 5/2/2019 09:12:25'!
selectionRectangle
	"Answer a rectangle that encompasses single or multiple selection.
	If no selection, answer a rectangle that includes cursor."
	selectionStartBlocks notEmpty ifTrue: [
		^ selectionStartBlocks first quickMerge: selectionStopBlocks last].
	^ markBlock quickMerge: pointBlock! !


!TextEditor methodsFor: 'accessing-selection' stamp: 'jmv 5/2/2019 09:02:22'!
hasSelection
	^ markBlock ~= pointBlock or: [ selectionStartBlocks notEmpty ]! !

!TextEditor methodsFor: 'accessing-selection' stamp: 'jmv 5/2/2019 09:00:31'!
selectionAsStream
	"Answer a ReadStream on the text that is currently  selected.
	Note: Only considers single selection. See #selection to see how we handle multiple selection."

	^ReadWriteStream
		on: self privateCurrentString
		from: self startIndex
		to: self stopIndex - 1! !


!PluggableScrollPane methodsFor: 'scrolling' stamp: 'jmv 5/2/2019 09:33:42'!
scrollToShow: aRectangle
	"scroll to include as much of aRectangle as possible, where aRectangle is in the scroller's local space.
	This means that 0@0 is scrolling all the way top and all the way left"
	| deltaY |
	deltaY _ (aRectangle amountToTranslateWithin: (`0@0` extent: self viewableExtent)) y.
	deltaY ~= 0 ifTrue: [
		self scrollBy: 0@deltaY ]! !


!TextModelMorph methodsFor: 'editor access' stamp: 'jmv 5/2/2019 09:10:04'!
scrollSelectionIntoView
	"Scroll my text into view if necessary and return true, else return false"

	self scrollToShow: (self editor selectionRectangle translatedBy: self textMorph morphPosition)! !

