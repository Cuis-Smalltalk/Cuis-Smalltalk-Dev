'From Cuis 6.0 [latest update: #5116] on 29 April 2022 at 11:09:13 pm'!
!classDefinition: #TextEditor category: #'System-Text'!
Editor subclass: #TextEditor
	instanceVariableNames: 'model textComposition markBlock pointBlock emphasisHere lastParenLocation selectionStartBlocks selectionStopBlocks initialSelectionStart initialSelectionStop doWordSelection doParagraphSelection defaultFont '
	classVariableNames: 'ChangeText FindText '
	poolDictionaries: ''
	category: 'System-Text'!
!classDefinition: #TextComposition category: #'Morphic-Text Support'!
Object subclass: #TextComposition
	instanceVariableNames: 'model extentForComposing lines maxRightX selectionStartBlocks selectionStopBlocks editor showTextCursor lastTextCursorRect selectionStopBlock selectionStartBlock '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Text Support'!

!TextComposition methodsFor: 'selection' stamp: 'jmv 4/29/2022 23:08:43'!
selectionStartBlock: startBlock selectionStopBlock: stopBlockBlock
	selectionStartBlock _ startBlock.
	selectionStopBlock _ stopBlockBlock.! !


!TextEditor methodsFor: 'accessing' stamp: 'jmv 4/29/2022 21:27:07'!
replaceSelectionWith: aTextOrString
	"Deselect, and replace the selection text by aText."
	"This is a user command, and generates undo"

	| start stop replacement |

	morph disablesEditing ifTrue: [
		^ self ].

	start _ self startIndex.
	stop _ self stopIndex.
	(aTextOrString isEmpty and: [stop > start]) ifTrue: [
		"If deleting, then set emphasisHere from 1st character of the deletion"
		emphasisHere _ (model actualContents attributesAt: start) select: [:att |
			att mayBeExtended]].

	(start = stop and: [ aTextOrString isEmpty ]) ifFalse: [
		replacement _ self addAttributesForPasting: aTextOrString.
		model logUndoAndReplaceFrom: start to: stop - 1 with: replacement.
		textComposition
			recomposeFrom: start
			to:  start + replacement size - 1
			delta: replacement size - (stop-start).
		self deselectAndPlaceCursorAt: start + replacement size.
		self userHasEdited  " -- note text now dirty" ].

	morph possiblyChanged! !

!TextEditor methodsFor: 'accessing' stamp: 'jmv 4/29/2022 21:11:48'!
selectionRectangle
	"Answer a rectangle that encompasses selection.
	If no selection, answer a rectangle that includes cursor."
	^ markBlock quickMerge: pointBlock! !

!TextEditor methodsFor: 'accessing-selection' stamp: 'jmv 4/29/2022 21:19:42'!
hasSelection
	^ markBlock ~= pointBlock! !

!TextEditor methodsFor: 'accessing-selection' stamp: 'jmv 4/29/2022 21:11:09'!
selection
	"Answer the text that is currently selected."

	| t firstIndex lastIndex |
	t _ model actualContents.
	firstIndex _ self startIndex.
	lastIndex _ self stopIndex - 1.
	(firstIndex = 1 and: [ lastIndex = t size ])
		ifTrue: [ ^t copy ].
	^t copyFrom: firstIndex to: lastIndex! !

!TextEditor methodsFor: 'accessing-selection' stamp: 'jmv 4/29/2022 21:11:20'!
selectionAsStream
	"Answer a ReadStream on the text that is currently  selected."

	^ReadWriteStream
		on: self privateCurrentString
		from: self startIndex
		to: self stopIndex - 1! !

!TextEditor methodsFor: 'current selection' stamp: 'jmv 4/29/2022 21:24:19'!
recomputeSelection
	"The same characters are selected but their coordinates may have changed."

	self markIndex: self markIndex pointIndex: self pointIndex.! !

!TextEditor methodsFor: 'events' stamp: 'jmv 4/29/2022 21:09:43'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	| b |

	initialSelectionStart _ nil.
	initialSelectionStop _ nil.
	doWordSelection _ false.
	doParagraphSelection _ false.

	b _ textComposition characterBlockAtPoint: localEventPosition.
	(textComposition clickAt: localEventPosition) ifTrue: [
		markBlock _ b.
		pointBlock _ b.
		aMouseButtonEvent hand releaseKeyboardFocus: self.
		^ self ].
	
	aMouseButtonEvent shiftPressed
		ifFalse: [
			markBlock _ b.
			pointBlock _ b.	
			self setEmphasisHereFromText ]! !

!TextEditor methodsFor: 'menu messages' stamp: 'jmv 4/29/2022 21:27:48'!
cut
	"Cut out the current selection and redisplay if necessary."
	"This is a user command, and generates undo"

	| selection |
	self lineSelectAndEmptyCheck: [^ self].
	selection _ self selection.
	self replaceSelectionWith: self nullText.
	self clipboardTextPut: selection.! !

!TextEditor methodsFor: 'new selection' stamp: 'jmv 4/29/2022 21:20:22'!
lineSelectAndEmptyCheck: returnBlock
	"If the current selection is empty, expand it to be the entire current line; if after that's done the selection is still empty, then evaluate the returnBlock, which will typically consist of '[^ self]' in the caller -- check senders of this method to understand this."

	self selectLine.  "if current selection is empty, then first select the entire line in which occurs before proceeding"
	self hasSelection ifFalse: [morph flash.  ^ returnBlock value]! !

!TextEditor methodsFor: 'new selection' stamp: 'jmv 4/29/2022 21:09:06'!
messageSendsRanges: aRanges
	"aRanges must be notEmpty"
	| lastRange |

	lastRange := nil.
	aRanges do: [ :range |
		( lastRange isNil or: [ range first > lastRange first ]) ifTrue: [
			lastRange _ range ]].

	self selectFrom: lastRange first to: lastRange last! !

!TextEditor methodsFor: 'new selection' stamp: 'jmv 4/29/2022 21:20:50'!
selectLine
	"Make the receiver's selection, if currently empty, encompass the current line."
	self hasSelection ifTrue:[^self].
	self selectInterval: (model actualContents encompassLine: self selectionInterval)! !

!TextEditor methodsFor: 'private' stamp: 'jmv 4/29/2022 21:23:52'!
applyAttribute: aTextAttribute
	"The user selected aTextAttribute via shortcut, menu or other means.
	If there is a selection, apply the attribute to the selection.
	In any case use the attribute for the user input (emphasisHere)"
	"This generates undo"
	| anythingDone interval |

	morph disablesEditing ifTrue: [
		^ self ].

	anythingDone _ false.
	emphasisHere _ Text addAttribute: aTextAttribute toArray: emphasisHere.

	interval _ self selectionInterval.
	(interval notEmpty or: [ aTextAttribute isParagraphAttribute ])
		ifTrue: [
			anythingDone _ true.
			model logUndoAndAddAttribute: aTextAttribute from: interval first to: interval last.
			textComposition recomposeFrom: interval first to: interval last delta: 0 ].

	anythingDone ifTrue: [
		self recomputeSelection.	"Needed so visible selection is updated to reflect new visual extent of selection"
		self userHasEdited ].

	"Even if nothing done, emphasisHere might have changed"
	morph possiblyChanged.! !

!TextEditor methodsFor: 'private' stamp: 'jmv 4/29/2022 21:25:57'!
findAndReplaceMany: doMany
	"Subroutine of search: and again.  Use same FindText and ChangeText as before.  If many is true, do it repeatedly.  Created 1/26/96 sw by adding the many argument to #againOrSame."
	"jmvnote: We currently have no command for search&replace, but it is really easy to add, and this method supports it. See ChangeText"
	"This is a user command, and generates undo"

	|  indexStream |

	"Find and Change, recording start indices in the array"
	indexStream _ WriteStream on: (Array new: 20). "an array to store change locs"
	[(self findAndReplaceOnce: indexStream) & doMany] whileTrue. "<-- this does the work"
	"Answer whether something was found"
	^indexStream notEmpty! !

!TextEditor methodsFor: 'private' stamp: 'jmv 4/29/2022 21:08:01'!
findAndReplaceOnce: indexStream
	"Find the next occurrence of FindText.  If none, answer false.
	Append the start index of the occurrence to the stream indices, and, if
	ChangeText is not the same object as FindText, replace the occurrence by it.
	Note that the search is case-sensitive for replacements, otherwise not."
	"This is a user command, and generates undo"

	| where |
	where _ model actualContents
		findString: self class findText
		startingAt: self stopIndex
		caseSensitive: ((self class changeText ~~ self class findText) or: [Preferences caseSensitiveFinds]).
	where = 0 ifTrue: [^ false].

	self selectFrom: where to: where + self class findText size - 1.	"Repeat it here. Senders beware: only one of these should last"

	self class changeText ~~ self class findText ifTrue: [ self replaceSelectionWith: self class changeText ].
	indexStream nextPut: where.
	^ true! !

!TextEditor methodsFor: 'private' stamp: 'jmv 4/29/2022 23:00:53'!
storeSelectionInComposition
	"for proper display of selected text"

	pointBlock ifNil: [ ^self ].
	textComposition
		selectionStartBlock: self startBlock
		selectionStopBlock: self stopBlock.! !

!TextEditor methodsFor: 'initialization' stamp: 'jmv 4/29/2022 21:27:58'!
initialize
	initialSelectionStart _ nil.
	initialSelectionStop _ nil.
	doWordSelection _ false.
	doParagraphSelection _ false.
	defaultFont _ FontFamily defaultFamilyAndPointSize! !

!TextEditor methodsFor: 'initialization' stamp: 'jmv 4/29/2022 21:28:03'!
resetState
	"Establish the initial conditions for editing the paragraph: place text cursor 
	before first character and set the emphasis to that of the first character"

	markBlock _ textComposition defaultCharacterBlock.
	pointBlock _ markBlock.! !


!TextComposition methodsFor: 'display' stamp: 'jmv 4/29/2022 23:08:30'!
displaySelectionInLine: line on: aCanvas textTopLeft: textTopLeft  selectionColor: sc

	"textTopLeft is relative to the morph currently being drawn"
	selectionStartBlock ifNotNil: [
		self
			displaySelectionStartBlock: selectionStartBlock
			stopBlock: selectionStopBlock
			InLine: line
			on: aCanvas
			textTopLeft: textTopLeft
			selectionColor: sc ].! !

!TextComposition methodsFor: 'selection' stamp: 'jmv 4/29/2022 23:05:56'!
selectionRects
	"Return an array of rectangles representing the selection regions."
	^ Array streamContents: [ :strm |
		selectionStartBlock ifNotNil: [
			self addSelectionRectsFrom: selectionStartBlock to: selectionStopBlock to: strm ]].! !

!TextComposition methodsFor: 'initialization' stamp: 'jmv 4/29/2022 23:08:39'!
initialize
	showTextCursor _ false.
	selectionStartBlock _ nil.
	selectionStopBlock _ nil.! !

!methodRemoval: TextComposition #selectionStartBlocks:selectionStopBlocks: stamp: 'jmv 4/29/2022 23:07:59'!
TextComposition removeSelector: #selectionStartBlocks:selectionStopBlocks:!
!methodRemoval: SmalltalkEditor #selection stamp: 'jmv 4/29/2022 23:07:34'!
SmalltalkEditor removeSelector: #selection!
!methodRemoval: TextEditor #zapMultiSelection stamp: 'jmv 4/29/2022 23:07:34'!
TextEditor removeSelector: #zapMultiSelection!
!methodRemoval: TextEditor #selectInterval: stamp: 'jmv 4/29/2022 23:07:34'!
TextEditor removeSelector: #selectInterval:!
!methodRemoval: TextEditor #deselectAndPlaceCursorAt: stamp: 'jmv 4/29/2022 23:07:34'!
TextEditor removeSelector: #deselectAndPlaceCursorAt:!
!methodRemoval: TextEditor #selectAll stamp: 'jmv 4/29/2022 23:07:34'!
TextEditor removeSelector: #selectAll!
!methodRemoval: TextEditor #selectionIntervalsDo: stamp: 'jmv 4/29/2022 23:07:34'!
TextEditor removeSelector: #selectionIntervalsDo:!
!methodRemoval: Editor #selectionIntervalsDo: stamp: 'jmv 4/29/2022 23:07:34'!
Editor removeSelector: #selectionIntervalsDo:!
!classDefinition: #TextEditor category: #'System-Text'!
Editor subclass: #TextEditor
	instanceVariableNames: 'model textComposition markBlock pointBlock emphasisHere lastParenLocation initialSelectionStart initialSelectionStop doWordSelection doParagraphSelection defaultFont'
	classVariableNames: 'ChangeText FindText'
	poolDictionaries: ''
	category: 'System-Text'!
!classDefinition: #TextComposition category: #'Morphic-Text Support'!
Object subclass: #TextComposition
	instanceVariableNames: 'model extentForComposing lines maxRightX editor showTextCursor lastTextCursorRect selectionStartBlock selectionStopBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Text Support'!
