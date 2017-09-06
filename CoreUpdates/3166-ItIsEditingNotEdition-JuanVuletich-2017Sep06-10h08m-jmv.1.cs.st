'From Cuis 5.0 of 7 November 2016 [latest update: #3165] on 6 September 2017 at 10:08:50 am'!

!ReparseAfterSourceEditing commentStamp: 'jmv 9/6/2017 10:05:54' prior: 0!
A ReparseAfterSourceEditing is a Notification used to restart the syntax parsing phase of a compilation after a change in source code.!


!TextModelMorph methodsFor: 'accessing' stamp: 'jmv 9/6/2017 10:01:18'!
disableEditing
	self textMorph disableEditing! !

!TextModelMorph methodsFor: 'accessing' stamp: 'jmv 9/6/2017 10:03:26'!
enableEditing

	self textMorph enableEditing! !


!BrowserWindow methodsFor: 'updating' stamp: 'jmv 9/6/2017 10:04:36'!
disableCodePaneEditing

	codePane ifNotNil: [ codePane disableEditing ]! !

!BrowserWindow methodsFor: 'updating' stamp: 'jmv 9/6/2017 10:04:48'!
enableCodePaneEditing

	codePane ifNotNil: [ codePane enableEditing ]! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 9/6/2017 10:01:59'!
disableEditing
	self setProperty: #disablesEditing toValue: true.
	self stopBlinking! !

!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 9/6/2017 10:03:03'!
enableEditing
	
	self removeProperty: #disablesEditing! !

!InnerTextMorph methodsFor: 'event handling testing' stamp: 'jmv 9/6/2017 10:02:01'!
disablesEditing

	^self hasProperty: #disablesEditing! !


!Inspector methodsFor: 'contents' stamp: 'jmv 9/6/2017 10:05:31'!
acceptedStringOrText
	"We need our cache not to be modified by user changes"
	^acceptedContentsCache copy! !


!TextEditor methodsFor: 'accessing' stamp: 'jmv 9/6/2017 10:02:22'!
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
		selectionStartBlocks _ #().
		selectionStopBlocks _ #().
		self userHasEdited  " -- note text now dirty" ].

	morph possiblyChanged! !

!TextEditor methodsFor: 'private' stamp: 'jmv 9/6/2017 10:02:19'!
applyAttribute: aTextAttribute
	"The user selected aTextAttribute via shortcut, menu or other means.
	If there is a selection, apply the attribute to the selection.
	In any case use the attribute for the user input (emphasisHere)"
	"This generates undo"
	| anythingDone |

	morph disablesEditing ifTrue: [
		^ self ].

	anythingDone _ false.
	emphasisHere _ Text addAttribute: aTextAttribute toArray: emphasisHere.
	self selectionIntervalsDo: [ :interval |
		(interval notEmpty or: [ aTextAttribute isParagraphAttribute ])
			ifTrue: [
				anythingDone _ true.
				model logUndoAndAddAttribute: aTextAttribute from: interval first to: interval last.
				textComposition recomposeFrom: interval first to: interval last delta: 0 ]].
	anythingDone ifTrue: [
		self recomputeSelection.	"Needed so visible selection is updated to reflect new visual extent of selection"
		self userHasEdited ].

	"Even if nothing done, emphasisHere might have changed"
	morph possiblyChanged! !

!TextEditor methodsFor: 'undo & redo' stamp: 'jmv 9/6/2017 10:02:20'!
redo

	morph disablesEditing ifTrue: [
		^ self ].

	model redoAndEvaluate: [ :modelUpdated :newCursorPos |
		newCursorPos
			ifNil: [ self recomputeSelection ]
			ifNotNil: [ self markIndex: newCursorPos pointIndex: newCursorPos ].
		modelUpdated ifTrue: [
			self userHasEdited.
			textComposition composeAll.			"this could be made more specific..."
			morph possiblyChanged ]]! !

!TextEditor methodsFor: 'undo & redo' stamp: 'jmv 9/6/2017 10:02:24'!
undo

	morph disablesEditing ifTrue: [
		^ self ].

	model undoAndEvaluate: [ :modelUpdated :newCursorPos |
		newCursorPos
			ifNil: [ self recomputeSelection ]
			ifNotNil: [ self markIndex: newCursorPos pointIndex: newCursorPos ].
		modelUpdated ifTrue: [
			self userHasEdited.
			textComposition composeAll.				"this could be made more specific..."
			morph possiblyChanged ]]! !


!BrowserWindow methodsFor: 'updating' stamp: 'jmv 9/6/2017 10:04:55'!
editSelectionChanged
	
	self isEditSelectionNone
		ifTrue: [ self disableCodePaneEditing ]
		ifFalse: [ self enableCodePaneEditing]! !


!InnerTextMorph methodsFor: 'blinking cursor' stamp: 'jmv 9/6/2017 10:02:07'!
showsBlinkingCursor

	^self handlesKeyboard and: [ self disablesEditing not ]! !

