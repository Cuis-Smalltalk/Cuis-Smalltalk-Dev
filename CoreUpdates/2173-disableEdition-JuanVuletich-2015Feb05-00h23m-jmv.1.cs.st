'From Cuis 4.2 of 25 July 2013 [latest update: #2172] on 5 February 2015 at 12:48:30.446864 am'!

!Editor commentStamp: '<historical>' prior: 0!
New text editors.
TextEditor provides most of the functionality that used to be in TextMorphEditor.
SmalltalkEditor is has Smalltalk code specific features.
SimpleEditor provides basic functionality for single line text editing. It does not handle fonts and styles, aligning and Smalltalk utilities. It handles one single line.!


!TextModelMorph methodsFor: 'accessing' stamp: 'jmv 2/5/2015 00:40'!
disableEdition
	self textMorph disableEdition! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 2/5/2015 00:39'!
disableEdition
	self setProperty: #disablesEdition toValue: true.
	self stopBlinking! !

!InnerTextMorph methodsFor: 'event handling testing' stamp: 'jmv 2/5/2015 00:45'!
disablesEdition

	^self hasProperty: #disablesEdition! !

!InnerTextMorph methodsFor: 'blinking cursor' stamp: 'jmv 2/5/2015 00:45'!
showsBlinkingCursor

	^self handlesKeyboard and: [ self disablesEdition not ]! !


!SimpleEditor methodsFor: 'accessing' stamp: 'jmv 2/5/2015 00:46'!
replaceSelectionWith: aString
	"Deselect, and replace the selection text by aText."

	| start stop |
	morph disablesEdition ifTrue: [
		^ self ].
	start _ self startIndex.
	stop _ self stopIndex.
	(start = stop and: [aString size = 0]) ifFalse: [
		string _ string copyReplaceFrom: start to: stop-1 with: aString.	
		self deselectAndPlaceCursorAt:  start + aString size.
		self userHasEdited  " -- note text now dirty" ]! !


!TextEditor methodsFor: 'accessing' stamp: 'jmv 2/5/2015 00:47'!
replaceSelectionWith: aTextOrString
	"Deselect, and replace the selection text by aText."
	"This is a user command, and generates undo"

	| start stop replacement |

	morph disablesEdition ifTrue: [
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

!TextEditor methodsFor: 'private' stamp: 'jmv 2/5/2015 00:46'!
applyAttribute: aTextAttribute
	"The user selected aTextAttribute via shortcut, menu or other means.
	If there is a selection, apply the attribute to the selection.
	In any case use the attribute for the user input (emphasisHere)"
	"This generates undo"
	| anythingDone |

	morph disablesEdition ifTrue: [
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

!TextEditor methodsFor: 'undo & redo' stamp: 'jmv 2/5/2015 00:46'!
redo

	morph disablesEdition ifTrue: [
		^ self ].

	model redoAndEvaluate: [ :modelUpdated :newCursorPos |
		newCursorPos
			ifNil: [ self recomputeSelection ]
			ifNotNil: [ self markIndex: newCursorPos pointIndex: newCursorPos ].
		modelUpdated ifTrue: [
			self userHasEdited.
			textComposition composeAll.			"this could be made more specific..."
			morph possiblyChanged ]]! !

!TextEditor methodsFor: 'undo & redo' stamp: 'jmv 2/5/2015 00:47'!
undo

	morph disablesEdition ifTrue: [
		^ self ].

	model undoAndEvaluate: [ :modelUpdated :newCursorPos |
		newCursorPos
			ifNil: [ self recomputeSelection ]
			ifNotNil: [ self markIndex: newCursorPos pointIndex: newCursorPos ].
		modelUpdated ifTrue: [
			self userHasEdited.
			textComposition composeAll.				"this could be made more specific..."
			morph possiblyChanged ]]! !


!InnerTextMorph methodsFor: 'event handling testing' stamp: 'jmv 11/9/2011 17:57'!
handlesKeyboard

	^self visible! !

!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 2/5/2015 00:33'!
keyboardFocusChange: aBoolean
	
	"The message is sent to a morph when its keyboard focus changes.
	The given argument indicates that the receiver is gaining (versus losing) the keyboard focus.
	In this case, all we need to do is to redraw border feedback"
	aBoolean
		ifTrue: [
			"A hand is wanting to send us characters..."
			editor ifNil: [ self editor storeSelectionInComposition ].	"Forces install"
			self showsBlinkingCursor ifTrue: [
				self startBlinking ]]
		ifFalse: [ self stopBlinking ].
	"Selection might be shown differently when focused"
	owner
		ifNotNil: [ owner redrawNeeded ]
		ifNil: [ self redrawNeeded ]		"Or at least redraw us"! !

!InnerTextMorph methodsFor: 'events-processing' stamp: 'jmv 2/5/2015 00:31'!
processKeystroke: aKeyboardEvent localPosition: localEventPosition
	"System level event handling."
		"localEventPosition?????"

	aKeyboardEvent wasHandled ifTrue:[^self].
	self handlesKeyboard ifFalse: [^ self].
	aKeyboardEvent wasHandled: true.
	self keyStroke: aKeyboardEvent! !

!InnerTextMorph methodsFor: 'blinking cursor' stamp: 'jmv 2/5/2015 00:33'!
pauseBlinking
	"Show a solid cursor (non blinking) for a short while"
	pauseBlinking _ true.
	textComposition ifNotNil: [
		self showsBlinkingCursor ifTrue: [
			"Show cursor right now if needed"
			textComposition showTextCursor ifFalse: [
				textComposition showTextCursor: true ]]]! !

!methodRemoval: InnerTextMorph #disableKeyboard!
InnerTextMorph removeSelector: #disableKeyboard!
!methodRemoval: TextModelMorph #disableKeyboard!
TextModelMorph removeSelector: #disableKeyboard!
!methodRemoval: TextModelMorph #disablesEdition!
TextModelMorph removeSelector: #disablesEdition!
