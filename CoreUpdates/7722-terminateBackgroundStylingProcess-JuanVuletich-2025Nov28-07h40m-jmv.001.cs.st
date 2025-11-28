'From Cuis7.5 [latest update: #7720] on 28 November 2025 at 7:42:03 am'!

!EntryFieldMorph methodsFor: 'unaccepted edits' stamp: 'jmv 27/Nov/2025 17:46:02'!
aboutToEdit
	"NOP in this morph."! !


!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 27/Nov/2025 17:45:19'!
aboutToEdit

	styler ifNotNil: [ styler terminateBackgroundStylingProcess ]! !


!SimpleEditor methodsFor: 'accessing' stamp: 'jmv 27/Nov/2025 17:47:16'!
replaceSelectionWith: aString
	"Deselect, and replace the selection text by aText."

	| start stop |
	morph disablesEditing ifTrue: [
		^ self ].

	morph aboutToEdit.
	start := self startIndex.
	stop := self stopIndex.
	(start = stop and: [aString size = 0]) ifFalse: [
		string := string copyReplaceFrom: start to: stop-1 with: aString.	
		self deselectAndPlaceCursorAt:  start + aString size.
		self userHasEdited  " -- note text now dirty" ]! !


!TextEditor methodsFor: 'accessing' stamp: 'jmv 27/Nov/2025 17:47:07'!
replaceSelectionWith: aTextOrString
	"Deselect, and replace the selection text by aText."
	"This is a user command, and generates undo"

	| start stop replacement |

	morph disablesEditing ifTrue: [
		^ self ].

	morph aboutToEdit.
	start := self startIndex.
	stop := self stopIndex.
	(aTextOrString isEmpty and: [stop > start]) ifTrue: [
		"If deleting, then set emphasisHere from 1st character of the deletion"
		emphasisHere := (model actualContents attributesAt: start) select: [:att |
			att mayBeExtended]].

	(start = stop and: [ aTextOrString isEmpty ]) ifFalse: [
		replacement := self addAttributesForPasting: aTextOrString.
		model logUndoAndReplaceFrom: start to: stop - 1 with: replacement.
		textComposition
			recomposeFrom: start
			to:  start + replacement size - 1
			delta: replacement size - (stop-start).
		self deselectAndPlaceCursorAt: start + replacement size.
		self userHasEdited.  " -- note text now dirty"
		morph updateFromTextComposition ].

	morph possiblyChanged! !

