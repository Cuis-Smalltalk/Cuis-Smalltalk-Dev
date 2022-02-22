'From Cuis 6.0 [latest update: #5071] on 22 February 2022 at 10:02:30 am'!
!classDefinition: #InnerTextMorph category: 'Morphic-Widgets'!
InnerPluggableMorph subclass: #InnerTextMorph
	instanceVariableNames: 'model wrapFlag textComposition editor pauseBlinking acceptOnCR hasUnacceptedEdits hasEditingConflicts askBeforeDiscardingEdits styler autoCompleter mutex needsFit acceptOn '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 2/22/2022 10:01:59'!
acceptOnCR
	"Answer whether the receiver wants to accept when the Return key is hit"

	^ acceptOn == true! !

!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 2/22/2022 10:01:59'!
processKeystrokeEvent: evt
	| action |

	(acceptOn and: [evt isReturnKey]) ifTrue: [^ self acceptContents].

	self pauseBlinking.
	
	"Return - check for special action"
	evt isReturnKey ifTrue: [	
		action _ self crAction.
		action ifNotNil: [ ^action value]].
	
	"Esc - check for special action"
	evt isEsc ifTrue: [			
		action _ self escAction.
		action ifNotNil: [ ^action value]].
	
	self handleInteraction: [ editor processKeystrokeEvent: evt ].	
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].! !

!InnerTextMorph methodsFor: 'initialization' stamp: 'jmv 2/22/2022 10:01:59'!
initialize
	super initialize.
	wrapFlag _ true.
	acceptOn _ false.
	hasUnacceptedEdits _ false.
	hasEditingConflicts _ false.
	askBeforeDiscardingEdits _ true.
	needsFit _ false.! !

!InnerTextMorph methodsFor: 'accept/cancel' stamp: 'jmv 2/22/2022 10:01:59'!
acceptOnCR: trueOrFalse
	acceptOn _ trueOrFalse! !

!classDefinition: #InnerTextMorph category: 'Morphic-Widgets'!
InnerPluggableMorph subclass: #InnerTextMorph
	instanceVariableNames: 'model wrapFlag textComposition editor pauseBlinking acceptOn hasUnacceptedEdits hasEditingConflicts askBeforeDiscardingEdits styler autoCompleter mutex needsFit'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!InnerTextMorph reorganize!
('accessing' askBeforeDiscardingEdits: contents: contentsAsIs: crAction crAction: disableEditing editor enableEditing escAction escAction: hasEditingConflicts hasEditingConflicts: isWrapped model: model:wrappedTo: positionInTextOf: textColor textColor: wrapFlag:)
('anchors' anchorMorph:at:)
('caching' releaseCachedState)
('drawing' debugDrawLineRectsOn: drawOn:)
('editing' acceptContents acceptOnCR confirmAcceptAnyway enterClickableRegion:localPosition: handleInteraction: hasUnacceptedEdits:)
('events' clickAndHalf:localPosition: doubleClickAndHalf:localPosition: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('event handling testing' disablesEditing handlesKeyboard handlesMouseDown:)
('event handling' keyboardFocusChange: processKeystrokeEvent:)
('events-processing' processKeystroke: processMouseMove:localPosition:)
('geometry' adjustExtent extentChanged: minimumExtent privateExtent:)
('initialization' defaultColor initialize)
('menu' addCustomMenuItems:hand: getMenu wrapOnOff wrapString)
('submorphs-add/remove' addMorphFrontFromWorldPosition:)
('private' autoCompleterClass: extentForComposing fit installEditorAndTextComposition mouseButton2Activity mutex releaseEditorAndTextComposition removedMorph: resetTextComposition selectionChanged stylerClass: textComposition updateFromTextComposition)
('notifications' fontPreferenceChanged possiblyChanged someSubmorphPositionOrExtentChanged)
('testing' canDiscardEdits hasUnacceptedEdits)
('blinking cursor' onBlinkCursor pauseBlinking showsBlinkingCursor startBlinking stopBlinking)
('accept/cancel' acceptOnCR:)
('macpal' flash)
('miscellaneous' disregardUnacceptedEdits selectAll)
('shout' formatAndStyleIfNeeded stylerStyled)
('selection' scrollSelectionIntoView)
('classification' is:)
('copying' okayToDuplicate)
('hover help' showHoverHelpWith:)
!

