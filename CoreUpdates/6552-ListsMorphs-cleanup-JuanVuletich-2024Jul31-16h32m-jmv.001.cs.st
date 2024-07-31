'From Cuis7.1 [latest update: #6551] on 31 July 2024 at 4:36:27 pm'!
!classDefinition: #HierarchicalListMorph category: #'Morphic-Widgets'!
PluggableScrollPane subclass: #HierarchicalListMorph
	instanceVariableNames: 'selectedMorph getListSelector keystrokeActionSelector autoDeselect autoExpand sortingSelector getSelectionSelector setSelectionSelector menuGetter mainView highlightedMorph doubleClickSelector '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!classDefinition: #PluggableListMorph category: #'Morphic-Widgets'!
PluggableScrollPane subclass: #PluggableListMorph
	instanceVariableNames: 'list getListSelector getIndexSelector setIndexSelector keystrokeActionSelector autoDeselect lastKeystrokeTime lastKeystrokes doubleClickSelector menuGetter mainView limitLayoutHeight '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 7/31/2024 16:13:53'!
keyStroke: aKeyboardEvent 
	"Process potential command keys"

	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ ^self ].

	(self arrowKey: aKeyboardEvent) ifNotNil: [
		^ aKeyboardEvent wasHandled: true ].

	aKeyboardEvent isEsc ifTrue: [ " escape key" ^ self mouseButton2Activity ].! !


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 7/31/2024 16:12:06'!
keyStroke: aKeyboardEvent 
	"Process keys"

	| aCharacter |
	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ ^self ].
	
	(self arrowKey: aKeyboardEvent) ifNotNil: [
		^ aKeyboardEvent wasHandled: true ].

	"All the cases below consume the event."
	aKeyboardEvent wasHandled: true.

	aKeyboardEvent isEsc ifTrue: [ " escape key" ^ self mouseButton2Activity ].
	aKeyboardEvent isDelete ifTrue: [ "delete key" ^ self deleteAction ].
	aKeyboardEvent isBackspace ifTrue: [ "backspace key" ^ self deleteAction ].
	
	aCharacter := aKeyboardEvent keyCharacter.
	^ self keyboardSearch: aCharacter! !

!methodRemoval: PluggableListMorph #mainView:menuGetter:keystrokeAction: stamp: 'jmv 7/31/2024 16:32:53'!
PluggableListMorph removeSelector: #mainView:menuGetter:keystrokeAction:!
!methodRemoval: PluggableListMorph #doKeystrokeAction: stamp: 'jmv 7/31/2024 16:34:51'!
PluggableListMorph removeSelector: #doKeystrokeAction:!
!methodRemoval: PluggableListMorph #getMenu stamp: 'jmv 7/31/2024 16:35:12'!
PluggableListMorph removeSelector: #getMenu!
!methodRemoval: HierarchicalListMorph #mainView:menuGetter:keystrokeAction: stamp: 'jmv 7/31/2024 16:32:53'!
HierarchicalListMorph removeSelector: #mainView:menuGetter:keystrokeAction:!
!methodRemoval: HierarchicalListMorph #getMenu stamp: 'jmv 7/31/2024 16:34:01'!
HierarchicalListMorph removeSelector: #getMenu!
!classDefinition: #HierarchicalListMorph category: #'Morphic-Widgets'!
PluggableScrollPane subclass: #HierarchicalListMorph
	instanceVariableNames: 'selectedMorph getListSelector autoDeselect autoExpand sortingSelector getSelectionSelector setSelectionSelector highlightedMorph doubleClickSelector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!classDefinition: #PluggableListMorph category: #'Morphic-Widgets'!
PluggableScrollPane subclass: #PluggableListMorph
	instanceVariableNames: 'list getListSelector getIndexSelector setIndexSelector autoDeselect lastKeystrokeTime lastKeystrokes doubleClickSelector limitLayoutHeight'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
