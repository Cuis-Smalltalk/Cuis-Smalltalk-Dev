'From Cuis7.1 [latest update: #6393] on 22 May 2024 at 10:26:23 am'!

!Morph methodsFor: 'events' stamp: 'jmv 5/22/2024 10:24:40'!
keyStroke: aKeyboardEvent
	"Handle a keystroke event.
	Subclasses should call super, and check if the event was handled."

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ aKeyboardEvent wasHandled: true ].
! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 5/22/2024 10:07:02'!
processKeystroke: aKeyboardEvent
	"System level event handling."

	aKeyboardEvent wasHandled ifTrue: [^self].
	self handlesKeyboard ifFalse: [^self].
	self keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled: true.! !


!WorldMorph methodsFor: 'events' stamp: 'jmv 5/22/2024 10:10:46'!
keyStroke: aKeyboardEvent

	self flag: #definesKeyboardShortcuts.
	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ ^self ].

	"Handle a keystroke event."
	(aKeyboardEvent commandAltKeyPressed or: [ aKeyboardEvent controlKeyPressed ])
		ifTrue: [
			aKeyboardEvent keyCharacter = $b ifTrue: [
				Browser open.
				aKeyboardEvent wasHandled: true. ].
			aKeyboardEvent keyCharacter = $f ifTrue: [
				BrowserWindow findClass.
				aKeyboardEvent wasHandled: true. ].
			aKeyboardEvent keyCharacter = $F ifTrue: [
				MethodSetWindow findInSourceCode.
				aKeyboardEvent wasHandled: true. ].
		].
	"aKeyboardEvent keyCharacter print."! !


!InnerTextMorph methodsFor: 'events' stamp: 'jmv 5/22/2024 10:04:53'!
keyStroke: aKeyboardEvent

	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ ^self ].

	autoCompleter 
		ifNil: [ self processKeystrokeEvent: aKeyboardEvent ]
		ifNotNil: [
			autoCompleter
				autoCompletionAround: [ self processKeystrokeEvent: aKeyboardEvent ]
				keyStroke: aKeyboardEvent ].

	aKeyboardEvent wasHandled: true.
! !


!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 5/22/2024 10:11:54'!
keyStroke: aKeyboardEvent

	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ ^self ].

	scroller ifNotNil: [
		scroller keyStroke: aKeyboardEvent ].! !


!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 5/22/2024 10:15:16'!
keyStroke: aKeyboardEvent 
	"Process potential command keys"

	| args |
	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ ^self ].

	(self arrowKey: aKeyboardEvent) ifNotNil: [
		^ aKeyboardEvent wasHandled: true ].

	aKeyboardEvent isEsc ifTrue: [ " escape key" ^ self mouseButton2Activity ].
	keystrokeActionSelector ifNil: [^self].
	(args := keystrokeActionSelector numArgs) = 1 
		ifTrue: [
			mainView perform: keystrokeActionSelector with: aKeyboardEvent keyCharacter.
			^ aKeyboardEvent wasHandled: true ].
	args = 2 
		ifTrue: [
			mainView 
				perform: keystrokeActionSelector
				with: aKeyboardEvent keyCharacter
				with: self.
			^ aKeyboardEvent wasHandled: true ].
	^self error: 'The keystrokeActionSelector must be a 1- or 2-keyword symbol'! !


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 5/22/2024 10:16:59'!
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
	aKeyboardEvent anyModifierKeyPressed
		ifTrue: [
			(self doKeystrokeAction: aCharacter)
				ifTrue: [ ^self ]].
	^ self keyboardSearch: aCharacter! !

!methodRemoval: TextModelMorph #keyStroke: stamp: 'jmv 5/22/2024 10:23:51'!
TextModelMorph removeSelector: #keyStroke:!
