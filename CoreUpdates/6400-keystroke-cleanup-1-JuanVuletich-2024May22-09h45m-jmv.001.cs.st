'From Cuis7.1 [latest update: #6391] on 22 May 2024 at 9:54:22 am'!

!Morph methodsFor: 'events-processing' stamp: 'jmv 5/22/2024 09:49:46'!
processKeystroke: aKeyboardEvent
	"System level event handling."

	aKeyboardEvent wasHandled ifTrue: [^self].
	self handlesKeyboard ifFalse: [^self].
	aKeyboardEvent wasHandled: true.
	self keyStroke: aKeyboardEvent! !


!WorldMorph methodsFor: 'events' stamp: 'jmv 5/22/2024 09:52:17'!
keyStroke: aKeyboardEvent
	self flag: #definesKeyboardShortcuts.

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].

	"Handle a keystroke event."
	(aKeyboardEvent commandAltKeyPressed or: [ aKeyboardEvent controlKeyPressed ])
		ifTrue: [
			aKeyboardEvent keyCharacter = $b ifTrue: [ Browser open ].
			aKeyboardEvent keyCharacter = $f ifTrue: [ BrowserWindow findClass ].
			aKeyboardEvent keyCharacter = $F ifTrue: [ MethodSetWindow findInSourceCode ].
		].
	"aKeyboardEvent keyCharacter print."
! !


!InnerTextMorph methodsFor: 'events' stamp: 'jmv 5/22/2024 09:50:50'!
keyStroke: aKeyboardEvent

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].

	autoCompleter 
		ifNil: [ self processKeystrokeEvent: aKeyboardEvent ]
		ifNotNil: [
			autoCompleter
				autoCompletionAround: [ self processKeystrokeEvent: aKeyboardEvent ]
				keyStroke: aKeyboardEvent ].
! !


!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 5/22/2024 09:52:30'!
keyStroke: aKeyboardEvent

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].

	scroller ifNotNil: [
		scroller keyStroke: aKeyboardEvent ].
! !


!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 5/22/2024 09:51:48'!
keyStroke: aKeyboardEvent 
	"Process potential command keys"

	| args |
	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].

	(self arrowKey: aKeyboardEvent) ifNotNil: [ ^ self ].
	aKeyboardEvent isEsc ifTrue: [ " escape key" ^ self mouseButton2Activity ].
	keystrokeActionSelector ifNil: [^self].
	(args := keystrokeActionSelector numArgs) = 1 
		ifTrue: [^mainView perform: keystrokeActionSelector with: aKeyboardEvent keyCharacter].
	args = 2 
		ifTrue: [
			^mainView 
				perform: keystrokeActionSelector
				with: aKeyboardEvent keyCharacter
				with: self].
	^self error: 'The keystrokeActionSelector must be a 1- or 2-keyword symbol'! !


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 5/22/2024 09:53:20'!
keyStroke: aKeyboardEvent 
	"Process keys"
	| aCharacter |
	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	
	(self arrowKey: aKeyboardEvent) ifNotNil: [ ^ self ].
	
	aKeyboardEvent isEsc ifTrue: [ " escape key" ^ self mouseButton2Activity ].
	aKeyboardEvent isDelete ifTrue: [ "delete key" ^ self deleteAction ].
	aKeyboardEvent isBackspace ifTrue: [ "backspace key" ^ self deleteAction ].
	
	aCharacter := aKeyboardEvent keyCharacter.
	
	aKeyboardEvent anyModifierKeyPressed
		ifTrue: [
			(self doKeystrokeAction: aCharacter)
				ifTrue: [ ^self ]].
	^ self keyboardSearch: aCharacter! !


!TextModelMorph methodsFor: 'events' stamp: 'jmv 5/22/2024 09:52:47'!
keyStroke: aKeyboardEvent
	"A keystroke was hit while the receiver had keyboard focus.  Pass the keywtroke on to my textMorph, and and also, if I have an event handler, pass it on to that handler"

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].

	scroller keyStroke: aKeyboardEvent.
! !

!methodRemoval: InnerTextMorph #processKeystroke: stamp: 'jmv 5/22/2024 09:49:18'!
InnerTextMorph removeSelector: #processKeystroke:!
