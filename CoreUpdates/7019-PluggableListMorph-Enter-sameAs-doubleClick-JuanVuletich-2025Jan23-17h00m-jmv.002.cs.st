'From Cuis7.3 [latest update: #7004] on 23 January 2025 at 5:03:00 pm'!

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 1/23/2025 16:01:36'!
keyStroke: aKeyboardEvent 
	"Process keys"

	| aCharacter |
	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ ^self ].
	
	(self arrowKey: aKeyboardEvent) ifNotNil: [
		^ aKeyboardEvent wasHandled: true ].

	"All the cases below consume the event."
	aKeyboardEvent wasHandled: true.
	aKeyboardEvent isReturnKey ifTrue: [
		^doubleClickSelector notNil ifTrue: [(self window ifNil: [model]) perform: doubleClickSelector]].
	aKeyboardEvent isEsc ifTrue: [ ^ self mouseButton2Activity ].
	aKeyboardEvent isDelete ifTrue: [ ^ self deleteAction ].
	aKeyboardEvent isBackspace ifTrue: [ ^ self deleteAction ].
	
	aCharacter := aKeyboardEvent keyCharacter.
	^ self keyboardSearch: aCharacter! !

