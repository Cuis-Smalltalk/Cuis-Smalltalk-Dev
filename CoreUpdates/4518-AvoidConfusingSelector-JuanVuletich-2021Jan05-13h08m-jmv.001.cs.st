'From Cuis 5.0 [latest update: #4517] on 5 January 2021 at 1:15:41 pm'!

!TextEditor methodsFor: 'typing support' stamp: 'jmv 1/5/2021 13:14:39'!
processKeystrokeEvent: aKeyboardEvent
	"Key struck on the keyboard. Find out which one and, if special, carry 
	out the associated special action. Otherwise, add the character to the 
	stream of characters."

	(self dispatchOn: aKeyboardEvent) ifTrue: [
		self storeSelectionInComposition.
		^self].

	markBlock _ pointBlock.
	self storeSelectionInComposition! !


!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 1/5/2021 13:14:54'!
processKeystrokeEvent: evt
	| action |

	(acceptOnCR and: [evt isReturnKey]) ifTrue: [^ self acceptContents].

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
	self scrollSelectionIntoView! !


!InnerTextMorph methodsFor: 'events' stamp: 'jmv 1/5/2021 13:14:49'!
keyStroke: aKeyboardEvent

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].

	autoCompleter 
		ifNil: [ self processKeystrokeEvent: aKeyboardEvent ]
		ifNotNil: [
			autoCompleter
				autoCompletionAround: [ self processKeystrokeEvent: aKeyboardEvent ]
				keyStroke: aKeyboardEvent ].
			
	super keyStroke: aKeyboardEvent! !

!methodRemoval: InnerTextMorph #processKeyStroke: stamp: 'jmv 1/5/2021 13:15:04'!
InnerTextMorph removeSelector: #processKeyStroke:!
!methodRemoval: TextEditor #processKeyStroke: stamp: 'jmv 1/5/2021 13:15:07'!
TextEditor removeSelector: #processKeyStroke:!
