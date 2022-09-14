'From Haver 6.0 [latest update: #5477] on 14 September 2022 at 11:51:37 am'!

!TextModelMorph methodsFor: 'accept/cancel' stamp: 'KLG 9/14/2022 11:44:26'!
acceptOnAny: aBoolean 
	"Accept on any keystroke."
	
	self scroller acceptOnAny: aBoolean ! !


!InnerTextMorph methodsFor: 'event handling' stamp: 'KLG 9/14/2022 11:49:11'!
processKeystrokeEvent: evt
	| action |

	(acceptOn = #enter and: [evt isReturnKey]) ifTrue: [^ self acceptContents].

	self pauseBlinking.
	
	"Return - check for special action"
	evt isReturnKey ifTrue: [	
		action _ self crAction.
		action ifNotNil: [ ^action value ] ].
	
	"Esc - check for special action"
	evt isEsc ifTrue: [			
		action _ self escAction.
		action ifNotNil: [ ^action value ] ].
	
	self handleInteraction: [ editor processKeystrokeEvent: evt ].	
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].

	"Only accept (on any) if we have unaccepted edits, otherwise the morph will flash."
	hasUnacceptedEdits and: [ acceptOn == #any ] :: ifTrue: [ self acceptContents ].! !

