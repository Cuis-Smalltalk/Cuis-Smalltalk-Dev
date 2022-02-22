'From Cuis 6.0 [latest update: #5071] on 22 February 2022 at 10:11:26 am'!

!InnerTextMorph methodsFor: 'accept/cancel' stamp: 'jmv 2/22/2022 10:00:47'!
acceptOnAny: aBoolean
	aBoolean
		ifTrue: [ acceptOn _ #any ]
		ifFalse: [
			acceptOn = #any
				ifTrue: [ acceptOn _ #none ]].! !


!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 2/22/2022 10:07:31'!
acceptOnCR
	"Answer whether the receiver wants to accept when the Return key is hit"

	^ acceptOn == #enter! !

!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 2/22/2022 10:09:51'!
processKeystrokeEvent: evt
	| action |

	(acceptOn = #enter and: [evt isReturnKey]) ifTrue: [^ self acceptContents].

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
		owner scrollSelectionIntoView ].

	acceptOn = #any ifTrue: [self acceptContents].! !

!InnerTextMorph methodsFor: 'initialization' stamp: 'jmv 2/22/2022 10:00:57'!
initialize
	super initialize.
	wrapFlag _ true.
	acceptOn _ #none.
	hasUnacceptedEdits _ false.
	hasEditingConflicts _ false.
	askBeforeDiscardingEdits _ true.
	needsFit _ false.! !

!InnerTextMorph methodsFor: 'accept/cancel' stamp: 'jmv 2/22/2022 10:08:16'!
acceptOnCR: aBoolean
	aBoolean
		ifTrue: [ acceptOn _ #enter ]
		ifFalse: [
			acceptOn = #enter
				ifTrue: [ acceptOn _ #none ]].! !

