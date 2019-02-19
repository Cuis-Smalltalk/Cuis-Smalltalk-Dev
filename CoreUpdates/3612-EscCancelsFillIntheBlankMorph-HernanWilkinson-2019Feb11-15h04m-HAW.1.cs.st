'From Cuis 5.0 of 7 November 2016 [latest update: #3598] on 14 February 2019 at 5:45:58 pm'!

!TextModelMorph methodsFor: 'initialization' stamp: 'HAW 2/14/2019 17:40:47'!
escAction: aBlock
	
	self textMorph escAction: aBlock! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'HAW 2/14/2019 17:37:45'!
escAction

	"Return the action to perform when user presses <Esc> key"

	^self valueOfProperty: #escAction! !

!InnerTextMorph methodsFor: 'accessing' stamp: 'HAW 2/14/2019 17:40:40'!
escAction: aBlock
	
	"Sets the action to perform when user presses <Esc> key"
	
	^self setProperty: #escAction toValue: aBlock ! !


!InnerTextMorph methodsFor: 'event handling' stamp: 'HAW 2/14/2019 17:45:05'!
processKeyStroke: evt
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
	
	self handleInteraction: [ editor processKeyStroke: evt ].

	"Is this really needed? It produces whole morph invalidation just by (for example)
	moving the cursor around... (jmv Aug 6, 2014)"
	"self updateFromTextComposition."

	self scrollSelectionIntoView! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'HAW 2/14/2019 17:42:22'!
createTextPaneAcceptOnCR: acceptBoolean
	"create the textPane"
	| result |

	self flag: #todo. "Integrate this method with the Theme system. --cbr"

	result _ TextModelMorph
				textProvider: self
				textGetter: #response
				textSetter: #response:
				selectionGetter: #selectionInterval.
	
	result 
		hasUnacceptedEdits: true;
		acceptOnCR: acceptBoolean;
		escAction: [ self cancelClicked ];
		morphExtent: `18@5` * self sizeUnit.
		
	self addMorph: result position: `1@2` * self sizeUnit.
	
	^ result! !

