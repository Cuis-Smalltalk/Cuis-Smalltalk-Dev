'From Cuis 5.0 [latest update: #4166] on 24 May 2020 at 9:40:32 am'!

!StringRequestMorph class methodsFor: 'new-morph participation' stamp: 'KenD 5/23/2020 19:20:43'!
initializedInstance
"
	StringRequestMorph initializedInstance.
"
	| stringRequestMorph |
	stringRequestMorph _ self 
					request: 'Enter answer here'
					 initialAnswer: 'What is the Answer?' 
					do: [:answer | PopUpMenu inform: answer ].
	^stringRequestMorph! !

