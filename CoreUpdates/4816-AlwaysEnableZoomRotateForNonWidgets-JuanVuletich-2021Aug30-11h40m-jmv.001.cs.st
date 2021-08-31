'From Cuis 5.0 [latest update: #4815] on 30 August 2021 at 11:41:50 am'!

!WidgetMorph methodsFor: 'halos and balloon help' stamp: 'jmv 8/30/2021 11:41:09'!
okayToResizeEasily
	"Answer whether it is appropriate to have the receiver be easily resized by the user from the halo"

	^ true.! !

!WidgetMorph methodsFor: 'halos and balloon help' stamp: 'jmv 8/30/2021 11:40:56'!
okayToRotateEasily
	"Answer whether it is appropriate for a rotation handle to be shown for the receiver."

	^ self world canvas usesVectorEnginePlugin.! !

!WidgetMorph methodsFor: 'halos and balloon help' stamp: 'jmv 8/30/2021 11:41:05'!
okayToScaleEasily
	"Answer whether it is appropriate for a scale handle to be shown for the receiver."

	^ self world canvas usesVectorEnginePlugin.! !


!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 8/30/2021 11:41:24'!
okayToRotateEasily
	"Answer whether it is appropriate for a rotation handle to be shown for the receiver."

	^ self hasOwnLocation! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 8/30/2021 11:41:30'!
okayToScaleEasily
	"Answer whether it is appropriate for a scale handle to be shown for the receiver."

	^ self hasOwnLocation! !

