'From Cuis 5.0 [latest update: #4683] on 20 July 2021 at 1:50:30 pm'!

!MorphicCanvas class methodsFor: 'services' stamp: 'jmv 7/20/2021 12:20:34'!
isVectorGraphicsActive
	"
	MorphicCanvas isVectorGraphicsActive
	"
	^MorphicCanvas activeSubclass ~~ BitBltCanvas ! !

!MorphicCanvas class methodsFor: 'services' stamp: 'jmv 7/20/2021 12:28:17'!
isVectorGraphicsPluginActive
	"
	MorphicCanvas isVectorGraphicsPluginActive
	"
	^self isVectorGraphicsActive and: [
		(Smalltalk at: #VectorEngineWithPlugin) isPluginAvailable ]! !

!MorphicCanvas class methodsFor: 'services' stamp: 'jmv 7/20/2021 12:21:10'!
isVectorGraphicsUsedForAllRendering
	"
	MorphicCanvas isVectorGraphicsUsedForAllRendering
	"
	^MorphicCanvas activeSubclass name == #VectorCanvas! !


!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 7/20/2021 12:28:28'!
okayToRotateEasily
	"Answer whether it is appropriate for a rotation handle to be shown for the receiver."

	^ self hasOwnLocation and: [MorphicCanvas isVectorGraphicsPluginActive]! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 7/20/2021 12:28:33'!
okayToScaleEasily
	"Answer whether it is appropriate for a scale handle to be shown for the receiver."

	^ self hasOwnLocation and: [MorphicCanvas isVectorGraphicsPluginActive]! !

