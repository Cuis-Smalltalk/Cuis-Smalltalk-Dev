'From Cuis7.1 [latest update: #6792] on 24 October 2024 at 11:13:31 am'!

!Theme methodsFor: 'colors' stamp: 'jmv 10/24/2024 11:03:23'!
defaultButtonColor

	^ self defaultWindowColor darker! !


!TaskbarMorph methodsFor: 'initialization' stamp: 'jmv 10/24/2024 10:44:59'!
defaultColor
	^ `Color lightGray`! !


!PluggableButtonMorph methodsFor: 'initialization' stamp: 'jmv 10/24/2024 10:56:43'!
defaultColor
	^Theme current defaultButtonColor! !


!Theme methodsFor: 'tool colors' stamp: 'jmv 10/24/2024 10:59:37'!
defaultWindowColor
	^ `Color veryVeryLightGray`! !


!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 10/24/2024 10:45:43'!
defaultColor
	^ Theme current defaultWindowColor! !

