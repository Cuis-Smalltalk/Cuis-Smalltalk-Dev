'From Cuis 5.0 of 7 November 2016 [latest update: #3020] on 2 January 2017 at 2:27:29 pm'!

!Preferences class methodsFor: 'personalization' stamp: 'jmv 1/2/2017 14:18:06'!
usePreDebugWindow
	^ self
		valueOfFlag: #usePreDebugWindow
		ifAbsent: [ false ].! !


!SystemWindow methodsFor: 'open/close' stamp: 'jmv 1/2/2017 14:11:03'!
initialFrameIn: aWorld
	^RealEstateAgent initialFrameFor: self world: aWorld! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 1/2/2017 14:13:23'!
initialFrameIn: aWorld
	| e |
	e _ self runningWorld morphExtent.
	^(0@0 corner: e) insetBy: e // 10! !


!SystemWindow methodsFor: 'open/close' stamp: 'jmv 1/2/2017 14:11:12'!
openInWorld: aWorld
	"This msg and its callees result in the window being activeOnlyOnTop"
	| frameRect |
	frameRect _ self initialFrameIn: aWorld.
	self morphExtent: frameRect extent.
	aWorld addMorph: self position: frameRect topLeft.
	"Do it deferred. Was needed for text cursor to start blinking if (Preferences disable: #focusFollowsMouse) "
	WorldState addDeferredUIMessage: [ self activate ]! !


!PreDebugWindow class methodsFor: 'instance creation' stamp: 'jmv 1/2/2017 14:19:05'!
open: model label: aString message: messageString
	|  window |
	Preferences usePreDebugWindow
		ifTrue: [
			window _ self new.
			window
				model: model;
				buildMorphicWindowMessage: messageString print.
			aString ifNotNil: [ window setLabel: aString ].
			 window openInWorld ]
		ifFalse: [
			model openFullMorphicLabel: aString ]! !

