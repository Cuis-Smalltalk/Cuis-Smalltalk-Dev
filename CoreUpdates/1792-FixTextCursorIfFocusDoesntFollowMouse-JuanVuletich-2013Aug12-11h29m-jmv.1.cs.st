'From Cuis 4.2 of 25 July 2013 [latest update: #1791] on 12 August 2013 at 11:30:06.514007 am'!

!SystemWindow methodsFor: 'open/close' stamp: 'jmv 8/12/2013 11:28'!
openInWorld: aWorld
	"This msg and its callees result in the window being activeOnlyOnTop"
	aWorld addMorph: self.
	self morphBoundsInWorld: (RealEstateAgent initialFrameFor: self world: aWorld).
	aWorld startSteppingSubmorphsOf: self.
	"Do it deferred. Was needed for text cursor to start blinking if (Preferences disable: #focusFollowsMouse) "
	WorldState addDeferredUIMessage: [ self activate ]! !

!SystemWindow methodsFor: 'open/close' stamp: 'jmv 8/12/2013 11:28'!
openInWorld: aWorld extent: extent
	"This msg and its callees result in the window being activeOnlyOnTop"
	aWorld addMorph: self.
	self morphPosition: (RealEstateAgent initialFrameFor: self world: aWorld) topLeft; morphExtent: extent.
	aWorld startSteppingSubmorphsOf: self.
	"Do it deferred. Was needed for text cursor to start blinking if (Preferences disable: #focusFollowsMouse) "
	WorldState addDeferredUIMessage: [ self activate ]! !

