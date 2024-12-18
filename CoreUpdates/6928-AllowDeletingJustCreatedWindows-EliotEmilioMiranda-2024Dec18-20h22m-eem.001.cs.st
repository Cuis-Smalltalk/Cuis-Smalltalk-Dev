'From Cuis7.3 [latest update: #6922] on 18 December 2024 at 8:23:02 pm'!

!SystemWindow methodsFor: 'open/close' stamp: 'eem 12/18/2024 11:22:52'!
openInWorld: aWorld
	"This msg and its callees result in the window being activeOnlyOnTop"
	| frameRect |
	frameRect := self initialFrameIn: aWorld.
	self morphExtent: frameRect extent.
	aWorld addMorph: self position: frameRect topLeft.
	"Do it deferred. Was needed for text cursor to start blinking if (Preferences disable: #focusFollowsMouse).
	 For strange circumstances such as tests provoking errors, only activate windows that are still in world
	 by the time the action is actually ready to evaluate. "
	UISupervisor whenUIinSafeState:
		[ self isInWorld ifTrue:
			[ self activate ] ]! !

