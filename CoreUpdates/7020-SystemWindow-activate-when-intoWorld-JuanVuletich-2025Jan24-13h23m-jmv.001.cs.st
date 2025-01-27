'From Cuis7.3 [latest update: #7010] on 24 January 2025 at 1:23:46 pm'!

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 1/24/2025 12:08:58'!
intoWorld: aWorld
	"The receiver has just appeared in a new world. Note:
		* aWorld can be nil (due to optimizations in other places)
		* owner is already set
		* owner's submorphs may not include receiver yet.
	Important: Keep this method fast - it is run whenever morphs are added."
	super intoWorld: aWorld.
	self activate.! !


!SystemWindow methodsFor: 'open/close' stamp: 'jmv 1/24/2025 12:09:59'!
openInWorld: aWorld
	"This msg and its callees result in the window being activeOnlyOnTop"
	| frameRect |
	frameRect := self initialFrameIn: aWorld.
	self morphExtent: frameRect extent.
	aWorld addMorph: self position: frameRect topLeft.! !

