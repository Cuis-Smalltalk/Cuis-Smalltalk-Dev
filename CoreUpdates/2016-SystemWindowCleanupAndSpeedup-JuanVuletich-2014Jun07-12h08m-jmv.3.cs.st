'From Cuis 4.2 of 25 July 2013 [latest update: #2017] on 7 June 2014 at 1:04:07.474454 pm'!

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 6/7/2014 12:26'!
addMorph: aMorph position: aPoint
	^self privateAddMorph: aMorph atIndex: 1 position: aPoint! !


!PreDebugWindow methodsFor: 'GUI building' stamp: 'jmv 6/7/2014 12:31'!
initialExtent
	^ 560 @ 300! !


!SystemWindow methodsFor: 'menu' stamp: 'jmv 6/7/2014 12:34'!
fullScreen
	"Zoom Window to Full World size with possible DeskMargins"

	"SystemWindow fullScreen"

	| left right possibleBounds |
	(self hasProperty: #originalBounds)
		ifFalse: [ "Expand"
			self setProperty: #originalBounds toValue: self morphBoundsInWorld.
			left := right := 0.
			possibleBounds := (RealEstateAgent maximumUsableAreaInWorld: self world) 
				insetBy: (left @ 0 corner: right @ 0).
			(Preferences fullScreenLeavesDeskMargins)
				ifTrue: [ possibleBounds := possibleBounds insetBy: 22 ].
		] 
		ifTrue: [ "Contract"
			possibleBounds := self valueOfProperty: #originalBounds.
			self removeProperty: #originalBounds.
		].
	self morphPosition: possibleBounds topLeft extent: possibleBounds extent! !

!SystemWindow methodsFor: 'open/close' stamp: 'jmv 6/7/2014 12:27'!
openInWorld: aWorld
	"This msg and its callees result in the window being activeOnlyOnTop"
	| frameRect |
	frameRect _ RealEstateAgent initialFrameFor: self world: aWorld.
	self morphExtent: frameRect extent.
	aWorld addMorph: self position: frameRect topLeft.
	aWorld startSteppingSubmorphsOf: self.
	"Do it deferred. Was needed for text cursor to start blinking if (Preferences disable: #focusFollowsMouse) "
	WorldState addDeferredUIMessage: [ self activate ]! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 6/7/2014 12:37'!
windowBottom: aNumber
	"aNumber is an Y coordinate in the owner's coordinate system"

	self morphHeight: aNumber - self morphPosition y! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 6/7/2014 12:49'!
windowBottomLeft: aPoint
	"aPoint is an X@Y coordinate pair in the owner's coordinate system"

	| e newP p |
	p _ self morphPosition.
	newP _ aPoint x @ p y.
	e _ extent x + p x - aPoint x @ (aPoint y - self morphPosition y).
	self morphPosition: newP extent: e! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 6/7/2014 12:44'!
windowBottomRight: aPoint
	"aPoint is an X@Y coordinate pair in the owner's coordinate system"

	self morphExtent: aPoint - self morphPosition! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 6/7/2014 12:42'!
windowLeft: aNumber
	"aNumber is an X coordinate in the owner's coordinate system"

	| e newP p |
	p _ self morphPosition.
	newP _ aNumber @ p y.
	e _ extent x + p x - aNumber @ extent y.
	self morphPosition: newP extent: e! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 6/7/2014 12:42'!
windowRight: aNumber
	"aNumber is an X coordinate in the owner's coordinate system"

	self morphWidth: aNumber - self morphPosition x! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 6/7/2014 12:43'!
windowTop: aNumber
	"aNumber is an X coordinate in the owner's coordinate system"

	| e newP p |
	p _ self morphPosition.
	newP _ p x @ aNumber.
	e _ extent x @ (extent y + p y - aNumber).
	self morphPosition: newP extent: e! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 6/7/2014 12:46'!
windowTopLeft: newPosition
	"aPoint is an X@Y coordinate pair in the owner's coordinate system"

	| e |
	e _ extent + self morphPosition - newPosition.
	self morphPosition: newPosition extent: e! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 6/7/2014 12:50'!
windowTopRight: aPoint
	"aPoint is an X@Y coordinate pair in the owner's coordinate system"

	| e newP p |
	p _ self morphPosition.
	newP _ p x @ aPoint y.
	e _ (aPoint x - self morphPosition x) @ (extent y + p y - aPoint y).
	self morphPosition: newP extent: e! !


!PreDebugWindow class methodsFor: 'instance creation' stamp: 'jmv 6/7/2014 12:32'!
open: model label: aString message: messageString
	|  window |
	window _ self new.
	window
		model: model;
		buildMorphicWindowMessage: messageString.
	aString ifNotNil: [ window setLabel: aString ].
	 window openInWorld: self runningWorld! !

!methodRemoval: SystemWindow #openInWorld:extent:!
SystemWindow removeSelector: #openInWorld:extent:!
