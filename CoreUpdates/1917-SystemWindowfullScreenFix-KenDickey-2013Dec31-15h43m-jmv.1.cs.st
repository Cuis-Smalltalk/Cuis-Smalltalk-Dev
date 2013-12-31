'From Cuis 4.2 of 25 July 2013 [latest update: #1916] on 31 December 2013 at 3:44:35.971522 pm'!

!SystemWindow methodsFor: 'menu' stamp: 'KenD 12/30/2013 20:44'!
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
			self morphBoundsInWorld: possibleBounds
		] 
		ifTrue: [ "Contract"
			possibleBounds := self valueOfProperty: #originalBounds.
			self removeProperty: #originalBounds.
			self morphBoundsInWorld: possibleBounds
		]! !

