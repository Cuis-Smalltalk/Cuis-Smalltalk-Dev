'From Cuis 4.2 of 25 July 2013 [latest update: #2237] on 14 April 2015 at 8:57:40.576204 am'!

!RectangleLikeMorph methodsFor: 'drawing' stamp: 'jmv 4/14/2015 08:53'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	aCanvas
		fillRectangle: self morphLocalBounds
		color: self color! !


!Morph methodsFor: 'drawing' stamp: 'jmv 4/14/2015 08:53'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	aCanvas
		fillRectangle: self morphLocalBounds
		color: Color blue! !

!Morph methodsFor: 'e-toy support' stamp: 'jmv 4/14/2015 08:55'!
embeddedInMorphicWindowLabeled: labelString 
	| window |
	window := SystemWindow new.
	window setLabel: labelString.
	window layoutMorph addMorph: self proportionalHeight: 1.
	^window! !

!Morph methodsFor: 'initialization' stamp: 'jmv 4/14/2015 08:55'!
inATwoWayScrollPane
	"Answer a two-way scroll pane that allows the user to scroll the receiver in either direction.  It will have permanent scroll bars unless you take some special action."
	"
	(EllipseMorph new morphExtent: 500@270) inATwoWayScrollPane openInHand
	"

	| widget |
	self flag: #jmvVer2.
	widget _ PluggableScrollPane new.
	widget addToScroller: self.
	widget morphExtent: (self morphWidth min: 300 max: 100) @ (self morphHeight min: 150 max: 100).
	widget setScrollDeltas.
	^widget! !


!RectangleLikeMorph reorganize!
('accessing' color color:)
('initialization' defaultColor initialize)
('drawing' drawOn:)
('geometry' morphExtent morphExtent: morphExtentInWorld: morphHeight morphHeight: morphPosition:extent: morphWidth morphWidth: privateExtent:)
('geometry testing' isOrthoRectangularMorph)
!

