'From Cuis 5.0 [latest update: #4061] on 8 March 2020 at 4:29:11 pm'!

!LayoutMorph methodsFor: 'geometry' stamp: 'KenD 2/22/2020 16:55:11'!
refreshExtent
	"Flush cache & recalculate"
	self morphExtent: (self morphExtent max: self minimumExtent)! !


!Morph methodsFor: 'geometry' stamp: 'jmv 3/8/2020 16:27:44'!
minimumLayoutExtent
	"This returns the minimum extent that the morph may be shrunk to, when resizing LayoutMorphs or when adjusting a LayoutAdjustingMorph. 
	It is expressed in the morph own coordinates, like morphExtent."

	| minExtent |
	minExtent _ self minimumExtent.
	^ layoutSpec
		ifNil: [ minExtent ]
		ifNotNil: [ minExtent max: layoutSpec minimumLayoutWidth @ layoutSpec minimumLayoutHeight ]! !


!LayoutSpec methodsFor: 'layout' stamp: 'jmv 3/8/2020 16:28:31'!
fixedOrMinimumLayoutHeight
	^fixedHeight ifNil: [ morph morphHeight ]! !

!LayoutSpec methodsFor: 'layout' stamp: 'jmv 3/8/2020 16:28:20'!
fixedOrMinimumLayoutWidth
	^fixedWidth ifNil: [ morph morphWidth ]! !

