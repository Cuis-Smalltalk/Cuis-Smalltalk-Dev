'From Cuis 4.0 of 3 April 2012 [latest update: #4211] on 12 April 2012 at 8:42:41 am'!
!classDefinition: #SystemWindow category: #'Morphic-Views for Models'!
PluggableMorph subclass: #SystemWindow
	instanceVariableNames: 'labelString collapsedFrame fullFrame isCollapsed updatablePanes widgetsColor layoutMorph adjusters '
	classVariableNames: 'TopWindow '
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 4/12/2012 08:36'!
makeMeVisible 

	self world morphExtent > (0@0) ifFalse: [^ self].

	(self morphPosition >= (0@0) and: [ self morphPosition < (self world morphExtent-self labelHeight)]) ifTrue: [
		^ self "OK -- at least my top left is visible"].

	"window not on screen (probably due to reframe) -- move it now"
	self morphPosition: (RealEstateAgent initialFrameFor: self initialExtent: extent world: self world) topLeft! !

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 4/12/2012 08:39'!
justDroppedInto: aMorph event: anEvent
	isCollapsed
		ifTrue: [
			self morphPosition: (self morphPosition max: 0@0) ]
		ifFalse: [
			TopWindow ~~ self ifTrue: [self activate]].
	^super justDroppedInto: aMorph event: anEvent! !

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 4/12/2012 08:39'!
morphExtent: aPoint 
	"Set the receiver's extent to value provided. Respect my minimumExtent."

	| newExtent |
	newExtent _ self isCollapsed
		ifTrue: [aPoint]
		ifFalse: [aPoint max: self minimumExtent].
	newExtent = extent ifTrue: [^ self].

	isCollapsed
		ifTrue: [super morphExtent: newExtent x @ (self labelHeight + 2)]
		ifFalse: [super morphExtent: newExtent]! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'jmv 4/12/2012 08:41'!
collapse
		
	self isCollapsed ifFalse: [

		"Collapse -- remove panes from morphics structure"

		isCollapsed _ true.
		layoutMorph ifNotNil: [ layoutMorph hide ].
		self morphExtent: 400@(self labelHeight + 2).
	
		Taskbar visible ifTrue: [ Taskbar minimize: self ]
	]! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'jmv 4/12/2012 08:33'!
expand
	self isCollapsed
		ifTrue: [
		
			"Expand -- restore panes to morphics structure"
	
			isCollapsed _ false.
			self activate.  "Bring to front first"

			self morphExtent: 400@300.

			layoutMorph ifNotNil: [ layoutMorph show ].
			
			Taskbar visible ifTrue: [ Taskbar restore: self. ]
		]! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'jmv 4/12/2012 08:38'!
expandBoxHit
	"The full screen expand box has been hit"

	isCollapsed ifTrue: [
		self hide.
		self collapseOrExpand.
		self fullScreen.
		^ self show].
	self fullScreen! !

!methodRemoval: SystemWindow #collapsedExtent!
SystemWindow removeSelector: #collapsedExtent!
!methodRemoval: SystemWindow #collapsedFrame!
SystemWindow removeSelector: #collapsedFrame!
!methodRemoval: SystemWindow #morphPosition:!
SystemWindow removeSelector: #morphPosition:!
!methodRemoval: SystemWindow #unexpandedFrame!
SystemWindow removeSelector: #unexpandedFrame!
!methodRemoval: SystemWindow #unexpandedFrame:!
SystemWindow removeSelector: #unexpandedFrame:!
!classDefinition: #SystemWindow category: #'Morphic-Views for Models'!
PluggableMorph subclass: #SystemWindow
	instanceVariableNames: 'labelString isCollapsed updatablePanes widgetsColor layoutMorph adjusters'
	classVariableNames: 'TopWindow'
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!
!methodRemoval: RealEstateAgent class #assignCollapseFrameFor:!
RealEstateAgent class removeSelector: #assignCollapseFrameFor:!
!methodRemoval: RealEstateAgent class #assignCollapsePointFor:!
RealEstateAgent class removeSelector: #assignCollapsePointFor:!
!methodRemoval: RealEstateAgent class #initialFrameFor:!
RealEstateAgent class removeSelector: #initialFrameFor:!
!methodRemoval: RealEstateAgent class #initialFrameFor:initialExtent:!
RealEstateAgent class removeSelector: #initialFrameFor:initialExtent:!
!methodRemoval: RealEstateAgent class #standardPositions!
RealEstateAgent class removeSelector: #standardPositions!
!methodRemoval: RealEstateAgent class #strictlyStaggeredInitialFrameFor:initialExtent:!
RealEstateAgent class removeSelector: #strictlyStaggeredInitialFrameFor:initialExtent:!
