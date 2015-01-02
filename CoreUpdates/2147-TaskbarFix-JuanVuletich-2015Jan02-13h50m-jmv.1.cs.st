'From Cuis 4.2 of 25 July 2013 [latest update: #2135] on 2 January 2015 at 1:53:25.527214 pm'!

!Taskbar methodsFor: 'initialization' stamp: 'jmv 1/2/2015 13:52'!
initialize
	super initialize.
	viewBox _ LayoutMorph newRow color: self defaultColor.
	self
		addMorph: UpdatingStringMorph initializedInstance 
		layoutSpec:  (LayoutSpec morphWidthProportionalHeight: 1.0).
	self
		addMorph: viewBox 
		layoutSpec: (LayoutSpec
			proportionalWidth: 1.0
			proportionalHeight: 1.0 
			minorDirectionPadding: #right).
	viewBox separation: 5
! !

Preferences cuisDefaults!