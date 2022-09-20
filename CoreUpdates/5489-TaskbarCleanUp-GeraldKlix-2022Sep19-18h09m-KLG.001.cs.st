'From Haver 6.0 [latest update: #5488] on 20 September 2022 at 11:46:50 am'!

!TaskbarMorph methodsFor: 'initialization' stamp: 'KLG 9/20/2022 11:36:07'!

initialize
	super initialize.
	viewBox _ LayoutMorph newRow color: self defaultColor.
	self
		addMorph: (clock _ UpdatingLabelMorph initializedInstance)
		layoutSpec:  (LayoutSpec morphWidthProportionalHeight: 0.5).
	self
		addMorph: viewBox 
		layoutSpec: (LayoutSpec
			proportionalWidth: 1.0
			proportionalHeight: 1.0 
			offAxisEdgeWeight: #rightOrBottom).
	viewBox separation: self defaultHeight // 8.
	self setBalloonText: [ 
		'About this system:' bold, `String newLineString`,
		Smalltalk systemInformationString ]
! !

!methodRemoval: TaskbarMorph #balloonText stamp: 'KLG 9/20/2022 11:36:26'!
TaskbarMorph removeSelector: #balloonText!

"Postscript:
Recreate the taskbar if necessary"

self runningWorld ifNotNil: [ :world |
	world taskbar ifNotNil: [
		world hideTaskbar.
		world showTaskbar ] ]!

