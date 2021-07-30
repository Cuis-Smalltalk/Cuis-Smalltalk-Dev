'From Cuis 5.0 [latest update: #4714] on 30 July 2021 at 10:12:42 am'!
!classDefinition: #TaskbarMorph category: #'Tools-Taskbar'!
LayoutMorph subclass: #TaskbarMorph
	instanceVariableNames: 'viewBox scale clock '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Taskbar'!

!TaskbarMorph methodsFor: 'notifications' stamp: 'jmv 7/29/2021 18:42:06'!
fontPreferenceChanged
	clock font: nil! !


!TaskbarMorph methodsFor: 'initialization' stamp: 'jmv 7/29/2021 18:40:36'!
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
	viewBox separation: self defaultHeight // 8
! !

!classDefinition: #TaskbarMorph category: #'Tools-Taskbar'!
LayoutMorph subclass: #TaskbarMorph
	instanceVariableNames: 'viewBox scale clock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Taskbar'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
TaskbarMorph allInstancesDo: [ :tb | tb submorphsDo: [ :m | m class == UpdatingLabelMorph ifTrue: [ tb instVarNamed: 'clock' put: m ]]]!

