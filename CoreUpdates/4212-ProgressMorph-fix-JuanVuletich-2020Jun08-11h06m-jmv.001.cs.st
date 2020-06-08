'From Cuis 5.0 [latest update: #4208] on 8 June 2020 at 11:42:03 am'!

!ProgressMorph methodsFor: 'accessing' stamp: 'jmv 6/8/2020 11:41:25'!
label: aString subLabel: otherString
	| label subLabel n |
	label _ aString.
	subLabel _ otherString.
	layoutSpec notNil ifTrue: [
		n _ label size * self morphWidth * 0.95 // (AbstractFont default widthOfString: label).
		label _ label squeezedTo: n.
		n _ subLabel size * self morphWidth * 0.95  // (AbstractFont default widthOfString: subLabel).
		subLabel _ subLabel squeezedTo: n ].
	self whenUIinSafeState: [
		labelMorph contents: label.
		subLabelMorph contents: subLabel.
		self updatePositionAndExtent. ]! !

!ProgressMorph methodsFor: 'accessing' stamp: 'jmv 6/8/2020 11:26:00'!
updatePositionAndExtent
	| w newExtent |
	layoutSpec notNil ifTrue: [
		^self ].
	w _ ((labelMorph measureContents x max: subLabelMorph measureContents x) max: 200) + 18.
	w _ w min: Display extent x.
	newExtent _ w > extent x
		ifTrue: [ w@(labelMorph morphHeight + subLabelMorph morphHeight + progress morphHeight + 10) ]
		ifFalse: [ extent ].
	self world
		ifNotNil: [ :world |
			self morphPosition: world morphExtent - newExtent // 2 extent: newExtent ]
		ifNil: [ self morphExtent: newExtent ]! !

