'From Cuis 5.0 of 7 November 2016 [latest update: #3651] on 10 March 2019 at 2:42:17 pm'!

!ProgressMorph methodsFor: 'accessing' stamp: 'jmv 3/10/2019 14:41:35'!
updatePositionAndExtent
	| w newExtent |
	w _ ((labelMorph measureContents x max: subLabelMorph measureContents x) max: 200) + 8.
	newExtent _ w > extent x
		ifTrue: [ w+10@(labelMorph morphHeight + subLabelMorph morphHeight + progress morphHeight + 10) ]
		ifFalse: [ extent ].
	self world
		ifNotNil: [ :world | self morphPosition: world morphExtent - newExtent // 2 extent: newExtent ]
		ifNil: [ self morphExtent: newExtent ]! !

