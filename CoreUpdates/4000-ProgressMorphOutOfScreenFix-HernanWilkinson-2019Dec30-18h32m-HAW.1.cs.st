'From Cuis 5.0 [latest update: #3997] on 31 December 2019 at 3:01:26 pm'!

!ProgressMorph methodsFor: 'accessing' stamp: 'HAW 12/31/2019 15:00:11'!
updatePositionAndExtent
	| w newExtent |
	w _ ((labelMorph measureContents x max: subLabelMorph measureContents x) max: 200) + 18.
	w _ w min: Display extent x.
	newExtent _ w > extent x
		ifTrue: [ w@(labelMorph morphHeight + subLabelMorph morphHeight + progress morphHeight + 10) ]
		ifFalse: [ extent ].
	self world
		ifNotNil: [ :world | self morphPosition: world morphExtent - newExtent // 2 extent: newExtent ]
		ifNil: [ self morphExtent: newExtent ]! !

