'From Cuis 5.0 of 7 November 2016 [latest update: #3649] on 8 March 2019 at 11:29:51 am'!

!DummyStream methodsFor: 'as yet unclassified' stamp: 'jmv 3/8/2019 10:17:33'!
padToEndIfCantTruncate
	"Only makes sense for file streams with existing content."! !


!Morph methodsFor: 'as yet unclassified' stamp: 'jmv 3/8/2019 11:24:08'!
whenUIinSafeState: evaluableObject
	self world
		ifNotNil: [ :w | w whenUIinSafeState: evaluableObject ]
		ifNil: evaluableObject! !


!ProgressMorph methodsFor: 'accessing' stamp: 'jmv 3/8/2019 11:29:08'!
updatePositionAndExtent
	| h w newExtent |
	w _ ((labelMorph measureContents x max: subLabelMorph measureContents x) max: 200) + 8.
	h _ labelMorph morphHeight + subLabelMorph morphHeight + progress morphHeight + 10.
	newExtent _ w@h.
	self world
		ifNotNil: [ :world | self morphPosition: world morphExtent - newExtent // 2 extent: newExtent ]
		ifNil: [ self morphExtent: newExtent ]! !


!ProgressMorph methodsFor: 'accessing' stamp: 'jmv 3/8/2019 11:27:03'!
label: aString subLabel: otherString
	self whenUIinSafeState: [
		labelMorph contents: aString.
		subLabelMorph contents: otherString.
		self updatePositionAndExtent. ]! !

!ProgressMorph methodsFor: 'accessing' stamp: 'jmv 3/8/2019 11:24:39'!
subLabel: aString
	self whenUIinSafeState: [
		subLabelMorph contents: aString.
		self updatePositionAndExtent. ]! !

!ProgressMorph methodsFor: 'initialization' stamp: 'jmv 3/8/2019 11:18:58'!
openInWorld: aWorld
	"This msg and its callees result in the window being activeOnlyOnTop"
	aWorld addMorph: self.
	self updatePositionAndExtent.
	labelMorph fitContents.
	subLabelMorph fitContents.
	layoutNeeded _ true.! !

