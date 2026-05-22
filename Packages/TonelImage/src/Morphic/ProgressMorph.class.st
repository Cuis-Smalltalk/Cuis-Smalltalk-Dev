Class {
	#name : 'ProgressMorph',
	#superclass : 'LinearLayoutMorph',
	#instVars : [
		'labelMorph',
		'subLabelMorph',
		'progress'
	],
	#category : 'Morphic-Composite Widgets',
	#package : 'Morphic',
	#tag : 'Composite Widgets'
}

{ #category : 'new-morph participation' }
ProgressMorph class >> categoryInNewMorphMenu [

	^ 'User Interface'
]

{ #category : 'example' }
ProgressMorph class >> example [
	"
	ProgressMorph example
	"
	| progress |
	progress := ProgressMorph label: 'Test progress' subLabel: 'this is the subheading'.
	progress openInWorld.
	[
		100 timesRepeat: [
			(Delay forMilliseconds: 20) wait.
			progress incrDone: 0.01 ].
		progress delete] fork
]

{ #category : 'new-morph participation' }
ProgressMorph class >> initializedInstance [
	| progress |
	progress := ProgressMorph label: 'Test progress' subLabel: 'this is the subheading'.
	[
		100 timesRepeat: [
			(Delay forMilliseconds: 30) wait.
			progress incrDone: 0.01 ].
		progress delete] fork.
	^progress
]

{ #category : 'instance creation' }
ProgressMorph class >> label: aString [
	^self label: aString subLabel: ''
]

{ #category : 'instance creation' }
ProgressMorph class >> label: aString subLabel: otherString [
	^self newColumn label: aString subLabel: otherString
]

{ #category : 'initialization' }
ProgressMorph >> defaultColor [
	^ `Color veryLightGray`
]

{ #category : 'accessing' }
ProgressMorph >> defaultLayerNumber [
	"Morphs with smaller layer number will always cover those with larger ones.
	We want to be rather on top."

	^10
]

{ #category : 'accessing' }
ProgressMorph >> done [
	^progress progressValue
]

{ #category : 'accessing' }
ProgressMorph >> done: amountDone [
	progress progressValue: ((amountDone min: 1.0) max: 0.0)
]

{ #category : 'accessing' }
ProgressMorph >> incrDone: incrDone [
	self done: self done + incrDone
]

{ #category : 'initialization' }
ProgressMorph >> initialize [
	super initialize.
	self separation: 0.
	labelMorph := LabelMorph contents: '' font: FontFamily defaultFamilyAndPointSize.
	subLabelMorph := LabelMorph contents: '' font: FontFamily defaultFamilyAndPointSize.
	progress := ProgressBarMorph new.
	progress morphExtent: 200 @ FontFamily defaultLineSpacing.
	self addMorphFront: labelMorph.
	self addMorphFront: subLabelMorph.
	self addMorph: progress fixedHeight: FontFamily defaultLineSpacing.
]

{ #category : 'accessing' }
ProgressMorph >> label: aString [
	self label: aString subLabel: ''
]

{ #category : 'accessing' }
ProgressMorph >> label: aString subLabel: otherString [
	| label subLabel n |
	label := aString.
	subLabel := otherString.
	layoutSpec notNil ifTrue: [
		label isEmpty ifFalse: [
			n := label size * extent x * 0.95 // (FontFamily defaultFamilyAndPointSize widthOfString: label).
			label := label squeezedTo: n ].
		subLabel isEmpty ifFalse: [
			n := subLabel size * extent x * 0.95  // (FontFamily defaultFamilyAndPointSize widthOfString: subLabel).
			subLabel := subLabel squeezedTo: n ] ].
	self whenUIinSafeState: [
		labelMorph contents: label.
		subLabelMorph contents: subLabel.
		self updatePositionAndExtent. ]
]

{ #category : 'initialization' }
ProgressMorph >> openInWorld: aWorld [
	"This msg and its callees result in the window being activeOnlyOnTop"
	aWorld addMorph: self.
	self updatePositionAndExtent.
	labelMorph fitContents.
	subLabelMorph fitContents.
	self layoutNeeded: true.
]

{ #category : 'accessing' }
ProgressMorph >> subLabel: aString [
	self whenUIinSafeState: [
		subLabelMorph contents: aString.
		self updatePositionAndExtent. ]
]

{ #category : 'accessing' }
ProgressMorph >> updatePositionAndExtent [
	| w newExtent |
	layoutSpec notNil ifTrue: [
		^self ].
	w := ((labelMorph measureContents x max: subLabelMorph measureContents x) max: 200) + 18.
	w := w min: Display extent x.
	newExtent := w > extent x
		ifTrue: [ w@(labelMorph morphHeight + subLabelMorph morphHeight + progress morphHeight + 10) ]
		ifFalse: [ extent ].
	self world
		ifNotNil: [ :world |
			self position: world morphExtent - newExtent // 2 extent: newExtent ]
		ifNil: [ self morphExtent: newExtent ]
]
