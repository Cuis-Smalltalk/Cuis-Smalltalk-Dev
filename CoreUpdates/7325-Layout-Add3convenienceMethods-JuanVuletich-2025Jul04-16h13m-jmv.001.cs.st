'From Cuis7.5 [latest update: #7324] on 4 July 2025 at 4:19:21 pm'!

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/4/2025 16:13:26'!
gap
	"Only for Linear Layouts"
	^layout isLinearLayout ifTrue: [
		layout gap ]! !

!LayoutMorph methodsFor: 'testing' stamp: 'jmv 7/4/2025 16:14:16'!
isColumn
	"Only makes sense for Linear Layouts"
	^layout isColumn! !

!LayoutMorph methodsFor: 'testing' stamp: 'jmv 7/4/2025 16:14:11'!
isRow
	"Only makes sense for Linear Layouts"
	^layout isRow! !

