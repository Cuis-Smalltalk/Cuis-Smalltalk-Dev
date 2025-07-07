'From Cuis7.5 [latest update: #7328] on 7 July 2025 at 10:19:06 am'!

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/7/2025 10:18:47'!
axisEdgeWeight
	"Only for Linear Layouts"
	^layout isLinearLayout ifTrue: [
		layout axisEdgeWeight ]! !

