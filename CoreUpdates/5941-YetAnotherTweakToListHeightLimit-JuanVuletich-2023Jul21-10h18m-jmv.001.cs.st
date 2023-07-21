'From Cuis 6.0 [latest update: #5940] on 21 July 2023 at 10:19:55 am'!

!PluggableListMorph methodsFor: 'layout' stamp: 'jmv 7/21/2023 10:18:06'!
maximumLayoutHeight
	| answer |
	limitLayoutHeight ifTrue: [
		answer := self desiredHeight.
		self hIsScrollbarShowing ifTrue: [
			answer := answer + ScrollBar scrollbarThickness].
		^answer ].
	^nil! !

