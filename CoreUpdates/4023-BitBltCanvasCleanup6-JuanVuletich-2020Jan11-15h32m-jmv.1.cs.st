'From Cuis 5.0 [latest update: #4020] on 11 January 2020 at 3:33:30 pm'!

!AbstractFont methodsFor: 'BitBltCanvas support' stamp: 'jmv 1/11/2020 15:32:19'!
onBitBltCanvasEngine: engine displayString: aString from: firstIndex to: lastIndex at: p color: color
	"Answer last affected pixel position
	Answer nil if nothing was done"

	self subclassResponsibility! !


!StrikeFont methodsFor: 'BitBltCanvas support' stamp: 'jmv 1/11/2020 15:32:02'!
onBitBltCanvasEngine: engine displayString: aString from: firstIndex to: lastIndex at: p color: color
	"Answer last affected pixel position.
	Answer nil if nothing was done."

	^ engine
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p
		strikeFont: self
		color: color! !

