'From Cuis 4.2 of 25 July 2013 [latest update: #1891] on 21 December 2013 at 2:34:55.187711 pm'!

!Morph methodsFor: 'drawing' stamp: 'jmv 12/21/2013 14:34'!
fullDrawOn: aCanvas
	"Draw the full Morphic structure on the given Canvas"
	"Draw receiver itself"
	| h |
	h _ self world ifNotNil: [ :w | w activeHand ].
	(h notNil and: [
			h morphInHandOrResizing == self and: [ self world isUpdateCycleSlow ]])
		ifTrue: [
			(aCanvas isVisible: self morphBoundsInWorld) ifTrue: [
				owner == h
					ifTrue: [
						aCanvas
							reverseRectangleBorder: (0 @ 0 extent: self morphExtent)
							borderWidth: 2 ]
					ifFalse: [ self drawOn: aCanvas ]]]
		ifFalse: [
			(aCanvas isVisible: self morphBoundsInWorld) ifTrue: [ self drawOn: aCanvas ].
			self drawSubmorphsOn: aCanvas ]! !

