'From Cuis 5.0 [latest update: #4589] on 10 May 2021 at 12:24:06 pm'!

!BrowserCommentTextMorph methodsFor: 'displaying' stamp: 'jmv 5/10/2021 12:20:29'!
hidePane

	self hide.
	separator visible ifTrue: [
		proportionalHeight _ self layoutSpec proportionalLayoutHeight ].
	separator layoutSpec fixedHeight: 0.
	self layoutSpec proportionalHeight: 0.
	code layoutSpec proportionalHeight: 1.0.
	separator hide.! !

!BrowserCommentTextMorph methodsFor: 'displaying' stamp: 'jmv 5/10/2021 12:20:45'!
showPane

	self show.
	proportionalHeight ifNotNil: [
		self layoutSpec proportionalHeight: proportionalHeight.
		code layoutSpec proportionalHeight: 1.0 - proportionalHeight ].
	separator layoutSpec fixedHeight: Theme current layoutAdjusterThickness.
	separator show! !

