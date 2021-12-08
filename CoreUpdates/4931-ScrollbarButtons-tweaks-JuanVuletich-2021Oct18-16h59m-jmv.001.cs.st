'From Cuis 5.0 [latest update: #4930] on 18 October 2021 at 4:59:38 pm'!

!Preferences class methodsFor: 'bigger and smaller GUI element sizes' stamp: 'jmv 10/18/2021 11:46:02'!
scrollbarThickness
	"Includes border.
	No less than PluggableButtonMorph >> #minimumExtent."
	^Preferences windowTitleFont pointSize + 2! !


!MorphicCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 10/18/2021 11:54:41'!
frameRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol
	"
	(BitBltCanvas onForm: Display)	
		frameRectangle: (10@10 extent: 300@200)
		color: Color green
		borderWidth: 10
		borderStyleSymbol: #raised.
	Display forceToScreen.
	"

	| tlColor brColor |
	tlColor _ aColor.
	brColor _ aColor.
	aSymbol == #raised ifTrue: [
		tlColor _ aColor lighter.
		brColor _ aColor darker ].

	aSymbol == #inset ifTrue: [
		tlColor _ aColor darker.
		brColor _ aColor lighter ].

	self frameRectangle: aRectangle topLeftColor: tlColor bottomRightColor: brColor borderWidth: borderWidth! !

