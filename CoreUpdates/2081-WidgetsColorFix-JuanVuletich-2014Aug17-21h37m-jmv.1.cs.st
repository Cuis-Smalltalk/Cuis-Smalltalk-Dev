'From Cuis 4.2 of 25 July 2013 [latest update: #2079] on 17 August 2014 at 9:39:13.423792 pm'!

!BorderedRectMorph methodsFor: 'initialization' stamp: 'jmv 8/17/2014 21:28'!
defaultBorderColor
	"answer the default border color/fill style for the receiver"
	^ Color gray! !


!PluggableScrollPane methodsFor: 'access' stamp: 'jmv 8/17/2014 21:21'!
adoptWidgetsColor: paneColor
	color = self defaultColor ifTrue: [
		color _ Theme current paneBackgroundFrom: paneColor.
	].
	super adoptWidgetsColor: paneColor.
	scrollBar adoptWidgetsColor: paneColor.
	hScrollBar adoptWidgetsColor: paneColor! !


!SystemWindow methodsFor: 'GUI building' stamp: 'jmv 8/17/2014 21:34'!
buttonColor

	^Theme current buttonColorFrom: self windowColor! !


!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 8/17/2014 21:38'!
defaultColor
	^Color gray! !

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 8/17/2014 21:23'!
adoptWidgetsColor: paneColor
	super adoptWidgetsColor: paneColor.
	
	color = self defaultColor ifTrue: [
		self color: (Theme current buttonColorFrom: paneColor) 
		]
	! !

