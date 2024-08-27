'From Cuis7.1 [latest update: #6643] on 27 August 2024 at 4:34:13 pm'!

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 8/27/2024 16:33:11'!
rescaleButtons
	"boxExtent changed. Update my buttons."

	| buttonExtent buttonPos spacing |
	buttonExtent := Theme current titleBarButtonsExtent.
	buttonPos := (self labelHeight + borderWidth - Theme current titleBarButtonsExtent / 2) ceiling asPoint.
	spacing := Theme current titleBarButtonsExtent x *13//10.
	self submorphsReverseDo: [ :aMorph |
		(aMorph is: #PluggableButtonMorph) 
		  ifTrue: [ 
				aMorph morphExtent: buttonExtent. 
				aMorph morphPosition: buttonPos.
				buttonPos := buttonPos + (spacing@0).
		].
	]! !

