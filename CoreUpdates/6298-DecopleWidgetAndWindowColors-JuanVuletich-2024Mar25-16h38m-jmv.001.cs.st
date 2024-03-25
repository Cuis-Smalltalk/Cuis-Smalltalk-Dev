'From Cuis6.3 [latest update: #6297] on 25 March 2024 at 4:39:27 pm'!

!Theme methodsFor: 'widget colors' stamp: 'jmv 3/25/2024 16:38:45'!
widgetsColorFromToolColor
	"Answer false for subclasses that want to control color of widgets independently of the SystemWindow owning them"
	^true! !


!SystemWindow methodsFor: 'initialization' stamp: 'jmv 3/25/2024 16:38:59'!
openInWorld
	"Ensure all widgets have proper colors before opening"
	Theme current widgetsColorFromToolColor ifTrue: [
		self widgetsColor: self windowColor ].
	super openInWorld! !

