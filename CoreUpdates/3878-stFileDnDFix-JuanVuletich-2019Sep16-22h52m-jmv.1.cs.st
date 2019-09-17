'From Cuis 5.0 [latest update: #3877] on 16 September 2019 at 10:53:22 pm'!

!SimpleServiceEntry methodsFor: 'accessing' stamp: 'jmv 9/16/2019 22:52:17'!
iconSpec

	^icon! !


!DropFilesAction methodsFor: 'evaluating - private' stamp: 'jmv 9/16/2019 22:52:28'!
createMenuFor: options

	| icons lines labels |

	"options is a small collection, no problem to collect twice - Hernan"	
	labels := options collect: [ :option | option label ].
	icons := options collect: [ :option | option iconSpec ].

	shouldAskToStop 
		ifTrue: [
			lines := Array with: labels size.
			labels add: 'stop here'.
			icons add: #cancelIcon ]
		ifFalse: [ lines := #() ].
		
	^PopUpMenu labelArray: labels lines: lines icons: icons! !

