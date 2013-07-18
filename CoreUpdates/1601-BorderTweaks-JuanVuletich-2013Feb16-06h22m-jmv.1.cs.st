'From Cuis 4.1 of 12 December 2012 [latest update: #1600] on 16 February 2013 at 6:28:21 am'!

!BorderedRectMorph commentStamp: '<historical>' prior: 0!
BorderedRectMorph introduce borders to RectangleLikeMorph. Borders have the instanceVariables borderWidth and borderColor.

BorderedRectMorph subclasses can use a variety of border styles: simple, inset, raised


BorderedRectMorph new borderColor: Color red; borderWidth: 10; openInWorld.
BorderedRectMorph new borderColor: Color white; openInWorld!


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 2/16/2013 05:59'!
createTextPaneExtent: answerExtent acceptBoolean: acceptBoolean
	"create the textPane"
	| result |

	self flag: #todo. "Integrate this method with the Theme system. --cbr"

	result _ TextModelMorph
				textProvider: self
				textGetter: #response
				textSetter: #response:
				selectionGetter: #selectionInterval
				allowStyler: true.
	result morphExtent: answerExtent.
	result hasUnacceptedEdits: true.
	result acceptOnCR: acceptBoolean.
	self addMorph: result.
	result morphPosition: 14@25.
	result morphExtent: extent-(28@62).
	^ result! !


!TaskbarTask methodsFor: 'commands' stamp: 'jmv 2/16/2013 06:05'!
minimize: aMorph

	| button label |

	morph _ aMorph.

	(morph respondsTo: #label) 
		ifTrue: [ label _ morph label ] 
		ifFalse: [ label _ morph class name ].

	self turnOn.

	button _ PluggableButtonMorph
			model: self
			stateGetter: nil
			action: #switch
			label: label.

	button
		layoutSpec: (LayoutSpec proportionalWidth: 1);
		color: Taskbar singleton color.

	Taskbar singleton addMorph: button.
			
	morph visible: false! !

!methodRemoval: ProgressMorph #defaultBorderColor!
ProgressMorph removeSelector: #defaultBorderColor!
!methodRemoval: ProgressMorph #defaultBorderWidth!
ProgressMorph removeSelector: #defaultBorderWidth!
!methodRemoval: ImageMorph #borderWidth:!
ImageMorph removeSelector: #borderWidth:!
