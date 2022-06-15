'From Cuis 6.0 [latest update: #5267] on 12 June 2022 at 7:19:46 pm'!

!CharacterSequence methodsFor: 'displaying' stamp: 'jmv 6/12/2022 19:12:32'!
displayAt: aPoint 
	"Display the receiver as a DisplayText at aPoint on the display screen."

	self displayOn: Display at: aPoint.
	DisplayScreen screenUpdateRequired: nil! !

!CharacterSequence methodsFor: 'displaying' stamp: 'jmv 6/12/2022 19:12:44'!
displayOn: aDisplayMedium
	"Display the receiver on the given DisplayMedium.  5/16/96 sw"

	self displayOn: aDisplayMedium at: `0 @ 0`! !

!CharacterSequence methodsFor: 'displaying' stamp: 'jmv 6/12/2022 19:13:01'!
displayOn: aDisplayMedium at: aPoint 
	"Show a representation of the receiver as a DisplayText at location aPoint on aDisplayMedium, using black-colored text."

	^self displayOn: aDisplayMedium at: aPoint textColor: `Color black`
	"
	'Display' displayOn: Display at: 10@10. Display forceToScreen.
	"! !

!CharacterSequence methodsFor: 'displaying' stamp: 'jmv 6/12/2022 19:13:15'!
displayOn: aDisplayMedium at: aPoint textColor: aColor
	"Show a representation of the receiver as a DisplayText at location aPoint on aDisplayMedium, rendering the text in the designated color"

	^aDisplayMedium getCanvas drawString: self at: aPoint font: nil color: aColor! !

!CharacterSequence methodsFor: 'displaying' stamp: 'jmv 7/17/2017 15:41:46'!
displayProgressAt: aPoint from: minVal to: maxVal during: workBlock
	"Display this string as a caption over a progress bar while workBlock is evaluated.

EXAMPLE (Select next 6 lines and Do It)

'Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 100
	during: [:barBlock |
		1 to: 100 do: [:x | 
			(Delay forMilliseconds: 10) wait.
			barBlock value: x.]].

['Now here''s some Real Progress' asUtf8String
	displayProgressAt: Sensor mousePoint
	from: 0 to: 100
	during: [:barBlock |
	1 to: 100 do: [:x | barBlock value: x.
			(Delay forMilliseconds: 100) wait]].] fork
	
['Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 10
	during: [:barBlock |
	1 to: 30 do: [:x | barBlock value: x \\ 11.
			(Delay forMilliseconds: 100) wait]]] fork

'Now here''s some Real Progress' asUtf8String
	displayProgressAt: Sensor mousePoint
	from: 0 to: 10
	during: [:barBlock |
	1 to: 30 do: [:x | barBlock value: nil.
			(Delay forMilliseconds: 200) wait]].

HOW IT WORKS (Try this in any other language :-)
Since your code (the last 2 lines in the above example) is in a block,
this method gets control to display its heading before, and clean up 
the screen after, its execution.
The key, though, is that the block is supplied with an argument,
named 'bar' in the example, which will update the bar image every 
it is sent the message value: x, where x is in the from:to: range.

The use of ProgressInitiationException allows for avoiding actual
progress display, by catching the exception.
"
	^ProgressInitiationException 
		display: self
		at: aPoint 
		from: minVal 
		to: maxVal 
		during: workBlock! !

!methodRemoval: String #displayOn:at: stamp: 'jmv 6/12/2022 19:13:01'!
String removeSelector: #displayOn:at:!
!methodRemoval: String #displayOn:at:textColor: stamp: 'jmv 6/12/2022 19:13:15'!
String removeSelector: #displayOn:at:textColor:!
!methodRemoval: String #displayAt: stamp: 'jmv 6/12/2022 19:12:32'!
String removeSelector: #displayAt:!
!methodRemoval: String #displayOn: stamp: 'jmv 6/12/2022 19:12:44'!
String removeSelector: #displayOn:!
!methodRemoval: String #displayProgressAt:from:to:during: stamp: 'jmv 6/12/2022 19:18:12'!
String removeSelector: #displayProgressAt:from:to:during:!
