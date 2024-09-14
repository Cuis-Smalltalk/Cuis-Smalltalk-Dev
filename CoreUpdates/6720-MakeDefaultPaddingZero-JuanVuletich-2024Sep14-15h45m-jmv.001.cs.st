'From Cuis7.1 [latest update: #6719] on 14 September 2024 at 3:51:01 pm'!

!BorderedBoxMorph methodsFor: 'initialization' stamp: 'jmv 9/14/2024 15:45:01'!
defaultPadding
	"Answer the default inner padding for the receiver.
	Redefine this method for a different initial padding in your morphs."

	^ `0@0`! !


!ImageMorph class methodsFor: 'instance creation' stamp: 'jmv 9/14/2024 15:47:29'!
new
	^super new
		image: self defaultForm.! !

!methodRemoval: TileResizeMorph #defaultPadding stamp: 'jmv 9/14/2024 15:45:16'!
TileResizeMorph removeSelector: #defaultPadding!
!methodRemoval: TextParagraphMorph #defaultPadding stamp: 'jmv 9/14/2024 15:45:15'!
TextParagraphMorph removeSelector: #defaultPadding!
!methodRemoval: PluggableMorph #defaultPadding stamp: 'jmv 9/14/2024 15:45:13'!
PluggableMorph removeSelector: #defaultPadding!
!methodRemoval: LabelMorph #defaultPadding stamp: 'jmv 9/14/2024 15:45:12'!
LabelMorph removeSelector: #defaultPadding!
!methodRemoval: ImageMorph #defaultPadding stamp: 'jmv 9/14/2024 15:45:10'!
ImageMorph removeSelector: #defaultPadding!
!methodRemoval: LayoutMorph #defaultPadding stamp: 'jmv 9/14/2024 15:45:09'!
LayoutMorph removeSelector: #defaultPadding!
!methodRemoval: ProgressBarMorph #defaultPadding stamp: 'jmv 9/14/2024 15:45:07'!
ProgressBarMorph removeSelector: #defaultPadding!

!ProgressBarMorph reorganize!
('accessing' progressValue progressValue:)
('drawing' drawOn:)
('initialization' defaultBorderWidth defaultColor initialize)
!

