'From Cuis7.3 [latest update: #7119] on 6 May 2025 at 7:40:46 pm'!

!ProgressMorph methodsFor: 'initialization' stamp: 'HAW 5/6/2025 19:37:31'!
initialize
	super initialize.
	self separation: 0.
	labelMorph := LabelMorph contents: '' font: FontFamily defaultFamilyAndPointSize.
	subLabelMorph := LabelMorph contents: '' font: FontFamily defaultFamilyAndPointSize.
	progress := ProgressBarMorph new.
	progress morphExtent: 200 @ FontFamily defaultLineSpacing.
	self addMorphFront: labelMorph.
	self addMorphFront: subLabelMorph.
	self addMorph: progress fixedHeight: FontFamily defaultLineSpacing.
	self setProperty: #layerNumber toValue: 10! !


!ProgressiveTestRunner methodsFor: 'progress bar - private' stamp: 'HAW 5/6/2025 19:33:34'!
createProgressBar
		
	progressBar := ProgressMorph label: testSuite name.
	progressBar setProperty: #layerNumber toValue: 100.
	self calculateTestRunIncrement.
	self updateProgressBarSubLabel.
	! !

!methodRemoval: ProgressMorph #layerNumber stamp: 'HAW 5/6/2025 19:37:38'!
ProgressMorph removeSelector: #layerNumber!

!ProgressMorph reorganize!
('accessing' done done: incrDone: label: label:subLabel: subLabel: updatePositionAndExtent)
('initialization' defaultColor initialize openInWorld:)
!

