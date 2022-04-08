'From Cuis 6.0 [latest update: #5101] on 8 April 2022 at 3:51:43 pm'!

!Preferences class methodsFor: 'standard queries' stamp: 'jmv 4/8/2022 15:50:57'!
shiftClickShowsImplementors
	^ self
		valueOfFlag: #shiftClickShowsImplementors
		ifAbsent: [ false ].! !


!CodeWindow methodsFor: 'GUI building' stamp: 'KenD 3/22/2022 13:37:53'!
buildMorphicCodePane
	"Construct the pane that shows the code.
	Respect the Preference for standardCodeFont."
	
	| codePane |
	
	codePane := self createCodePaneMorph.	
	(Preferences valueOfFlag: #shiftClickShowsImplementors)
		ifTrue: [ self addShiftClickEventHandlerFor: codePane ].
		
	^codePane! !

