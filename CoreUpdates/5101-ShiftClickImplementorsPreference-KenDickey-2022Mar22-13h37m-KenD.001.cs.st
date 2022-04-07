'From Cuis 6.0 [latest update: #5094] on 22 March 2022 at 2:19:16 pm'!

!Browser class methodsFor: 'class initialization' stamp: 'KenD 3/22/2022 14:14:02'!
initialize

	Preferences 
		addPreference: #shiftClickShowsImplementors 
		categories: {'keyboard shortcuts'} 
		default: false 
		balloonHelp: 'New CodePane shows implementors on shift+click'.

	! !


!CodeWindow methodsFor: 'GUI building' stamp: 'KenD 3/22/2022 13:37:53'!
buildMorphicCodePane
	"Construct the pane that shows the code.
	Respect the Preference for standardCodeFont."
	
	| codePane |
	
	codePane := self createCodePaneMorph.	
	(Preferences valueOfFlag: #shiftClickShowsImplementors)
		ifTrue: [ self addShiftClickEventHandlerFor: codePane ].
		
	^codePane! !

Browser initialize!

!Browser class reorganize!
('instance creation' new)
('cached state access' recentClasses releaseClassCachedState)
('class list' disableListClassesHierarchically enableListClassesHierarchically listClassesHierarchically)
('class initialization' initialize)
!

