'From Cuis 5.0 of 7 November 2016 [latest update: #3208] on 19 November 2017 at 12:34:29 am'!

!ChangeSorterWindow class methodsFor: 'instance creation' stamp: 'JO 11/18/2017 23:13:44'!
openChangeSorter
	self
		open: ChangeSorter new
		label: nil.! !


!TheWorldMenu methodsFor: 'construction' stamp: 'JO 11/18/2017 22:23:10'!
changesMenu
        "Build the changes menu for the world."

	| menu |
	menu _ self menu: 'Changes...'.
	self fillIn: menu from: {
		{ 'Change Sorter' . {ChangeSorterWindow. #openChangeSorter}.  'Open a 3-paned changed-set viewing tool'}.
		nil.

		{ 'Install New Updates' . { ChangeSet. #installNewUpdates }.
'Install in the current image the new updates available
in directory named ./CoreUpdates
or in directory named ../Cuis-Smalltalk-Dev/CoreUpdates'}.
		nil.

		{ 'Browse my Changes' . { Smalltalk . #browseMyChanges }.
				'Browse all of my changes since the last time #condenseSources was run.'}.
		{ 'Recently logged Changes...' . { ChangeList . #browseRecentLog}.'Open a change-list browser on the latter part of the changes log.  You can use this browser to recover logged changes which were not saved in your image, in the event of a crash or other interruption.'}.

		nil.
		{ 'Save World as morph file' . {self. #saveWorldInFile}. 'Save a file that, when reloaded, reconstitutes the current World.'}.
	}.
	^ menu! !

!methodRemoval: TheWorldMenu #openChangeSorter1!
TheWorldMenu removeSelector: #openChangeSorter1!
