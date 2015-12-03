'From Cuis 4.2 of 25 July 2013 [latest update: #2571] on 16 November 2015 at 10:39:15.608546 pm'!

!ChangeSet class methodsFor: 'services' stamp: 'jmv 11/16/2015 22:37'!
installNewUpdates
 	
	| updatesFileDirectory |

	updatesFileDirectory _ FmDirectoryEntry smalltalkImageDirectory / 'CoreUpdates'.
	updatesFileDirectory exists ifFalse: [
		updatesFileDirectory _ FmDirectoryEntry smalltalkImageDirectory parent / 			'Cuis-Smalltalk-Dev/CoreUpdates' ].

	updatesFileDirectory exists
		ifFalse: [ self inform: 'Could not find a CoreUpdates folder\No updates loaded' withNewLines ] 
		ifTrue: [ ChangeSet installNewUpdates: updatesFileDirectory ]! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 11/16/2015 22:38'!
changesMenu
        "Build the changes menu for the world."

	| menu |
	menu _ self menu: 'Changes...'.
	self fillIn: menu from: {
		{ 'Change Sorter' . {self. #openChangeSorter1}.  'Open a 3-paned changed-set viewing tool'}.
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

!methodRemoval: FmDirectoryEntry class #parent!
FmDirectoryEntry class removeSelector: #parent!
