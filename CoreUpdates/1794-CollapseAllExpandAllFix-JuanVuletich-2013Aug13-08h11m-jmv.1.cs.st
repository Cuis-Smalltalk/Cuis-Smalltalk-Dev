'From Cuis 4.2 of 25 July 2013 [latest update: #1793] on 13 August 2013 at 8:32:50.371173 am'!

!Taskbar methodsFor: 'testing' stamp: 'jmv 8/13/2013 08:14'!
is: aSymbol
	^ aSymbol == #Taskbar or: [ super is: aSymbol ]! !


!TaskbarTask methodsFor: 'commands' stamp: 'jmv 8/13/2013 08:22'!
restore: aMorph

	aMorph visible: true.
	
	Taskbar singleton submorphs do: [ :button |
		button model == self ifTrue: [ Taskbar singleton removeMorph: button ]
	]! !


!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 8/13/2013 08:19'!
expandAll
	"Expand all windows"
	Taskbar singleton restoreAll.
	(SystemWindow windowsIn: self satisfying: [:w | w isCollapsed])
		reverseDo: [:w | w collapseOrExpand.  self displayWorld]! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 8/13/2013 08:14'!
allNonFlapRelatedSubmorphs
	"Answer all non-window submorphs that are not flap-related"

	^submorphs 
		reject: [ :m | (m is: #SystemWindow) or: [ m is: #Taskbar ] ]! !


!Taskbar methodsFor: 'commands' stamp: 'jmv 8/13/2013 08:25'!
restore: aMorph
	
	self submorphs do: [ :button |
		button model morph == aMorph
			ifTrue: [ button model restore: aMorph ]]! !

