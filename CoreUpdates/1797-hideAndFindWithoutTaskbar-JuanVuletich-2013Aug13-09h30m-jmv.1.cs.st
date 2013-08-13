'From Cuis 4.2 of 25 July 2013 [latest update: #1796] on 13 August 2013 at 9:34:51.093006 am'!

!Morph methodsFor: 'testing' stamp: 'jmv 8/13/2013 09:32'!
isCollapsed

	^ Taskbar isCollapsed: self! !


!Morph methodsFor: 'menus' stamp: 'jmv 8/13/2013 09:31'!
collapse
	"If taskbar not visible, just hide."

	Taskbar minimize: self! !

!methodRemoval: SystemWindow #collapse!
SystemWindow removeSelector: #collapse!
!methodRemoval: SystemWindow #expand!
SystemWindow removeSelector: #expand!
!methodRemoval: SystemWindow #isCollapsed!
SystemWindow removeSelector: #isCollapsed!
