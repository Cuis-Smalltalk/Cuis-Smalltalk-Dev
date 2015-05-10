'From Cuis 4.2 of 25 July 2013 [latest update: #2289] on 1 May 2015 at 10:47:59.082699 pm'!

!BreakpointManager class methodsFor: 'cached state access' stamp: 'jmv 5/1/2015 22:46'!
releaseClassCachedState

	Installed ifNotNil: [
		Installed isEmpty ifTrue: [
			Installed _ nil ]]! !


!CodePackage class methodsFor: 'cached state access' stamp: 'jmv 5/1/2015 22:47'!
releaseClassCachedState

	InstalledPackages ifNotNil: [
		InstalledPackages isEmpty ifTrue: [
			InstalledPackages _ nil ]]! !


!BreakpointManager class reorganize!
('install-uninstall' installInClass:selector: unInstall:)
('private' breakpointMethodSourceFor:in: compilePrototype:in: installed)
('intialization-release' clear)
('testing' methodHasBreakpoint:)
('examples' testBreakpoint)
('cached state access' releaseClassCachedState)
!

