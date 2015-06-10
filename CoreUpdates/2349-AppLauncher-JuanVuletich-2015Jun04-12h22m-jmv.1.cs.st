'From Cuis 4.2 of 25 July 2013 [latest update: #2346] on 4 June 2015 at 2:00:02.948626 pm'!
!classDefinition: #AppLauncher category: #'System-Support'!
Object subclass: #AppLauncher
	instanceVariableNames: ''
	classVariableNames: 'AppGlobalName AppSelector'
	poolDictionaries: ''
	category: 'System-Support'!

!AppLauncher class methodsFor: 'accessing' stamp: 'jmv 6/4/2015 13:36'!
appGlobalName: appGlobalName appSelector: appSelector
	"For starting end user code (i.e. applications) on startup"

	AppGlobalName _ appGlobalName.
	AppSelector _ appSelector! !

!AppLauncher class methodsFor: 'system startup' stamp: 'jmv 6/4/2015 13:36'!
launchApp

	Smalltalk at: AppGlobalName ifPresent: [ :cls |
		cls perform: AppSelector withPossiblyWrongSizedArguments: Smalltalk startUpArguments ]! !


!SystemDictionary methodsFor: 'startup' stamp: 'jmv 6/4/2015 13:38'!
startup: resuming
	"
	resuming
		true: system is coming up (VM and image just started)
		false: we have just saved an image snapshot, but didn't quit.
	"

	"Here, startup begins!!"
	Cursor normal activateCursor.
	self setGCParameters.
	resuming ifTrue: [
		self clearExternalObjects ].
	self readCommandLineArguments.
	self processStartUpList: resuming.
	resuming ifTrue: [
		self setPlatformPreferences.
		self setStartupStamp.
		self processCommandLineArguments.
		AppLauncher launchApp ]! !


!AppLauncher class reorganize!
('accessing' appGlobalName:appSelector:)
('system startup' launchApp)
!

