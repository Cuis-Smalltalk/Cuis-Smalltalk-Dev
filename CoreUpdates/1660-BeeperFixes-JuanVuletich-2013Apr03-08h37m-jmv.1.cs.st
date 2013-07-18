'From Cuis 4.1 of 12 December 2012 [latest update: #1650] on 3 April 2013 at 8:40:49 am'!

!Preferences class methodsFor: 'personalization' stamp: 'jmv 4/3/2013 08:37'!
disableProgrammerFacilities
	"Warning: do not call this lightly!!  It disables all access to menus, debuggers, halos.  There is no guaranteed return from this, which is to say, you cannot necessarily reenable these things once they are disabled -- you can only use whatever the UI of the current project affords, and you cannot even snapshot -- you can only quit. 

     You can completely reverse the work of this method by calling the dual Preferences method enableProgrammerFacilities, provided you have left yourself leeway to bring about a call to that method.

	To set up a system that will come up in such a state, you have to request the snapshot in the same breath as you disable the programmer facilities.  To do this, put the following line into the 'do' menu and then evaluate it from that 'do' menu:

         Preferences disableProgrammerFacilities.

You will be prompted for a new image name under which to save the resulting image."

	Beeper beep.
	(self confirm: 'CAUTION!!!!
This is a drastic step!!
Do you really want to do this?')
		ifFalse: [
			Beeper beep.  ^ self inform: 'whew!!'].

	self disable: #cmdDotEnabled.       "No user-interrupt-into-debugger"

	"self enable: #noviceMode."           "No control-menu"
	self disable: #warnIfNoSourcesFile.
	self disable: #warnIfNoChangesFile.
	Smalltalk saveAs! !


!SystemDictionary methodsFor: 'memory space' stamp: 'jmv 4/3/2013 08:37'!
lowSpaceWatcher
	"Wait until the low space semaphore is signalled, then take appropriate actions."

	self garbageCollectMost <= self lowSpaceThreshold ifTrue: [
		self garbageCollect <= self lowSpaceThreshold ifTrue: [
			"free space must be above threshold before starting low space watcher"
			^ Beeper beep]].

	LowSpaceSemaphore _ Semaphore new.
	self primLowSpaceSemaphore: LowSpaceSemaphore.
	self primSignalAtBytesLeft: self lowSpaceThreshold.  "enable low space interrupts"

	LowSpaceSemaphore wait.  "wait for a low space condition..."

	self primSignalAtBytesLeft: 0.  "disable low space interrupts"
	self primLowSpaceSemaphore: nil.
	LowSpaceProcess _ nil.
	"Note: user now unprotected until the low space watcher is re-installed"

	ProjectX currentInterruptNameX: 'Space is low'! !


!Utilities class methodsFor: 'recent method submissions' stamp: 'jmv 4/3/2013 08:37'!
revertLastMethodSubmission
	| changeRecords lastSubmission theClass theSelector |
	"If the most recent method submission was a method change, revert
	that change, and if it was a submission of a brand-new method, 
	remove that method."

	RecentSubmissions isEmptyOrNil ifTrue: [^ Beeper beep].
	lastSubmission _ RecentSubmissions last.
	theClass _ lastSubmission actualClass ifNil: [^ Beeper beep].
	theSelector _ lastSubmission methodSymbol.
	changeRecords _ theClass changeRecordsAt: theSelector.
	changeRecords isEmptyOrNil ifTrue: [^ Beeper beep].
	changeRecords size = 1
		ifTrue: [
			"method has no prior version, so reverting in this case means removing"
			theClass removeSelector: theSelector]
		ifFalse: [
			changeRecords second fileIn].

"Utilities revertLastMethodSubmission"! !

!methodRemoval: PluggableListMorph #selectedMorph:!
PluggableListMorph removeSelector: #selectedMorph:!
