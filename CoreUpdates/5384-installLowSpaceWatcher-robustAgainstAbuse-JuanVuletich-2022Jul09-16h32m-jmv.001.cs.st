'From Cuis 6.0 [latest update: #5382] on 9 July 2022 at 4:46:46 pm'!

!SystemDictionary methodsFor: 'memory space' stamp: 'jmv 7/9/2022 16:46:11'!
installLowSpaceWatcher
	"Start a process to watch for low-space conditions."
	"
	Smalltalk installLowSpaceWatcher
	"

	[
		self stopLowSpaceWatcher.
		LowSpaceProcess _ [self lowSpaceWatcher] newProcess.
		LowSpaceProcess priority: Processor lowIOPriority + 5.
		LowSpaceProcess name: '[system] Low Space Watcher'.
		LowSpaceProcess resume.
	] valueUnpreemptively
! !

