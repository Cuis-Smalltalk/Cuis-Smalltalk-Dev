'From Cuis 6.0 [latest update: #5608] on 25 December 2022 at 1:18:01 pm'!

!VectorEngineWithPlugin class methodsFor: 'private' stamp: 'jmv 12/25/2022 13:17:10'!
protected: aBlock
	"VectorEnginePlugin is not reentrant.
	- Any proces that is not the main UI process must wait.
	- The main UI process will take the plugin without caring of some other process drawing breaks."
	
	(AccessLock notNil and: [ AccessLock ~= Processor activeProcess ]) ifTrue: [
		"Unless we are running the UI, wait"
		UISupervisor isUIProcessRunning ifFalse: [
			('VectorEnginePlugin is in use by: ', AccessLock printString) print.
			'Wait a bit.' print. 
			UISupervisor whenUIinSafeState: [ Processor activeProcess resume ].
			Processor activeProcess suspend ].
	].
	AccessLock _ Processor activeProcess.
	aBlock value.
	AccessLock _ nil.! !

