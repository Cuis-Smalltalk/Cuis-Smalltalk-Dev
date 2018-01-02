'From Cuis 5.0 of 7 November 2016 [latest update: #3242] on 28 December 2017 at 3:56:57 pm'!

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 12/28/2017 15:54:48'!
ui
	^UIProcess ifNotNil: [
		UIProcess isSuspended ifFalse: [
			UIProcess animatedUI ]]! !

!UISupervisor class methodsFor: 'services' stamp: 'jmv 12/28/2017 15:56:12'!
restoreDisplay
	DisplayScreen startUp.
	self ui ifNotNil: [ :guiRootObject |
		guiRootObject restoreDisplay ].
	self whenUIinSafeState: [ Cursor normal activateCursor ]! !

