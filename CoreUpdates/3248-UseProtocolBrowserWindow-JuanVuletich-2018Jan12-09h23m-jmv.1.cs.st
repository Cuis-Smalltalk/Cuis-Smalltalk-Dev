'From Cuis 5.0 of 7 November 2016 [latest update: #3247] on 12 January 2018 at 9:26:33 am'!

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 1/12/2018 09:24:56'!
browseProtocol
	"Create and schedule a new protocol browser on the currently selected class or meta."
	| aPBrowser label |
	model selectedClassOrMetaClass ifNotNil: [ :classOrMetaclass |
		aPBrowser _ ProtocolBrowser new onSubProtocolOf: classOrMetaclass.
		label _'Sub-protocol of: ', classOrMetaclass name.
		ProtocolBrowserWindow open: aPBrowser label: label ]! !


!DebuggerWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 1/12/2018 09:25:29'!
browseFullProtocolIn: anInspector
	"Spawn a window showing full protocol for the receiver's selection"

	ProtocolBrowserWindow
		openFullProtocolForClass: anInspector selectedClassOrMetaClass! !


!InspectorWindow methodsFor: 'menu commands' stamp: 'jmv 1/12/2018 09:23:23'!
browseFullProtocol
	"Spawn a window showing full protocol for the receiver's selection"

	ProtocolBrowserWindow openFullProtocolForClass: model selectedClassOrMetaClass! !


!ObjectExplorerWindow methodsFor: 'menu commands' stamp: 'jmv 1/12/2018 09:23:27'!
browseFullProtocol
	"Spawn a window showing full protocol for the receiver's selection"

	ProtocolBrowserWindow openFullProtocolForClass: model selectedClass! !

