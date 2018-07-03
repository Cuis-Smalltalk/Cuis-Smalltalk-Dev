'From Cuis 5.0 of 7 November 2016 [latest update: #3348] on 30 June 2018 at 7:27:04 pm'!

!PopUpMenu class methodsFor: 'instance creation' stamp: 'HAW 6/30/2018 19:26:48'!
labels: aString icons: icons
	"Answer an instance of me whose items are in aString."

	^self labels: aString lines: nil icons: icons! !

!PopUpMenu class methodsFor: 'dialogs' stamp: 'HAW 6/30/2018 19:26:35'!
inform: aString
	"PopUpMenu inform: 'I like Cuis'"

	UISupervisor whenUIinSafeState: [ (self labels: ' OK ' icons: #(emblemImportantIcon)) startUpWithCaption: aString ]! !

