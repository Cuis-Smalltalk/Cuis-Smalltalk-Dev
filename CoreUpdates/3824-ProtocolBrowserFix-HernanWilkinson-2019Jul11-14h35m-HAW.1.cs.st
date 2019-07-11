'From Cuis 5.0 [latest update: #3823] on 11 July 2019 at 2:45:03 pm'!

!ProtocolBrowser methodsFor: 'accessing' stamp: 'HAW 7/11/2019 14:44:31'!
labelString
	"Answer the string for the window title"
	
	^ 'Protocol for: ', baseClass name, ' up to: ', (selectedName ifNil: [ ProtoObject name asString ])! !

