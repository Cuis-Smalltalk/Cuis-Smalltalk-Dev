'From Cuis6.3 [latest update: #6317] on 9 April 2024 at 10:08:45 am'!

!MethodSetWindow class methodsFor: 'opening' stamp: 'jmv 4/9/2024 10:07:47'!
openFullProtocolForClass: aClass
	"Create and schedule a browser for the entire protocol of the class."
	"
	ProtocolBrowserWindow openFullProtocolForClass: Number.
	"

	| aPBrowser label |

	aPBrowser := ProtocolBrowser new on: aClass.
	label := 'Entire protocol of: ', aClass name.

	^self open: aPBrowser label: label! !

!MethodSetWindow class methodsFor: 'opening' stamp: 'jmv 4/9/2024 10:07:42'!
openSubProtocolForClass: aClass
	"Create and schedule a browser for the entire protocol of the class."
	"
	ProtocolBrowserWindow openSubProtocolForClass: Number.
	"

	| aPBrowser label |

	aPBrowser := ProtocolBrowser new onSubProtocolOf: aClass.
	label := 'Sub-protocol of: ', aClass name.

	^self open: aPBrowser label: label! !

