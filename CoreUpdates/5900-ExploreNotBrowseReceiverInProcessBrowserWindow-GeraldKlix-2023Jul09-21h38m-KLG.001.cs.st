'From Haver 6.0 [latest update: #5898] on 9 July 2023 at 9:39:04 pm'!

!ProcessBrowserWindow methodsFor: 'menu commands' stamp: 'KLG 7/9/2023 21:36:12'!
exploreReceiver
	"Explore the receiver."

	model selectedContext
		ifNotNil: [ :context | context receiver explore ]! !

