'From Cuis 4.0 of 21 April 2012 [latest update: #1267] on 28 April 2012 at 11:38:12 am'!

!MenuMorph methodsFor: 'accessing' stamp: 'jmv 4/28/2012 11:34'!
popUpOwner
	^popUpOwner! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 4/27/2012 16:37'!
makeMeFullyVisible 

	self world extent > (0@0) ifFalse: [^ self].

	(self position >= (0@0) and: [ self position < (self world extent-self extent)]) ifTrue: [
		^ self "OK -- visible"].

	"window not on screen (probably due to reframe) -- move it now"
	self isCollapsed
		ifTrue: [self position: (RealEstateAgent assignCollapsePointFor: self)]
		ifFalse: [self position: (RealEstateAgent initialFrameFor: self initialExtent: bounds extent world: self world) topLeft].

! !


!MenuItemMorph methodsFor: 'events' stamp: 'jmv 4/28/2012 11:38'!
mouseEnter: evt
	"The mouse entered the receiver"
	owner popUpOwner ifNotNil: [ :parentItem |
		parentItem removeAlarm: #deselectTimeOut: ].
	owner ifNil: [ ^self ].
	owner selectItem: self event: evt! !


!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 4/27/2012 16:38'!
bringWindowsFullOnscreen
	"Make ever SystemWindow on the desktop be totally on-screen, whenever possible."
	
	(SystemWindow windowsIn: self satisfying: [:w | true]) do: [ :each |
		each makeMeFullyVisible ]! !

!methodRemoval: MenuMorph #staysUp!
MenuMorph removeSelector: #staysUp!
