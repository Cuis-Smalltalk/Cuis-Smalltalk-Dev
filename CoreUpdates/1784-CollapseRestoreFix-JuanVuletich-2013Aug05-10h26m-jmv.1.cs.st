'From Cuis 4.2 of 25 July 2013 [latest update: #1783] on 5 August 2013 at 10:35:17.043043 am'!

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 8/5/2013 10:28'!
makeMeFullyVisible 

	self world extent > (0@0) ifFalse: [^ self].

	(self position >= (0@0) and: [ self position < (self world extent-self extent)]) ifTrue: [
		^ self "OK -- visible"].

	self position: (RealEstateAgent initialFrameFor: self initialExtent: self morphBoundsInWorld world: self world) topLeft! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'jmv 8/5/2013 10:34'!
collapse

	self isCollapsed
		ifFalse: [
			isCollapsed _ true.
			Taskbar visible ifTrue: [ Taskbar minimize: self ]
		]! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'jmv 8/5/2013 10:34'!
expand

	self isCollapsed
		ifTrue: [
			isCollapsed _ false.
			self activate.  "Bring to front first"
			Taskbar visible ifTrue: [ Taskbar restore: self ]
		]! !

