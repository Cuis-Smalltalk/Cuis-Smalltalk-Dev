'From Cuis7.1 [latest update: #6392] on 24 May 2024 at 1:39:27 pm'!

!TileResizeMorph class methodsFor: 'as yet unclassified' stamp: 'MM 5/24/2024 13:36:04'!
allowedArea
	^(RealEstateAgent maximumUsableAreaInWorld: self runningWorld) insetBy: Theme current fullScreenDeskMargin! !


!SystemWindow methodsFor: 'resize/collapse' stamp: 'MM 5/24/2024 13:32:21'!
resizeLeft
	self resize: (self allowedArea right: self allowedArea center x)! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'MM 5/24/2024 13:32:38'!
resizeRight
	self resize: (self allowedArea left: self allowedArea center x)! !


!TileResizeMorph class methodsFor: 'as yet unclassified' stamp: 'MM 5/24/2024 13:35:48'!
calculateTileRegions: aNumber
	
	^ self calculateTileRegionsIn: self allowedArea by: aNumber! !

