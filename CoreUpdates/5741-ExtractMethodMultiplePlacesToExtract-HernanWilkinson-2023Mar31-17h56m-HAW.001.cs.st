'From Cuis 6.0 [latest update: #5738] on 31 March 2023 at 6:56:43 pm'!

!ExtractMethodMessageSet methodsFor: 'message list' stamp: 'HAW 3/31/2023 18:55:03'!
doNotRefreshSelection
	
	selectedIndex := 0! !


!ExtractMethodMessageSet methodsFor: 'source code ranges' stamp: 'HAW 9/5/2021 21:46:48'!
messageSendsRangesOf: aSelector
	
	| replacement |
	
	replacement := finder replacementsAt: self messageListIndex ifAbsent: [ ^#()  ].
	
	^Array with: replacement intervalToExtract 
	! !


!ExtractMethodReplacementsWindow methodsFor: 'actions' stamp: 'HAW 3/31/2023 18:54:09'!
closeAfter: aBlock

	model doNotRefreshSelection.
	aBlock value.
	self whenUIinSafeState: [ self delete ].
	! !


!ExtractMethodMessageSet reorganize!
('initialization' initialize initializeFinder:)
('message list' messageListIndex messageListIndex: removeMessageFromBrowserKeepingLabel doNotRefreshSelection)
('source code ranges' messageSendsRangesOf:)
!


!ExtractMethodReplacementsWindow reorganize!
('actions' closeAfter: extractAllInClass extractInMethodOnly extractSelectionOnly refactor remove)
('GUI building' addButton:to:color: addButtonsTo:color: buildLowerPanes buttonsRow)
('button creation' createCancelButton createExtractAllInClassButton createExtractInMethodOnlyButton createExtractSelectionOnlyButton createRefactorButton createRemoveButton)
('initialization' initializeFrom:with:)
!

