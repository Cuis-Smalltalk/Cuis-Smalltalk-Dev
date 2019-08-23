'From Cuis 5.0 [latest update: #3839] on 17 August 2019 at 12:35:37 pm'!

!ChangeSelectorKeepingParameters methodsFor: 'rename senders - private' stamp: 'HAW 8/17/2019 12:29:38'!
addRangesOf: oldSelectorKeywordsRanges to: rangesToKeywords

	oldSelectorKeywordsRanges do: [ :aMessageSendSelectorRanges |
		aMessageSendSelectorRanges withIndexDo: [ :aRange :index | rangesToKeywords add: aRange -> (newSelectorKeywords at: index) ]].
	
	
	! !


!ChangeSelectorKeepingParameters methodsFor: 'rename senders - private' stamp: 'HAW 8/17/2019 12:29:21'!
addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords

	| oldSelectorKeywordsRanges |

	oldSelectorKeywordsRanges := aMethodNode messageSendSelectorKeywordPositionsOf: oldSelector ifAbsent: [ #() ].
	self addRangesOf: oldSelectorKeywordsRanges to: rangesToKeywords.
	
	! !


!RenameSelector class reorganize!
('as yet unclassified')
!

