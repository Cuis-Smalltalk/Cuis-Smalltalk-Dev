'From Cuis 5.0 of 7 November 2016 [latest update: #3145] on 23 August 2017 at 2:35:44 pm'!

!TextModelMorph methodsFor: 'accessing' stamp: 'HAW 8/23/2017 14:22:17'!
enableEdition

	self textMorph enableEdition! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'HAW 8/23/2017 14:23:36'!
enableEdition
	
	self removeProperty: #disablesEdition! !


!TextModelMorph reorganize!
('accessing' disableEdition editor enableEdition textMorph wrapFlag:)
('dependents access' canDiscardEdits)
('drawing' drawOn: wantsFrameAdornments)
('editor access' scrollSelectionIntoView selectAll)
('events' doubleClick:localPosition: doubleClickAndHalf:localPosition: keyStroke: mouseEnter:)
('event handling' mouseButton2Activity)
('focus handling' focusText)
('geometry' innerHeight: scrollDeltaHeight)
('initialization' acceptOnCR: defaultColor innerMorphClass model:)
('interactive error protocol' correctFrom:to:with: nextTokenFrom:direction: notify:at:in: selectFrom:to: selectInvisiblyFrom:to: selectionInterval)
('model access' setSelection: setTextColor: text)
('notifications' possiblyChanged)
('scrolling' mightNeedHorizontalScrollBar)
('unaccepted edits' askBeforeDiscardingEdits: hasUnacceptedEdits:)
('updating' update:)
!

